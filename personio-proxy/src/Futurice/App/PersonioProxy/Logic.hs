{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.PersonioProxy.Logic where

import Codec.Picture                    (DynamicImage, decodePng)
import Control.Concurrent.STM           (readTVarIO)
import Control.Lens                     ((.=))
import Control.Monad.State.Strict       (evalState, state)
import Data.Aeson.Types                 (parseEither, parseJSON)
import Data.Distributive                (Distributive (..))
import Data.Functor.Rep
       (Representable (..), apRep, distributeRep, pureRep)
import Data.List                        (foldl', partition)
import Data.Time                        (addDays, fromGregorian, toGregorian)
import FUM.Types.Login                  (Login)
import Futurice.App.PersonioProxy.Types
import Futurice.Cache                   (cachedIO)
import Futurice.CareerLevel
import Futurice.Daily
import Futurice.Email                   (emailFromText)
import Futurice.Monoid                  (Average (..), mkAverage)
import Futurice.Office                  (officeToText)
import Futurice.Prelude
import Futurice.Time.Month              (dayToMonth, monthInterval)
import Futurice.Tribe
import Prelude ()
import Servant
import Servant.Cached                   (Cached, mkCached)
import Servant.Chart                    (Chart (..), SVG)

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Futurice.Chart.Stacked        as C
import qualified Futurice.Colour               as FC
import qualified Futurice.IdMap                as IdMap
import qualified Futurice.Postgres             as Postgres
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Network.HTTP.Client           as HTTP
import qualified Numeric.Interval.NonEmpty     as NE
import qualified Personio                      as P
import qualified Xeno.DOM                      as Xeno

personioRequest :: Ctx -> P.SomePersonioReq -> Handler P.SomePersonioRes
personioRequest ctx (P.SomePersonioReq res) = case res of
    P.PersonioEmployees       -> P.SomePersonioRes res <$> rawEmployees ctx
    P.PersonioValidations     -> P.SomePersonioRes res <$> rawValidations ctx
    P.PersonioSimpleEmployees -> P.SomePersonioRes res <$> rawSimpleEmployees ctx
    P.PersonioAll             -> P.SomePersonioRes res <$> rawAllData ctx

rawValidations :: Ctx -> Handler [P.EmployeeValidation]
rawValidations ctx = P.paValidations <$> liftIO (readTVarIO $ ctxPersonioData ctx)

rawEmployees :: Ctx -> Handler [P.Employee]
rawEmployees ctx = do
    es <- P.paEmployees <$> (liftIO $ readTVarIO $ ctxPersonioData ctx)
    -- no filtering, all employees
    pure $ toList es

rawAllData :: Ctx -> Handler P.PersonioAllData
rawAllData ctx = liftIO $ readTVarIO $ ctxPersonioData ctx

scheduleEmployees :: Ctx -> Handler [P.ScheduleEmployee]
scheduleEmployees ctx = do
    es <- P.paEmployees <$> (liftIO $ readTVarIO $ ctxPersonioData ctx)
    -- no filtering, all employees
    pure $ P.fromPersonio $ toList es

inventoryEmployees :: Ctx -> Handler [P.InventoryEmployee]
inventoryEmployees ctx = do
    es <- P.paEmployees <$> (liftIO $ readTVarIO $ ctxPersonioData ctx)
    pure $ P.inventoryEmployeeFromPersonio $ toList es

employeeUsername :: Ctx -> Text -> Handler Login
employeeUsername ctx name = do
    es <- P.paEmployees <$> (liftIO $ readTVarIO $ ctxPersonioData ctx)
    let emap = Map.fromList $ catMaybes $ (\e -> (,) <$> (e ^. P.employeeEmail) <*> (e ^. P.employeeLogin)) <$> es
    case emailFromText (name <> "@futurice.com") >>= \e -> emap ^.at e of
      Just login -> pure login
      Nothing -> throwError $ err404 { errBody = "No login found" }

getSimpleEmployees :: Ctx -> LogT IO (Map Day [P.SimpleEmployee])
getSimpleEmployees ctx = do
    res <- Postgres.safePoolQuery_ ctx "SELECT DISTINCT ON (timestamp :: date) timestamp, contents FROM \"personio-proxy\".log;"
    let ses' :: Map Day [P.SimpleEmployee]
        ses' = P.internSimpleEmployees (traverse . traverse) $ Map.fromList
            [ (d, ids)
            | (t, v) <- res
            , let d = utctDay t
            , let ids = case parseEither parseJSON v of
                    Left _   -> mempty
                    Right es -> (es :: [P.Employee]) ^.. folded . P.simpleEmployee
            ]
    pure ses'

--needed so rarely that we can just fetch stuff on need basis
rawSimpleEmployees :: Ctx -> Handler (Map Day [P.SimpleEmployee])
rawSimpleEmployees ctx = liftIO $ runLogT "personio-proxy" (ctxLogger ctx) $ getSimpleEmployees ctx

employeePicture :: Ctx -> P.EmployeeId -> Handler DynamicImage
employeePicture ctx eid = do
    picture <- liftIO $ P.evalPersonioQueryIO mgr lgr cfg $ P.QueryEmployeePicture eid
    case decodePng picture of
      Right pic -> pure pic
      Left _ -> throwError err404 { errBody = "Picture couldn't be decoded."}
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx

-------------------------------------------------------------------------------
-- Cached
-------------------------------------------------------------------------------

mkCached' :: (Typeable ct, Typeable a, MimeRender ct a) => Ctx -> IO a -> Handler (Cached ct a)
mkCached' ctx action = liftIO $
    cachedIO (ctxLogger ctx) (ctxCache ctx) 600 () $ mkCached <$> action

-------------------------------------------------------------------------------
-- Employees Chart
-------------------------------------------------------------------------------

employeesChart :: Ctx -> Handler (Cached SVG (Chart "employees"))
employeesChart ctx = mkCached' ctx $ runLogT "chart-employees" (ctxLogger ctx) $ do
    today <- currentDay
    let ses = getSimpleEmployees ctx

    values <- do
        fes <- liftIO $ readTVarIO $ ctxPersonioData ctx
        let fes' = IdMap.fromFoldable $ P.paEmployees fes
        res <- fmap (dailyFromMap' []) ses
        return
            [ pair d $ employeesCounts fes' d (res ! d)
            | d <- [ $(mkDay "2018-06-01") .. today ]
            ]

    return $ Chart . C.toRenderable $ do
        C.layout_title .= "Employees"

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (PerEmploymentType "Internal" "External")
            values

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Employees"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    pair :: a -> b -> (a, b)
    pair = (,)

    employeesCounts :: IdMap.IdMap P.Employee -> Day -> [P.SimpleEmployee] -> PerEmploymentType Int
    employeesCounts fes today ses' = PerEmploymentType (length int) (length ext)
      where
        ses = filter (P.employeeIsActive today) ses'
        (int, ext) = partition
            (\p -> fes ^? ix (p ^. P.employeeId) . P.employeeEmploymentType . _Just /= Just P.External)
            ses

    overrideY :: C.AxisData Int -> C.AxisData Int
    overrideY ax = ax & C.axis_grid .~ (ax ^.. C.axis_ticks . folded . _1)

data PerEmploymentType a = PerEmploymentType a a
  deriving (Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (PerEmploymentType a)

instance Distributive PerEmploymentType where
    distribute = distributeRep

instance Applicative PerEmploymentType where
    pure  = pureRep
    (<*>) = apRep

instance Representable PerEmploymentType where
    type Rep PerEmploymentType = P.EmploymentType

    tabulate f = PerEmploymentType (f P.Internal) (f P.External)
    index (PerEmploymentType x _) P.Internal = x
    index (PerEmploymentType _ x) P.External = x

-------------------------------------------------------------------------------
-- Tribes employees chart
-------------------------------------------------------------------------------

tribeEmployeesChart :: Ctx -> Handler (Cached SVG (Chart "tribe-employees"))
tribeEmployeesChart ctx = mkCached' ctx $ runLogT "chart-tribe-employees" (ctxLogger ctx) $ do
    today <- currentDay
    let ses = getSimpleEmployees ctx

    (values, future) <- do
        fes <- liftIO $ readTVarIO $ ctxPersonioData ctx
        let fes' = IdMap.fromFoldable $ P.paEmployees fes
        res <- fmap (dailyFromMap' []) ses
        return $ pair
            [ pair d $ employeesCounts True fes' d (res ! d)
            | d <- [ $(mkDay "2018-06-01") .. today ]
            ]
            [ pair d $ employeesCounts False fes' d (res ! d)
            | d <- [ today .. addDays 50 today ]
            ]

    return $ Chart . C.toRenderable $ do
        C.layout_title .= "Employees per tribe"

        C.setColors $ cycle
            [ C.opaque $ FC.colourToDataColour $ FC.FutuAccent f a
            | f <- [ minBound .. maxBound ]
            , a <- [ FC.AC2, FC.AC3 ]
            ]

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (tabulate $ view unpacked . tribeToText)
            values

        C.setColors $ cycle
            [ flip C.withOpacity 0.5 $ FC.colourToDataColour $ FC.FutuAccent f a
            | f <- [ minBound .. maxBound ]
            , a <- [ FC.AC2, FC.AC3 ]
            ]

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (tabulate $ view unpacked . tribeToText)
            future

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Employees"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    employeesCounts :: Bool -> IdMap.IdMap P.Employee -> Day -> [P.SimpleEmployee] -> PerTribe Int
    employeesCounts status fes today es' = foldl' go (pureRep 0) es
      where
        es :: [P.SimpleEmployee]
        es = flip filter es' $ \e ->
            (if status then P.employeeIsActive else P.employeeIsActive') today e
            && fes ^? ix (e ^. P.employeeId) . P.employeeEmploymentType . _Just == Just P.Internal

        go :: PerTribe Int -> P.SimpleEmployee -> PerTribe Int
        go (Per m) e = Per $ m
            & at (e ^. P.employeeTribe) %~ Just . maybe 1 succ

    overrideY :: C.AxisData Int -> C.AxisData Int
    overrideY ax = ax & C.axis_grid .~ (ax ^.. C.axis_ticks . folded . _1)

    pair :: a -> b -> (a, b)
    pair = (,)

-------------------------------------------------------------------------------
-- Career levels chart
-------------------------------------------------------------------------------

careerLevelsChart :: Ctx -> Handler (Cached SVG (Chart "career-levels"))
careerLevelsChart ctx = mkCached' ctx $ runLogT "career-levels" (ctxLogger ctx) $ do
    (values, perRole) <- liftIO $ do
        xs <- liftIO $ readTVarIO $ ctxPersonioData ctx
        return (P.paCareerLevels xs, P.paCareerLevelsRole xs)

    let roles = Map.keys perRole

    let transp :: Map CareerLevel [Int]
        transp = Map.fromList
            [ (cl, xs)
            | cl <- [ minBound .. maxBound ]
            , let xs = fromMaybe 0 (values ^? ix cl)
                     : [ fromMaybe 0 $ perRole ^? ix r . ix cl | r <- roles ]
            ]

    let titles = "All" : fmap (view unpacked) roles
    let points :: [(CareerLevel, [Int])]
        points = Map.toList transp

    return $ Chart . C.toRenderable $ do
        C.layoutlr_title .= "Career level distribution"
        C.layoutlr_x_axis . C.laxis_title .= "Career level"
        C.layoutlr_left_axis . C.laxis_title .= "Employees"
        C.layoutlr_right_axis . C.laxis_title .= "Cumulative"

        colors
        C.plotLeft $ fmap (C.plotBars . style) $ C.bars titles points

        colors
        C.plotRight $ fmap C.toPlot $ C.line "All" [Map.toList $ sumScan values]
        ifor_ perRole $ \role xs ->
            C.plotRight $ fmap C.toPlot $ C.line (role ^. unpacked) [Map.toList $ sumScan xs]

  where
    colors =
        C.setColors $ cycle
            [ C.opaque $ FC.colourToDataColour c
            | c <-
                [ FC.FutuGreen
                , FC.FutuAccent FC.AF4 FC.AC3
                , FC.FutuAccent FC.AF1 FC.AC3
                , FC.FutuAccent FC.AF2 FC.AC3
                , FC.FutuAccent FC.AF3 FC.AC3
                ]
            ]

    style b = b
        & C.plot_bars_spacing .~ C.BarsFixWidth 5

    sumScan :: Traversable t => t Int -> t Int
    sumScan t = evalState (traverse (\x -> state $ \s -> let xs = s + x in xs `seq` (xs, xs)) t) 0

-------------------------------------------------------------------------------
-- Roles distribution
-------------------------------------------------------------------------------

rolesDistributionChart :: Ctx -> Handler (Cached SVG (Chart "roles-distribution"))
rolesDistributionChart ctx = mkCached' ctx $ runLogT "chart-roles-distribution" (ctxLogger ctx) $ do
    values' <- do
        res <- Postgres.safePoolQuery_ ctx "SELECT DISTINCT ON (timestamp :: date) timestamp, contents FROM \"personio-proxy\".log;"
        return $ res <&> \(t, c) ->
            let d = utctDay t
            in (d, rolesDistribution d c)

    -- titles are also positions!
    let (titles, values) = postprocess values'

    return $ Chart . C.toRenderable $ do
        C.setColors $ cycle
            [ C.opaque $ FC.colourToDataColour $ FC.FutuAccent f a
            | f <- [ minBound .. maxBound ]
            , a <- [ FC.AC2, FC.AC3 ]
            ]

        C.layout_title .= "Relative role/competence distribution over time"

        C.plot $ fmap (C.stackedToPlot' titles (flip indexTMap)) $ C.stacked
            (fmap (view unpacked) titles)
            values

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Percent"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    rolesDistribution :: Day -> Value -> TMap Text Int
    rolesDistribution today v = case parseEither parseJSON v of
        Left _    -> pure 0
        Right es' -> foldl' go (pure 0) es
          where
            es :: [P.Employee]
            es = filter (\e -> P.employeeIsActive today e && e ^. P.employeeEmploymentType == Just P.Internal) es'

            go :: TMap Text Int -> P.Employee -> TMap Text Int
            go (TMap d m) e = TMap d $ m
                & at (e ^. P.employeeRole) %~ Just . maybe 1 succ

    overrideY :: C.AxisData Double -> C.AxisData Double
    overrideY ax = ax
        & C.axis_grid   %~ filter (<= 100)
        & C.axis_ticks  %~ filter ((<= 100) . fst)
        & C.axis_labels %~ map (filter ((<= 100) . fst))

    postprocess :: [(Day, TMap Text Int)] -> (TMap Text Text, [(Day, TMap Text Double)])
    postprocess xs = (titles, values)
      where
        keys = mconcat [ Map.keysSet m | (_, TMap _ m) <- xs ]
        keys' = Map.fromSet (const 0) keys

        titles = TMap "?" $ Map.fromSet id keys
        values =
            [ (d, TMap 0 $ fmap (\x -> 100 * fromIntegral x / s) m')
            | (d, TMap _  m) <- xs
            , let m' = Map.union m keys'
            , let s = fromIntegral (sum m')
            ]

-------------------------------------------------------------------------------
-- Attrition rate
-------------------------------------------------------------------------------

leavers :: Ctx -> Day -> Day -> Handler Int
leavers ctx start end = do
    employees <- rawEmployees ctx
    pure $ length
        $ filter (\e -> e ^. P.employeeTerminationType == Just "employee-quit")
        $ filter (\e -> e ^. P.employeeEndDate <= Just end && e ^. P.employeeEndDate >= Just start)
        $ filter (\e -> e ^. P.employeeContractType /= Just P.FixedTerm)
        $ filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal) employees

-- Attrition rate = selected leavers in period / Average HC in period X (12 / M in period) x 100%
attritionRate :: Ctx -> Maybe Day -> Maybe Day -> Handler AttritionRate
attritionRate ctx startDay endDay = do
    now <- currentDay
    let (startDay', endDay') =
            case (startDay, endDay) of
              (Nothing, Nothing) -> currentQuarter now
              (Just day, Nothing) -> (day, addDays 90 day)
              (Nothing, Just day) -> (addDays (-90) day, day)
              (Just day1, Just day2) -> (day1, day2)
    leavers' <- leavers ctx startDay' endDay'
    activeList <- liftIO $ runLogT "personio-proxy" (ctxLogger ctx) $ averageHCFunc startDay' endDay'
    pure $ AttritionRate
        { _attrAttritionRate = ((fromIntegral leavers') / (averageHC activeList) * (12.0 / 3) * 100.0)
        , _attrLeavers = leavers'
        , _attrMonths = (Map.fromList activeList)
        }
  where
    averageHC :: [(Month, Int)] -> Double
    averageHC x = (fromIntegral $ sum $ fmap snd x) / (fromIntegral $ length x)
    averageHCFunc :: Day -> Day -> LogT IO [(Month, Int)]
    averageHCFunc start end = do
        let days = [start .. end]
        let months' = Map.fromListWith (<>) $ (\d -> (dayToMonth d, [d])) <$> days
        let monthList = Map.toList months' <> [((dayToMonth $ last days, [last days]))]
        res <- for monthList $ \(m,days') -> do
            --TODO: remove partial
            (m,) <$> Postgres.safePoolQuery ctx "SELECT contents FROM \"personio-proxy\".log WHERE timestamp < ? ORDER BY timestamp DESC LIMIT 1;" (Only $ addDays 1 $ last $ sort days')
        let emp' :: [(Month, [P.Employee])] =
                [ (m',ids)
                | (m', days') <- res
                , v <- concat days'
                , let ids = case parseEither parseJSON v of
                        Left _   -> mempty
                        Right es -> (es :: [P.Employee])
                , length ids > 0
                ]
        let emps' :: [(Month, Int)]
            emps' = map (\(m, employees) -> (m, length $ filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal)
                                                $ filter (\e -> e ^. P.employeeStatus == P.Active
                                                           || (e ^. P.employeeStatus == P.Onboarding && maybe False (withinMonth m) (e ^. P.employeeHireDate))) employees)) emp'
        pure $ emps'
    withinMonth month day = day `NE.member` monthInterval month

    currentQuarter d =
        let (year, month, _) = toGregorian d
        in if | month <= 3 -> ((fromGregorian year 1 1),(fromGregorian year 3 31))
              | month <= 6 -> ((fromGregorian year 4 1),(fromGregorian year 6 30))
              | month <= 9 -> ((fromGregorian year 7 1),(fromGregorian year 9 30))
              | otherwise -> ((fromGregorian year 10 1),(fromGregorian year 12 31))

averageTargetMonthlyCompensation :: Ctx -> Handler MonthlyCompensation
averageTargetMonthlyCompensation ctx = do
    employees <- rawEmployees ctx
    currencyMap <- liftIO $ fetchCurrencyRates ctx
    let employees' = filter (\e -> e ^. P.employeeStatus == P.Active || e ^. P.employeeStatus == P.Leave) $
                     filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal) $
                     filter (\e -> e ^. P.employeeContractType == Just P.PermanentAllIn || e ^. P.employeeContractType == Just P.Permanent) employees
    let tmc :: P.Employee -> Maybe Double
        tmc e = e ^. P.employeeSalaryCurrency <&>
              (\c ->
                  let withDefault' = withDefault currencyMap c
                  in (withDefault' $ e ^. P.employeeHourlySalary) * 158.0
                     + (withDefault' $ e ^. P.employeeMonthlyFixedSalary)
                     - (withDefault' $ e ^. P.employeeMonthlyFixedVariableSalary)
                     + ((withDefault' $ e ^. P.employeeMonthlyFixedVariableSalary) / 0.66)
                     + (withDefault' $ e ^. P.employeeMonthlyVariableSalary))
    let averageTmc xs = getAverage $ foldMap mkAverage $ catMaybes $ map tmc xs
    let offices = [minBound .. maxBound]
    let tmcForOffice office =
            let officeEmployees = filter (\e -> e ^. P.employeeOffice == office) employees'
            in if length officeEmployees > 20 then
                 Just (officeToText office, averageTmc officeEmployees)
               else
                 Nothing
    pure $ MonthlyCompensation
        { _mcTotal = averageTmc employees'
        , _mcPerOffice = Map.fromList $ catMaybes $ tmcForOffice <$> offices
        }
  where
    withDefault currencyMap cur = conversion currencyMap cur . realToFrac . fromMaybe 0
    conversion :: (Map Text Double) -> P.Currency -> Double -> Double
    conversion currencyMap cur val = maybe val (\rate -> val / rate) (currencyMap ^.at (P.currencyToText cur))

-- Give the currency rates against euro averaging over 30 days
fetchCurrencyRates :: Ctx -> IO (Map Text Double)
fetchCurrencyRates ctx = do
    now <- currentDay
    let interval = [(addDays (-30) now) .. now]
    let address = "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist-90d.xml"
    request <- liftIO $ HTTP.parseRequest address
    response <- liftIO $ HTTP.httpLbs request (ctxManager ctx)
    let parsedResponse :: Map Day (Map Text (Average Double))
        parsedResponse = toMap $ Xeno.parse $ LBS.toStrict $ HTTP.responseBody response
    let dataForDays = Map.unionsWith (<>) $ catMaybes $ map (\d -> parsedResponse ^.at d) interval
    pure $ Map.map getAverage dataForDays
  where
    toMap (Right node) = Map.fromList $ catMaybes $ map processDay $ Xeno.children $ head $ drop 2 $ Xeno.children node
    toMap _ = error "Problem parsing currency rate data"

    processDay node = (,) <$> (readMaybe =<< T.unpack . decodeUtf8Lenient . snd <$> listToMaybe (Xeno.attributes node)) <*> (Just $ Map.fromList $ catMaybes $ map processCurrency $ Xeno.children node)

    processCurrency node = let m = Map.fromList $ Xeno.attributes node
                           in (,) <$> (decodeUtf8Lenient <$> m ^.at "currency") <*> ((T.unpack . decodeUtf8Lenient <$> m ^.at "rate") >>= readMaybe >>= Just . mkAverage)
-------------------------------------------------------------------------------
-- Per Enum/Bounded
-------------------------------------------------------------------------------

type PerTribe = Per Tribe

-- | This might be useful elsewhere.
--
newtype Per k a = Per (Map k a)
  deriving (Functor, Foldable, Traversable, Generic)

instance (NFData k, NFData a) => NFData (Per k a)

instance (Ord k, Enum k, Bounded k) => Applicative (Per k) where
    pure  = pureRep
    (<*>) = apRep

instance (Ord k, Enum k, Bounded k) => Distributive (Per k) where
    distribute = distributeRep

instance (Ord k, Enum k, Bounded k) => Representable (Per k) where
    type Rep (Per k) = k

    tabulate f = Per $ Map.fromList
        [ (t, f t)
        | t <- [ minBound .. maxBound ]
        ]

    index (Per m) t = fromMaybe (error "panic! index @Per") $ m ^? ix t

-------------------------------------------------------------------------------
-- TMap
-------------------------------------------------------------------------------

data TMap k v = TMap v (Map k v)
  deriving (Functor, Foldable, Traversable, Generic)

instance (NFData k, NFData a) => NFData (TMap k a)

indexTMap :: Ord k => k -> TMap k v -> v
indexTMap k (TMap def m) = fromMaybe def (Map.lookup k m)

instance Ord k => Applicative (TMap k) where
    pure x = TMap x mempty

    TMap f fm <*> TMap x xm = TMap (f x) $ Map.unions
        [ Map.intersectionWith ($) fm xm
        , fmap f xm
        , fmap ($ x) fm
        ]
