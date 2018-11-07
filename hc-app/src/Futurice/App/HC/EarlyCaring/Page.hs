{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.EarlyCaring.Page (earlyCaringPage, EarlyCaringPlanMill (..)) where

import Data.Either               (isRight)
import Data.Fixed                (Deci)
import Data.Time                 (addDays, diffDays)
import FUM.Types.Login           (Login)
import Futurice.Integrations     (Employee (..))
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Futurice.Tribe            (tribeToText)
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()
import Web.HttpApiData           (toUrlPiece)

import qualified Data.Aeson         as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Personio           as P
import qualified PlanMill           as PM

import Futurice.App.HC.API
import Futurice.App.HC.EarlyCaring.Template
import Futurice.App.HC.EarlyCaring.Types
import Futurice.App.HC.Markup

-- | PlanMill data needed to make early caring report
data EarlyCaringPlanMill = EarlyCaringPlanMill
    { ecpmLogin       :: !Login
    , ecpmPMUid       :: !PM.UserId
    , ecpmEmployee    :: !Employee
    , ecpmCapacities  :: !PM.UserCapacities
    , ecpmTimereports :: !PM.Timereports
    , ecpmBalance     :: !PM.TimeBalance
    }
  deriving Show

earlyCaringPage
    :: Either Login ByteString
    -> Day
    -> Interval Day
    -> [P.Employee]
    -> [EarlyCaringPlanMill]
    -> PM.Absences
    -> Map (PM.EnumValue PM.Absence "absenceType") Text
    -> HtmlPage "early-caring"
earlyCaringPage esecret today interval personioEmployees0 planmillData absences0 absenceTypes = page_ "Early caring report" (Just NavEarlyCaring) $ do
    ul_ $ do
        li_ $ toHtml $ "To approximate missing hours the PlanMill hour making data from " ++ show interval ++ " is used"
        li_ "Externals are filtered from this report"
        li_ "Sickness absence information is missing"
        li_ "Report is grouped by supervisors"
        li_ "Absences are counted in the last 90 days (around 3 months)"
        li_ "Adjacent absences are counted as one. (Weekends separate)"
        li_ "Absence days are counted in the last 365 days (a year)"
        li_ $ i_ "TODO" <> " Sick days counts weekends and holidays days too"
        li_ "Monthly balances are emphasised if they are under -10 or over 20 hours"

    when hasSecret $ fullRow_ $ div_ [ class_ "callout primary" ] $ do
        p_ $ a_ [ recordHref_ recEarlyCaring True ] "Supervisor view"

    when hasSecret $ fullRow_ $ div_ [ class_ "callout primary" ] $ do
        p_ "Send early caring emails to the supervisors"
        ul_ [ id_ "futu-early-caring-all-supervisors" ] mempty
        button_ [ id_ "futu-early-caring-send-all", class_ "button primary", disabled_ "disabled" ] "Send to all"

    for_ balances $ \bs -> do
        let ms = balanceSupervisor $ NE.head bs
        let supervisorHeader e = mconcat
                [ e ^. P.employeeFullname
                , " ("
                , tribeToText (e ^. P.employeeTribe)
                , ")"
                ]

        when (isSelfOrSecret ms) $ do
            fullRow_ $ h2_ $ maybe "No supervisor" supervisorHeader ms
            fullRow_ $ table_ $ do
                thead_ $ tr_ $ do
                    th_ "Name"
                    th_ "Contract type"
                    th_ "Flex balance"
                    th_ "Missing hours"
                    th_ [ dashedUnderline, title_ "Sum of flex balance and missing hours, that's what the flex balance would be if all missing hours are marked" ] "Sum (flex + missing)"
                    th_ [ dashedUnderline, title_ "Marked hours - Capacity = Month balance" ] $ toHtml $ "Balance in " <> toUrlPiece month1
                    th_ [ dashedUnderline, title_ "Marked hours - Capacity = Month balance" ] $ toHtml $ "Balance in " <> toUrlPiece month2
                    th_ [ dashedUnderline, title_ "Separate sickness absences in last 90 days (about 3 months)" ] "Sick. absences (3m)"
                    th_ [ dashedUnderline, title_ "Sickness absences days in last 365 days (1 year)" ]            "Sick. days (1y)"
                    th_ [ dashedUnderline, title_ "As marked in PlanMill, adjacent absenced not combined" ] "Sickess absences (1y)"


                tbody_ $ for_ bs $ \b-> tr_ $ do
                    let e = balanceEmployee b
                    td_ $ toHtml $ e ^. P.employeeFullname
                    td_ $ traverse_ toHtml $ e ^. P.employeeContractType
                    td_ [ style_ "text-align: right" ] $ toHtml $ balanceHours b
                    td_ [ style_ "text-align: right" ] $ toHtml $ balanceMissingHours b
                    td_ [ style_ "text-align: right" ] $ toHtml $ balanceHours b + balanceMissingHours b
                    td_ [ style_ "white-space: nowrap" ] $ monthFlexHtml $ balanceMonthFlex b ^. ix month1
                    td_ [ style_ "white-space: nowrap" ] $ monthFlexHtml $ balanceMonthFlex b ^. ix month2
                    td_ [ style_ "text-align: right" ] $ showHighlight 4  $ countAbsences today $ balanceAbsences b
                    td_ [ style_ "text-align: right" ] $ showHighlight 20 $ countAbsenceDays today $ balanceAbsences b
                    td_ $ forWith_ (";" <> br_ []) (balanceAbsences b) intervalDayToHtml

            for_ ms $ \s -> fullRow_ $ case s ^. P.employeeEmail of
                Nothing -> div_ [ class_ "callout warning" ] "Supervisor don't have an email set"
                Just a -> div_ [ class_ "callout secondary" ] $ do
                    h3_ "Reminder mail"
                    let email = EarlyCaringEmail a "Early caring email" $ renderTemplate
                            (s ^. P.employeeFirst)
                            today
                            interval
                            (toList bs)

                    pre_ $ toHtml email
                    for_ esecret $ \secret -> do
                        hr_ []
                        button_
                            [ data_ "futu-early-caring-mail" (decodeUtf8Lenient (Aeson.encode (mkSignedBlob secret email) ^. strict))
                            , data_ "futu-early-caring-name" $ s ^. P.employeeFullname
                            , class_ "button primary"
                            , disabled_ "disabled"
                            ]
                            "Send"

    forSelf_ esecret $ \self -> do
        h2_ "Your all subordinates"
        condensedTable_ $ do
            thead_ $ tr_ $ do
                th_ "Name"
                th_ "Contract type"
                th_ "Flex balance"
                th_ "Missing hours"
                th_ [ dashedUnderline, title_ "Sum of flex balance and missing hours, that's what the flex balance would be if all missing hours are marked" ] "Sum (flex + missing)"
                th_ [ dashedUnderline, title_ "Marked hours - Capacity = Month balance" ] $ toHtml $ "Balance in " <> toUrlPiece month1
                th_ [ dashedUnderline, title_ "Marked hours - Capacity = Month balance" ] $ toHtml $ "Balance in " <> toUrlPiece month2
                th_ [ dashedUnderline, title_ "Separate sickness absences in last 90 days (about 3 months)" ] "Sick. absences (3m)"
                th_ [ dashedUnderline, title_ "Sickness absences days in last 365 days (1 year)" ]            "Sick. days (1y)"

            tbody_ $ for_ (sortOn (view P.employeeLast . balanceEmployee) rawBalances) $ \b -> do
                let e = balanceEmployee b
                when (Just self == balanceSupervisor b ^? _Just . P.employeeLogin . _Just) $ tr_ $ do
                    td_ $ toHtml $ e ^. P.employeeFullname
                    td_ $ traverse_ toHtml $ e ^. P.employeeContractType
                    td_ [ style_ "text-align: right" ] $ toHtml $ balanceHours b
                    td_ [ style_ "text-align: right" ] $ toHtml $ balanceMissingHours b
                    td_ [ style_ "text-align: right" ] $ toHtml $ balanceHours b + balanceMissingHours b
                    td_ [ style_ "white-space: nowrap" ] $ monthFlexHtml $ balanceMonthFlex b ^. ix month1
                    td_ [ style_ "white-space: nowrap" ] $ monthFlexHtml $ balanceMonthFlex b ^. ix month2
                    td_ [ style_ "text-align: right" ] $ showHighlight 4  $ countAbsences today $ balanceAbsences b
                    td_ [ style_ "text-align: right" ] $ showHighlight 20 $ countAbsenceDays today $ balanceAbsences b


    -- Show active in Personio only when there's secret:
    when hasSecret $ do
        fullRow_ $ h2_ "Active in Personio, cannot find PlanMill data"
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Personio"
                th_ "Name"
                th_ "Supervisor"
                th_ "Login"

            tbody_ $ for_ notInPlanMill $ \e -> tr_ $ do
                td_ $ toHtml $ e ^. P.employeeId
                td_ $ toHtml $ e ^. P.employeeFullname
                td_ $ traverse_ (toHtml . view P.employeeFullname) $ supervisor e
                td_ $ traverse_ toHtml $ e ^. P.employeeLogin

  where
    hasSecret = isRight esecret
    isSelf p = case esecret of
        Right _    -> False
        Left login -> Just login == p ^? _Just . P.employeeLogin . _Just

    forSelf_ (Left e)  f = f e
    forSelf_ (Right _) _ = pure ()

    isSelfOrSecret p = isSelf p || hasSecret

    month1 = dayToMonth $ inf interval
    month2 = succ month1

    dashedUnderline = style_ "text-decoration: underline; text-decoration-style: dashed"

    personioEmployees = filter
        (\e -> P.employeeIsActive today e && e ^. P.employeeEmploymentType == Just P.Internal)
        personioEmployees0

    pMap :: Map P.EmployeeId P.Employee
    pMap = Map.fromList $ map (\e -> (e ^. P.employeeId, e)) personioEmployees

    pmMap :: Map Login EarlyCaringPlanMill
    pmMap = Map.fromList $ map (\x -> (ecpmLogin x, x)) planmillData

    absences :: PM.UserId -> [Interval Day]
    absences uid = absenceMap ^. ix uid

    absenceMap :: Map PM.UserId [Interval Day]
    absenceMap = Map.fromListWith (++) $ mapMaybe f (toList absences0) where
        f a | PM.absenceAbsenceType a `notElem` sickAbsences = Nothing
            | PM.absenceFinish a < day                       = Nothing
            | otherwise = Just (PM.absencePerson a, [PM.absenceStart a ... PM.absenceFinish a])

        -- we aren't interested in older absences, let's filter them
        day = addDays (negate 365) today

        -- NOTE: hardcoded values, as there is really no way to find out
        -- which types are sick leaves.
        sickAbsences = map PM.EnumValue [1010, 1025]

    absencesMap' :: Map PM.UserId [PM.Absence]
    absencesMap' = Map.fromListWith (++) $ map f $ toList absences0 where
        f a = (PM.absencePerson a, [a])

    supervisor :: P.Employee -> Maybe P.Employee
    supervisor e = do
        eid <- e ^. P.employeeSupervisorId
        pMap ^? ix eid

    (inPlanMill, notInPlanMill) = partitionMaybe checkInPlanMill personioEmployees where
        checkInPlanMill e = do
            login <- e ^. P.employeeLogin
            pm <- pmMap ^? ix login
            return (e, pm)

    balances :: [NonEmpty Balance]
    balances
        = sortOn (fmap (view P.employeeTribe) . balanceSupervisor . NE.head)
        $ NE.groupBy ((==) `on` superId)
        $ sortOn superId
        $ filter (not . isHourly)
        $ filter (not . isPermanentAllIn)
        $ filter (not . balanceNormal today)
        $ rawBalances
      where
        superId = fmap (view P.employeeId) . balanceSupervisor

    rawBalances = map (uncurry toBalance) inPlanMill

    toBalance :: P.Employee -> EarlyCaringPlanMill -> Balance
    toBalance e pm = Balance
        { balanceEmployee     = e
        , balanceSupervisor   = supervisor e
        , balanceHours        = ndtConvert' balanceMinutes
        , balanceMissingHours = missingHours (ecpmTimereports pm) (ecpmCapacities pm)
        , balanceAbsences     = ab
        , balanceMonthFlex    = monthFlex pm
        }
      where
        PM.TimeBalance balanceMinutes = ecpmBalance pm
        ab = absences (ecpmPMUid pm)

    monthFlex :: EarlyCaringPlanMill -> Map Month MonthFlex
    monthFlex pm = Map.unionWith (<>) result absenceResult
      where
        -- hours
        result = Map.fromListWith (<>) $
            map fromUC (toList $ ecpmCapacities pm) <> map fromTR (toList $ ecpmTimereports pm)

        absenceResult = Map.restrictKeys fromAbsences (Map.keysSet result)

        fromUC :: PM.UserCapacity -> (Month, MonthFlex)
        fromUC uc =
            ( dayToMonth $ PM.userCapacityDate uc
            , MonthFlex 0 (ndtConvert' $ PM.userCapacityAmount uc)
            )

        ucPerDay :: Map Day (NDT 'Minutes Int)
        ucPerDay = Map.fromList
            [ (PM.userCapacityDate uc, PM.userCapacityAmount uc)
            | uc <- toList $ ecpmCapacities pm
            ]

        fromTR :: PM.Timereport -> (Month, MonthFlex)
        fromTR tr = (month , MonthFlex (ndtConvert' $ PM.trAmount tr) 0)
          where
            month = dayToMonth (PM.trStart tr)

        -- we go thru absences, substracting flex balances from MonthFlex
        fromAbsences :: Map Month MonthFlex
        fromAbsences = Map.unionsWith (<>) $ map f (absencesMap' ^. ix (ecpmPMUid pm)) where
            f :: PM.Absence -> Map Month MonthFlex
            f a | absenceTypes ^? ix (PM.absenceAbsenceType a) == Just "Balance leave" =
                Map.fromListWith (<>)
                    [ (dayToMonth day, MonthFlex (negate $ maybe 0 ndtConvert' $ ucPerDay ^? ix day) 0)
                    | day <- [ PM.absenceStart a .. PM.absenceFinish a ]
                    ]
                | otherwise = mempty

    showHighlight :: (Ord a, Show a, Monad m) => a -> a -> HtmlT m ()
    showHighlight m n
        | n < m     = toHtml $ show n
        | otherwise = b_ $ toHtml $ show  n

    monthFlexHtml :: Monad m => MonthFlex -> HtmlT m ()
    monthFlexHtml (MonthFlex marked capa) = do
        toHtml marked
        " - "
        toHtml capa
        " = "
        emph_ $ toHtml bal
      where
        bal = marked - capa
        emph_ :: Monad m => HtmlT m () -> HtmlT m ()
        emph_
            | bal < -10 || bal > 20 = b_
            | otherwise             = id

    intervalDayToHtml :: Monad m => Interval Day -> HtmlT m ()
    intervalDayToHtml i
        | mi == ma = toHtml (show mi)
        | otherwise = span_ [ style_ "white-space: nowrap", title_ $ "length: " <> textShow (diffDays ma mi + 1) <> " days" ] $ do
            toHtml $ show mi
            toHtmlRaw ("&ndash;" :: String)
            toHtml $ show ma
      where
        mi = inf i
        ma = sup i

missingHours :: PM.Timereports -> PM.UserCapacities -> NDT 'Hours Deci
missingHours trs ucs =
    -- Sum of capacity hours of days with no (or zero) hour markings
    sum $ capacities `Map.difference` nonEmptyHours
  where
    hours :: Map Day (NDT 'Hours Deci)
    hours = Map.fromListWith (+) $ map mk $ toList trs where
        mk tr = (PM.trStart tr, ndtConvert' $ PM.trAmount tr)

    nonEmptyHours :: Map Day (NDT 'Hours Deci)
    nonEmptyHours = Map.filter (> 0) hours

    capacities :: Map Day (NDT 'Hours Deci)
    capacities = Map.fromListWith (+) $ map mk $ toList ucs where
        mk uc = (PM.userCapacityDate uc, ndtConvert' $ PM.userCapacityAmount uc)

-- partition with evidence function
partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe p = foldr select ([], []) where
    select a (bs, as) = case p a of
        Just b  -> (b : bs, as)
        Nothing -> (bs, a : as)
