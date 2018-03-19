{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.EarlyCaring.Page (earlyCaringPage, EarlyCaringPlanMill (..)) where

import Data.Fixed                (Deci)
import Data.Time                 (addDays)
import FUM.Types.Login           (Login)
import Futurice.Integrations     (Employee (..))
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Numeric.Interval.NonEmpty (Interval, inf, (...))
import Prelude ()
import Web.HttpApiData           (toUrlPiece)

import qualified Data.Aeson         as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Personio           as P
import qualified PlanMill           as PM

import Futurice.App.HC.EarlyCaring.Template
import Futurice.App.HC.EarlyCaring.Types
import Futurice.App.HC.Markup

-- | PlanMill data needed to make early caring report
data EarlyCaringPlanMill = EarlyCaringPlanMill
    { ecpmLogin         :: !Login
    , ecpmPMUid         :: !PM.UserId
    , ecpmEmployee      :: !Employee
    , ecpmCapacities    :: !PM.UserCapacities
    , ecpmTimereports   :: !PM.Timereports
    , ecpmBalance       :: !PM.TimeBalance
    }
  deriving Show

earlyCaringPage
    :: ByteString
    -> Day
    -> Interval Day
    -> [P.Employee]
    -> [EarlyCaringPlanMill]
    -> PM.Absences
    -> HtmlPage "early-caring"
earlyCaringPage secret today interval personioEmployees0 planmillData absences0 = page_ "Early caring report" (Just NavEarlyCaring) $ do
    ul_ $ do
        li_ $ toHtml $ "To approximate missing hours the PlanMill hour making data from " ++ show interval ++ " is used"
        li_ "Externals are filtered from this report"
        li_ "Sickness absence information is missing"
        li_ "Report is grouped by supervisors"
        li_ "Absences are counted in the last 90 days (around 3 months)"
        li_ "Adjacent absences are counted as one. (Weekends separate)"
        li_ "Absence days are counted in the last 365 days (a year)"
        li_ "TODO: Sick days counts weekends and holidays days too"
        li_ "Monthly balances are emphasised if they are under -10 or over 20 hours"

    fullRow_ $ div_ [ class_ "callout primary" ] $ do
        p_ "Send early caring emails to the supervisors"
        ul_ [ id_ "futu-early-caring-all-supervisors" ] mempty
        button_ [ id_ "futu-early-caring-send-all", class_ "button primary", disabled_ "disabled" ] "Send to all"

    for_ balances $ \bs -> do
        let ms = balanceSupervisor $ NE.head bs
        let supervisorHeader e = do
                toHtml $ e ^. P.employeeFullname
                " ("
                toHtml $ e ^. P.employeeTribe
                ")"

        fullRow_ $ h2_ $ maybe "No supervisor" supervisorHeader ms
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Name"
                th_ "Flex balance"
                th_ "Missing hours"
                th_ [ dashedUnderline, title_ "Sum of flex balance and missing hours, that's what the flex balance would be if all missing hours are marked" ] "Sum (flex + missing)"
                th_ [ dashedUnderline, title_ "Marked hours - Capacity = Month balance" ] $ toHtml $ "Balance in " <> toUrlPiece month1
                th_ [ dashedUnderline, title_ "Marked hours - Capacity = Month balance" ] $ toHtml $ "Balance in " <> toUrlPiece month2
                th_ [ dashedUnderline, title_ "Separate sickness absences in last 90 days (about 3 months)" ] "Sick. absences (3m)"
                th_ [ dashedUnderline, title_ "Sickness absences days in last 365 days (1 year)" ]            "Sick. days (1y)"
                th_ [ dashedUnderline, title_ "As marked in PlanMill, adjacent absenced not combined" ] "Sickess absences (1y)"

            let showHighlight :: (Ord a, Show a, Monad m) => a -> a -> HtmlT m ()
                showHighlight m n
                    | n < m     = toHtml $ show n
                    | otherwise = b_ $ toHtml $ show  n

            tbody_ $ for_ bs $ \b-> tr_ $ do
                let e = balanceEmployee b
                td_ $ toHtml $ e ^. P.employeeFullname
                td_ [ style_ "text-align: right" ] $ toHtml $ balanceHours b
                td_ [ style_ "text-align: right" ] $ toHtml $ balanceMissingHours b
                td_ [ style_ "text-align: right" ] $ toHtml $ balanceHours b + balanceMissingHours b
                td_ $ monthFlexHtml $ balanceMonthFlex b ^. ix month1
                td_ $ monthFlexHtml $ balanceMonthFlex b ^. ix month2
                td_ [ style_ "text-align: right" ] $ showHighlight 4  $ countAbsences today $ balanceAbsences b
                td_ [ style_ "text-align: right" ] $ showHighlight 20 $ countAbsenceDays today $ balanceAbsences b
                td_ $ forWith_ (br_ []) (balanceAbsences b) $ toHtml . show

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
                hr_ []
                button_
                    [ data_ "futu-early-caring-mail" (decodeUtf8Lenient (Aeson.encode (mkSignedBlob secret email) ^. strict))
                    , data_ "futu-early-caring-name" $ s ^. P.employeeFullname
                    , class_ "button primary"
                    , disabled_ "disabled"
                    ]
                    "Send"

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
        $ filter (not . balanceNormal today)
        $ map (uncurry toBalance) inPlanMill
      where
        superId = fmap (view P.employeeId) . balanceSupervisor

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
    monthFlex pm = Map.fromListWith (<>) $
        map fromUC (toList $  ecpmCapacities pm) <> map fromTR (toList $ ecpmTimereports pm)
      where
        fromUC :: PM.UserCapacity -> (Month, MonthFlex)
        fromUC uc =
            ( dayToMonth $ PM.userCapacityDate uc
            , MonthFlex 0 (ndtConvert' $ PM.userCapacityAmount uc)
            )

        fromTR :: PM.Timereport -> (Month, MonthFlex)
        fromTR tr =
            ( dayToMonth $ PM.trStart tr
            , MonthFlex (ndtConvert' $ PM.trAmount tr) 0
            )

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
