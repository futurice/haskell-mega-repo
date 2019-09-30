{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.PlanMillSync.IndexPage (indexPage) where

import Control.Lens
       (IndexedGetting, filtered, ifoldMapOf, iforOf_)
import Control.Monad.Writer.CPS    (Writer, runWriter)
import Data.Map.Lens               (toMapOf)
import Data.Maybe                  (isNothing)
import Data.Monoid                 (Any (..))
import Data.Ord                    (Down (..))
import Data.These                  (_That, _These, _This)
import FUM.Types.Login             (Login, loginRegexp)
import Futurice.Company
       (companyToText, countryCompany, countryFinland)
import Futurice.Constants          (competenceMap)
import Futurice.CostCenter
import Futurice.Email              (emailToText)
import Futurice.Prelude
import Prelude ()
import Servant                     (toUrlPiece)
import Servant.Links               (Link, safeLink)
import Text.Regex.Applicative.Text (match)

import qualified Data.Text as T
import qualified Personio  as P
import qualified PlanMill  as PM

import Futurice.App.PlanMillSync.Actions
import Futurice.App.PlanMillSync.API
import Futurice.App.PlanMillSync.Markup
import Futurice.App.PlanMillSync.Types   (PMUser (..))

itoListWithOf :: IndexedGetting i (Endo [x]) s a -> (i -> a -> x) ->  s -> [x]
itoListWithOf l f s = appEndo (ifoldMapOf l (\i a -> Endo (f i a :)) s) []

indexPage
    :: Day
    -> [PMUser]
    -> [P.Employee]
    -> HtmlPage "index"
indexPage today planmills personios = page_ "PlanMill sync" (Just NavHome) $ do
    div_ [ class_ "callout alert "] $ ul_ $ do
        li_ $ "PlanMill data is fetched directly from PlanMIll."
        li_ $ "Personio data updates every ~5min"
        li_ $ "PlanMill data updates every night"
        li_ $ do
            "When values are different, both are shown: "
            b_ "Personio ≠ PlanMill"
            "."

    h2_ "People Active in PlanMIll but not in Personio"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Login"
            th_ "Planmill"
            th_ "Name"
            th_ "PlanMill Team"
            th_ "Contract span"

        tbody_ $ iforOf_ (ifolded . _This) employees $ \login pm -> do
            let pmu :: PM.User
                pmu = pmUser pm

            when (pmPassive pm == "Active") $ tr_ $ do
                td_ $ toHtml login
                td_ $ toHtml $ pmu ^. PM.identifier
                td_ $ toHtml $ PM.uFirstName pmu <> " " <> PM.uLastName pmu
                td_ $ traverse_ (toHtml . PM.tName) $ pmTeam pm
                td_ $ do
                    let pmStart = PM.uHireDate pmu
                    let pmEnd = PM.uDepartDate pmu
                    noWrapSpan_ $ toHtml $ formatDateSpan pmStart pmEnd

    h2_ "People Active in Personio but not in PlanMill"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Login"
            th_ "Personio"
            th_ "Name"
            th_ "Tribe"
            th_ "Office"
            th_ "Contract span"
            th_ "HR Number"

        tbody_ $ iforOf_ (ifolded . _That) employees $ \login p -> do
            when (P.employeeIsActive today p) $ tr_ $ do
                td_ $ toHtml login
                personioHtml p

    h2_ $ "People Active in Personio, but without login"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Personio"
            th_ "Name"
            th_ "Tribe"
            th_ "Office"
            th_ "Contract span"
            th_ "HR Number"
        tbody_ $ for_ personios $ \p ->
            when (P.employeeIsActive today p && isNothing (p ^. P.employeeLogin)) $
                tr_ $ personioHtml p

    h2_ "Cross-check of people in PlanMill and Personio"

    h3_ "Employees"
    summaryTable employees $ \pm p ->
        p ^. P.employeeEmploymentType /= Just P.External &&
        (pmPassive pm == "Active" || P.employeeIsActive today p)

    h3_ "Subcontractors"
    summaryTable employees $ \pm p ->
        p ^. P.employeeEmploymentType == Just P.External &&
        (pmPassive pm == "Active" || P.employeeIsActive today p)

    h3_ "Inactive"
    summaryTable employees $ \pm p ->
        not (pmPassive pm == "Active" || P.employeeIsActive today p)

  where
    summaryTable
        :: Monad m
        => Map Login (These PMUser P.Employee)
        -> (PMUser -> P.Employee -> Bool)
        -> HtmlT m ()
    summaryTable es predicate = sortableTable_ $ do
        thead_ $ tr_ $ do
            th_ "Login"
            th_ "Personio"
            th_ "Planmill"
            th_ "Name"
            -- td_ "Tribe"
            -- td_ "Office"
            th_ "Status"
            th_ "Ext"
            th_ "Contract type"
            th_ "Contract span"

            th_ "HR Number"
            th_ "PM Superior"
            th_ "Cost center = PM Team"
            th_ "Expat"
            th_ "PM Account"
            th_ "PM email"
            th_ "Competence"

        tbody_ $ traverse_ id elements2
      where
        elements0 = itoListWithOf
            (ifolded . _These . filtered (uncurry predicate))
            processBoth es
        elements1 = map (runWriter . commuteHtmlT) elements0
        elements2 = map fst $ sortOn (Down . snd) elements1

    processBoth :: MonadWriter Any m => Login -> (PMUser, P.Employee) -> HtmlT m ()
    processBoth login (pm, p) = tr_ $ do
        let pmu = pmUser pm
        let pmt = pmTeam pm

        td_ $ toHtml login
        td_ $ toHtml $ p ^. P.employeeId
        td_ $ toHtml $ pmu ^. PM.identifier
        td_ $ toHtml $ p ^. P.employeeFullname
        -- td_ $ toHtml $ p ^. P.employeeTribe
        -- td_ $ toHtml $ p ^. P.employeeOffice

        cell_ $ do
            let pActive = P.employeeIsActive today p

            if pActive then "Active" else "Inactive"

            unless (pActive == (p ^. P.employeeStatus `elem` [P.Active, P.Leave])) $
                markPersonioCell $ mconcat
                    [ "Personio status and contract dates disagree"
                    , "Status: "
                    , P.statusToText (p ^. P.employeeStatus)
                    ]

            unless (pActive == (pmPassive pm == "Active")) $ do
                markFixableCell $ mconcat
                    [ "PlanMill active status doesn't agree with Personio status: Personio "
                    , P.statusToText (p ^. P.employeeStatus)
                    , " ≇ PM "
                    , pmPassive pm
                    ]
                " ≠ "
                toHtml $ pmPassive pm

            div_ [ class_ "button-group"] $ do
                -- action to update the end date
                for_ (canUpdateStatus today p pm) $ \s -> button_
                    [ data_ "futu-post-button" $ linkToText $ safeLink planmillSyncApi updateStatusEndpoint login
                    , class_ "button"
                    , disabled_ "disabled"
                    ] $
                    toHtml $ "Update PM status to " <> s

        -- employment type: int/ext
        cell_ $ case p ^. P.employeeEmploymentType of
            Nothing -> markPersonioCell "Personio employee should have employment type set"
            Just P.Internal -> pure ()
            Just P.External -> "Ext"

        -- Contract type
        cell_ $ case p ^. P.employeeContractType of
            Nothing -> markPersonioCell "Personio employee should have contract type set"
            Just pContract -> do
                let pEmploymentType = p ^. P.employeeEmploymentType
                let pContract' = contractType pEmploymentType pContract (p ^. P.employeeSalaryType)

                noWrapSpan_ $ toHtml pContract'
                toHtml $ " (" <> textShow pContract <> ")"

                when (pEmploymentType == Just P.External && pContract /= P.FixedTerm) $
                    markPersonioCell "Externals should have contract type FixedTerm"

                unless (pContract' == pmContract pm) $ do
                    markErrorCell "Contract types don't agree"
                    " ≠ "
                    noWrapSpan_ $ toHtml $ pmContract pm

                div_ [ class_ "button-group"] $ do
                    -- action to update the end date
                    for_ (canUpdateContractType p pm) $ \t -> button_
                        [ data_ "futu-post-button" $ linkToText $ safeLink planmillSyncApi updateContractTypeEndpoint login
                        , class_ "button"
                        , disabled_ "disabled"
                        ] $
                        toHtml $ "Update contract type to " <> t

        -- Contract span
        cell_ $ do

            let pStart = p ^. P.employeeHireDate
            let pEnd = p ^. P.employeeEndDate

            let pmStart = PM.uHireDate pmu
            let pmEnd = PM.uDepartDate pmu

            -- there should be starting date(s)!
            when (isNothing pStart) $ markErrorCell "Employee should have hire date in Personio"
            when (isNothing pmStart) $ markErrorCell "Employee should have hire date in PlanMill"

            when (p ^. P.employeeContractType == Just P.FixedTerm) $ do
                pure ()
                -- when (isNothing pEnd) markErrorCell
                -- when (isNothing pmEnd) markErrorCell

            noWrapSpan_ $ toHtml $ formatDateSpan pStart pEnd

            {-
            let startDifferent = case (pStart, pmStart) of
                    (Nothing, Nothing) -> False
                    (Nothing, Just _)  -> True
                    (Just _,  Nothing) -> True
                    -- planmill start date could be larger than actual start date.
                    (Just a, Just b)   -> a > b
            -}

            let endDifferent = pEnd /= pmEnd

            when endDifferent $ do
                markErrorCell "Contract dates differ"
                " ≠ "
                noWrapSpan_ $ toHtml $ formatDateSpan pmStart pmEnd

            div_ [ class_ "button-group"] $ do
                -- action to update the end date
                for_ (canUpdateDepartDate p pm) $ \_ -> button_
                    [ data_ "futu-post-button" $ linkToText $ safeLink planmillSyncApi addDepartDateEndpoint login
                    , class_ "button"
                    , disabled_ "disabled"
                    ]
                    "Update depart date"

        -- hr number
        cell_ $ do
            let planmillNum = PM.uOperationalId pmu
            let personioNum = p ^. P.employeeHRNumber

            -- only internals at helsinki & tampere -offices
            when (p ^. P.employeeEmploymentType == Just P.Internal && p ^. P.employeeCountry == Just countryFinland) $ do
                traverse_ (toHtml . show) personioNum
                unless (fromMaybe False $ liftA2 (==) planmillNum personioNum) $do
                    markErrorCell "HR number doesn't match"
                    " ≠ "
                    traverse_ (toHtml . show) planmillNum

        -- superior
        cell_ $ for_ (PM.uSuperior pmu) $ \sv -> do
            markFixableCell "PlanMill employee shouldn't have supervisor set"
            toHtml (show sv)

        -- Cost centers
        cell_ $ do
            let planmillCC = costCenterFromText =<< (PM.tName <$> pmt)
            let personioCC = p ^. P.employeeCostCenter
            let ccEqual = planmillCC == personioCC
            traverse_ toHtml $  p ^. P.employeeCostCenter
            unless ccEqual $ do
                markErrorCell "Cost centers should be equal"
                " ≠ "
                traverse_ (toHtml . PM.tName) pmt

        -- Expat
        td_ $ when (p ^. P.employeeExpat) "Expat"

        -- PM Account
        cell_ $ case pmAccount pm of
            Nothing -> markErrorCell "PlanMill employee doesn't have account set"
            Just a  -> do
                let name = PM.saName a
                unless (Just name == fmap companyToText (countryCompany <$> p ^. P.employeeCountry)) $ do
                    markErrorCell "PM Account doesn't agree with Personio Country value"
                    maybe "No country" toHtml (p ^. P.employeeCountry)
                    " ≠ "
                toHtml name

        -- PM email
        cell_ $ case (PM.uEmail pmu, p ^. P.employeeEmail) of
            (Nothing, Just e) -> do
                markFixableCell "PlanMill user doesn't have email set"
                toHtml e
            (Just e, Nothing) -> do
                markFixableCell "No email or wrong format email in Personio"
                toHtml e
            (Nothing, Nothing) -> markFixableCell "No email in Planmill or in Personio"
            (Just e, Just pe)  ->
                if e == emailToText pe
                then "OK"
                else do
                    markFixableCell ("Email should be '" <> emailToText pe <> "' according to Personio")
                    toHtml e

        -- Role & Competence
        cell_ $ do
            let pCompetence  = p ^. P.employeeRole
            let pmCompetence = PM.uCompetence pmu
            toHtml pCompetence
            unless (eqCompareCompetence pCompetence pmCompetence) $ do
                markErrorCell "Competences don't match"
                " ≠ "
                traverse_ toHtml pmCompetence

    planmillMap :: Map Login PMUser
    planmillMap = toMapOf (folded . getter f . _Just . ifolded) planmills
      where
        f u = do
            login <- match loginRe (PM.uUserName (pmUser u))
            pure (login, u)

        loginRe = "https://login.futurice.com/openid/" *> loginRegexp

    personioMap :: Map Login P.Employee
    personioMap = toMapOf (folded . getter f . _Just . ifolded) personios
      where
        f u = do
            login <- u ^. P.employeeLogin
            pure (login, u)

    employees :: Map Login (These PMUser P.Employee)
    employees = align planmillMap personioMap

-------------------------------------------------------------------------------
-- Only in Personio
-------------------------------------------------------------------------------

personioHtml :: Monad m => P.Employee -> HtmlT m ()
personioHtml p = fst $ runWriter $ commuteHtmlT $ do
    td_ $ toHtml $ p ^. P.employeeId
    td_ $ toHtml $ p ^. P.employeeFullname
    td_ $ toHtml $ p ^. P.employeeTribe
    td_ $ toHtml $ p ^. P.employeeOffice
    td_ $ do
        let pStart = p ^. P.employeeHireDate
        let pEnd = p ^. P.employeeEndDate
        noWrapSpan_ $ toHtml $ formatDateSpan pStart pEnd
    cell_ $ case p ^. P.employeeHRNumber of
        Just x | x > 0 -> toHtml (show x) -- TODO: remove check, fix personio-client
        _ -> when (p ^. P.employeeCountry == Just countryFinland) $
            markErrorCell "HR Number is required for people working in Finland"

-------------------------------------------------------------------------------
-- Competence
-------------------------------------------------------------------------------

eqCompareCompetence :: Text -> Maybe Text -> Bool
-- TODO: empty competence should be an error too
eqCompareCompetence "" Nothing  = True
eqCompareCompetence _ Nothing   = False
eqCompareCompetence p (Just pm) = eq (T.toLower p) (T.toLower pm')
  where
    pm' = T.strip $ T.takeWhile (/= '(') pm

    eq x y | competenceMap ^. at x == Just y = True
    eq x y = x == y

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

formatDateSpan :: Maybe Day -> Maybe Day -> String
formatDateSpan s e =
    let s' = maybe "?" show s
        e' = maybe arrow (\x -> ndash ++ show x) e
    in s' ++ e'
  where
    ndash = "–"
    arrow = " →" -- space is intentional

noWrapSpan_ :: Monad m => HtmlT m () -> HtmlT m ()
noWrapSpan_ = span_ [ style_ "white-space: nowrap" ]

-------------------------------------------------------------------------------
-- Cells
-------------------------------------------------------------------------------

-- | Smaller state: better.
data CellState
    = CellOk
    | CellFixable (NonEmpty Text)     -- ^ Error which is mechanically fixable
    | CellPersonio (NonEmpty Text)    -- ^ personio data is inconsistent
    | CellInconsistent (NonEmpty Text) -- ^ Planmill and personio disagree
  deriving (Eq, Ord, Show)

cellStateErrors :: CellState -> [Text]
cellStateErrors CellOk = []
cellStateErrors (CellFixable xs) = toList xs
cellStateErrors (CellPersonio xs) = toList xs
cellStateErrors (CellInconsistent xs) = toList xs

append :: NonEmpty a -> [a] -> NonEmpty a
append (x :| xs) ys = x :| (xs ++ ys)

instance Semigroup CellState where
    CellInconsistent xs <> s = CellInconsistent (append xs $ cellStateErrors s)
    s <> CellInconsistent xs = CellInconsistent (append xs $ cellStateErrors s)
    CellPersonio xs <> s     = CellPersonio (append xs $ cellStateErrors s)
    s <> CellPersonio xs     = CellPersonio (append xs $ cellStateErrors s)
    CellFixable xs <> s      = CellFixable (append xs $ cellStateErrors s)
    s <> CellFixable xs      = CellFixable (append xs $ cellStateErrors s)
    CellOk <> CellOk         = CellOk

instance Monoid CellState where
    mempty = CellOk
    mappend = (<>)

markFixableCell :: MonadWriter CellState m => Text -> m ()
markFixableCell err = tell (CellFixable (err :| []))

markPersonioCell :: MonadWriter CellState m => Text -> m ()
markPersonioCell err = tell (CellPersonio (err :| []))

markErrorCell :: MonadWriter CellState m => Text -> m ()
markErrorCell err = tell (CellInconsistent (err :| []))

cell_ :: MonadWriter Any m => HtmlT (Writer CellState) () -> HtmlT m ()
cell_ html = case runWriter (commuteHtmlT html) of
    (html', CellOk)              -> td_ html'
    (html', CellFixable xs)      -> do
        tell (Any True)
        td_ [ style_ "background: #ccf; font-weight: bold", errorsTitle_ xs ] html'
    (html', CellPersonio xs)     -> do
        tell (Any True)
        td_ [ style_ "background: #ffc; font-weight: bold", errorsTitle_ xs ] html'
    (html', CellInconsistent xs) -> do
        tell (Any True)
        td_ [ style_ "background: #fcc; font-weight: bold", errorsTitle_ xs ] html'

errorsTitle_ :: NonEmpty Text -> Attribute
errorsTitle_ xs = title_ $ T.intercalate "; " $ toList xs

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l
