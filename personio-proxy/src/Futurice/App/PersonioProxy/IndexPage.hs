{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.PersonioProxy.IndexPage (indexPage) where

import Data.Aeson (Value (..))
import FUM.Types.Login
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), pageParamsWithJS, page_)
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe
import Futurice.IdMap (IdMap)
import Prelude ()

import qualified Data.Aeson as Aeson
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified GitHub     as GH
import qualified Personio   as P

data Nav = NavHome deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Personio Proxy"

    navLink NavHome = (href_ "/", "Personio Proxy")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "personio-proxy.js" >>= embedJS)

indexPage :: IdMap P.Employee -> HtmlPage "index"
indexPage ps = page_ "Personio Proxy" (Just NavHome) $ do
    fullRow_ $
        input_ [ id_ "cmd", type_ "text", placeholder_ "type :help to show help", autofocus_ ]

    div_ [ id_ "help", class_ "row", style_ "display: none" ] $ div_ [ class_ "columns" ] $ do
        h3_ "Keywords"

        ul_ $ do
            li_ $ do
                "Status "
                code_ ":active"
                " "
                code_ ":inactive"
                " "
                code_ ":leave"
                " "
                code_ ":onboarding"

            li_ $ do
                "Employee types "
                code_ ":internal"
                " "
                code_ ":external"

            li_ $ do
                "Tribes"
                for_ [ minBound .. maxBound ] $ \tribe -> " " <> code_ (toHtml $ toKeyword (tribeToText tribe))

            li_ $ do
                "Offices"
                for_ [ minBound .. maxBound ] $ \tribe -> " " <> code_ (toHtml $ toKeyword (officeToText tribe))

        h3_ "Notes"

        ul_ $ do
            li_ $ "TAB completion for keywords maybe works too"
            li_ $ "free words searches for names, login, github, phone..."

        h3_ "Examples"

        ul_ $ do
            li_ $ code_ ":active :internal" >> " Active employees"
            li_ $ code_ "Kimmo :helsinki" >> " Kimmos in Helsinki"

    div_ [ id_ "single", class_ "row" ] $ do
        div_ [ class_ "columns large-12" ] $ termWith "h2" [ id_ "s-fullname" ] "???"

        div_ [ class_ "columns medium-6" ] $ do
            h3_ "Public profile"

            table_ $ do
                tr_ $ td_ "Email"    >> td_ [ id_ "s-email" ] "???"
                tr_ $ td_ "Office"   >> td_ [ id_ "s-office" ] "???"
                tr_ $ td_ "Tribe"    >> td_ [ id_ "s-tribe" ] "???"
                tr_ $ td_ "Position" >> td_ [ id_ "s-position" ] "???"
                tr_ $ td_ "Country"  >> td_ [ id_ "s-country" ] "???"
                tr_ $ td_ "Employer" >> td_ [ id_ "s-employer" ] "???"
                tr_ $ td_ "Phone"    >> td_ [ id_ "s-phone" ] "???"
                tr_ $ td_ "Role"     >> td_ [ id_ "s-role" ] "???"

            h3_ "User accounts"

            table_ $ do
                tr_ $ td_ "Login"    >> td_ [ id_ "s-login" ] "???"
                tr_ $ td_ "GitHub"   >> td_ [ id_ "s-github" ] "???"
                tr_ $ td_ "Flowdock" >> td_ [ id_ "s-flowdock" ] "???"

        div_ [ class_ "columns medium-6" ] $ do
            h3_ "HR information"

            table_ $ do
                tr_ $ td_ "Status"        >> td_ [ id_ "s-status" ] "???"
                tr_ $ td_ "Employment"    >> td_ [ id_ "s-employment-type" ] "???"
                tr_ $ td_ "Hire date"     >> td_ [ id_ "s-hire-date" ] "???"
                tr_ $ td_ "Contract ends" >> td_ [ id_ "s-contract-ends" ] "???"
                tr_ $ td_ "Cost center"   >> td_ [ id_ "s-cost-center" ] "???"
                tr_ $ td_ "Supervisor"    >> td_ [ id_ "s-supervisor" ] "???"
                tr_ $ td_ "Weekly hours"  >> td_ [ id_ "s-weekly-hours" ] "???"
                tr_ $ td_ "Contract type" >> td_ [ id_ "s-contract-type" ] "???"
                tr_ $ td_ "Salary type"   >> td_ [ id_ "s-salary-type" ] "???"
                tr_ $ td_ "HR Number"     >> td_ [ id_ "s-hr-number" ] "???"
                tr_ $ td_ "Expat"         >> td_ [ id_ "s-expat" ] "???"

    div_ [ id_ "table", class_ "row" ] $ div_ [ class_ "columns" ] $ table_ $ do
        thead_ $ do
            th_ "#"
            th_ "Login"
            th_ "Name"
            th_ "Tribe"
            th_ "Office"
            th_ "Hire"
            th_ "End"
            th_ "Status"
            th_ "Type"

        tbody_ $ for_ (sortOn (view P.employeeLast) (toList ps)) $ \e -> do
            let addSupervisor :: Value -> Value
                addSupervisor (Object hm)
                    | Just supervisorName <-
                        e ^. P.employeeSupervisorId >>= \sid -> ps ^? ix sid . P.employeeFullname
                    = Object $ hm & at "supervisor" ?~ String supervisorName
                addSupervisor value = value

            tr_
                [ data_ "futu-keywords" $ decodeUtf8Lenient $ view strict $ Aeson.encode $ employeeKeywords e
                , data_ "futu-words"    $ decodeUtf8Lenient $ view strict $ Aeson.encode $ employeeWords e
                , data_ "futu-employee" $ decodeUtf8Lenient $ view strict $ Aeson.encode $ addSupervisor $ Aeson.toJSON e
                ] $ do
            ----------
                td_ $ toHtml $ e ^. P.employeeId
                td_ $ traverse_ toHtml $ e ^. P.employeeLogin
                td_ $ toHtml $ e ^. P.employeeFullname
                td_ $ toHtml $ e ^. P.employeeTribe
                td_ $ toHtml $ e ^. P.employeeOffice
                td_ $ traverse_ (toHtml . show) $ e ^. P.employeeHireDate
                td_ $ traverse_ (toHtml . show) $ e ^. P.employeeEndDate
                td_ $ toHtml $ e ^. P.employeeStatus
                td_ $ traverse_ toHtml $ e ^. P.employeeEmploymentType

employeeWords :: P.Employee -> Set Text
employeeWords p = Set.map T.toLower $ mconcat
    [ Set.singleton $ p ^. P.employeeFirst
    , Set.singleton $ p ^. P.employeeLast
    , maybe Set.empty Set.singleton $ p ^? P.employeeGithub . _Just . getter GH.untagName
    , maybe Set.empty Set.singleton $ p ^? P.employeeLogin . _Just . getter loginToText
    , maybe Set.empty Set.singleton $ p ^. P.employeeWorkPhone
    ]

employeeKeywords :: P.Employee -> Set Text
employeeKeywords p = Set.map toKeyword $ mconcat
    [ Set.singleton status
    , estatus
    , Set.singleton office
    , Set.singleton tribe
    ]
  where
    status = case p ^. P.employeeStatus of
        P.Active     -> "active"
        P.Inactive   -> "inactive"
        P.Leave      -> "leave"
        P.Onboarding -> "onboarding"

    estatus = case p ^. P.employeeEmploymentType of
        Just P.Internal -> Set.singleton "internal"
        Just P.External -> Set.singleton "external"
        Nothing         -> mempty

    office = p ^. P.employeeOffice . getter officeToText
    tribe  = p ^. P.employeeTribe . getter tribeToText

toKeyword :: T.Text -> T.Text
toKeyword = (":" <>) . T.replace " " "-" . T.toLower
