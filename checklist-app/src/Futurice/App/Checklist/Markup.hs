{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Markup (
    -- * Structure
    checklistPage_,
    Nav (..),
    subheader_,
    -- * Futu id
    futuId_,
    futuForm_,
    -- * Link attributes
    linkToText,
    indexPageHref,
    tasksPageHref,
    checklistsPageHref,
    createTaskPageHref,
    createEmployeePageHref,
    checklistPageHref,
    taskPageHref,
    employeePageHref,
    applianceHelpHref,
    servicesHelpHref,
    -- * Links
    employeeLink,
    checklistLink,
    taskLink,
    -- * ToHtml
    nameHtml,
    nameText,
    roleHtml,
    contractTypeHtml,
    checklistNameHtml,
    locationHtml,
    -- * Counter
    TodoCounter (..),
    Counter (..),
    toTodoCounter,
    -- * Tasks
    taskCheckbox_,
    taskCommentInput_,
    taskInfo_,
    -- * Headers
    viewerItemsHeader,
    -- * Defaults
    defaultShowAll,
    ) where

import Control.Lens        (has, re, _Wrapped)
import FUM.Types.Login     (Login, loginToText)
import Futurice.Exit
import Futurice.Prelude
import Prelude ()
import Servant.Utils.Links (Link, safeLink)
import Web.HttpApiData     (toUrlPiece)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay  (pageParams)
import Futurice.App.Checklist.Types
import Futurice.Lucid.Foundation

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified GitHub    as GH
import qualified Personio  as P
import qualified PlanMill  as PM

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

data Nav
    = NavIndex
    | NavChecklists
    | NavTasks
    | NavCreateTask
    | NavPersonio
    | NavArchive
    | NavStats
    | NavHelp
    | NavCreateEmployee
  deriving (Eq, Enum, Bounded)

navLink :: Nav -> (Attribute, Text)
navLink NavIndex           = (indexPageHref Nothing Nothing (Nothing :: Maybe Task) defaultShowAll False, "Employees")
navLink NavChecklists      = (checklistsPageHref , "Checklists")
navLink NavTasks           = (tasksPageHref Nothing Nothing, "Tasks")
navLink NavCreateTask      = (createTaskPageHref , "Create Task")
navLink NavPersonio        = (personioPageHref , "Create from Personio")
navLink NavArchive         = (archivePageHref , "Archive")
navLink NavStats           = (statsPageHref, "Stats")
navLink NavHelp            = (servicesHelpHref, "Services")
navLink NavCreateEmployee  = (createEmployeePageHref , "Create raw")

checklistPage_ :: Text -> [Maybe Text] -> AuthUser -> Maybe Nav -> Html () -> HtmlPage sym
checklistPage_ title titleParts authUser nav body =
    page_ (title <> " - Checklist²" ) pageParams $ do
        navigation authUser nav
        div_ [ futuId_ "error-callout", class_ "callout alert", style_ "display: none" ] $ do
            div_ [ futuId_ "error-callout-content" ] $ pure ()
            button_ [ class_ "button" ] "Close"
        header_ (header title titleParts)
        row_ $ large_ 12 [ class_ "futu-block" ] body

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => AuthUser -> Maybe Nav -> HtmlT m ()
navigation (fu, viewerRole) nav' = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "menu horizontal" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Checklist"
                sup_ "2"
            li_ $ a_ [ id_ "futu-reload-indicator", href_ "#", style_ "display: none", title_ "You made changes, refresh page to show" ]  "1"
            for_ [minBound .. maxBound] $ \nav -> do
                let (aAttr, t) = navLink nav
                let liAttrs = if Just nav == nav' then [ class_ "futu-active" ] else []
                li_ liAttrs $ a_ [ aAttr ] $ toHtml t
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "menu" ] $ do
              li_ [ class_ "menu-text" ] $ do
                "Hello "
                toHtml (loginToText fu)
                ", you are "
                toHtml $ viewerRole ^. re _TaskRole

header
    :: Monad m
    => Text          -- ^ default title
    -> [Maybe Text]  -- ^ title parts
    -> HtmlT m ()
header title titleParts' = row_ $ large_ 12 $ header_ $ h1_ $ toHtml $
    if null titleParts
        then title
        else T.intercalate " - " titleParts
  where
    titleParts = catMaybes titleParts'

subheader_
    :: Monad m
    => Text
    -> HtmlT m ()
subheader_ title = row_ $ large_ 12 $ h2_ $ toHtml title

-------------------------------------------------------------------------------
-- Futu id
-------------------------------------------------------------------------------

futuId_ :: Text -> Attribute
futuId_ = data_ "futu-id"

futuForm_ :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
futuForm_ i attrs = row_ . large_ 12 . form_ (futuId_ i : attrs)

-------------------------------------------------------------------------------
-- Name helpers
-------------------------------------------------------------------------------

nameText :: HasName a => Getter a Text
nameText = name . _Wrapped

nameHtml :: (HasName a, Monad m) => Getter a (HtmlT m ())
nameHtml = nameText . getter toHtml

-------------------------------------------------------------------------------
-- Hrefs
-------------------------------------------------------------------------------

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

indexPageHref
    :: (HasIdentifier t Task)
    => Maybe Office -> Maybe ChecklistId -> Maybe t -> Bool -> Bool -> Attribute
indexPageHref mloc mlist mtask showDone showOld =
    href_ $ linkToText $ safeLink checklistApi indexPageEndpoint mloc
        mlist
        (mtask ^? _Just . identifier)
        showDone showOld

tasksPageHref :: Maybe TaskRole -> Maybe ChecklistId -> Attribute
tasksPageHref mrole mlist =
    href_ $ linkToText $ safeLink checklistApi tasksPageEndpoint mrole mlist

checklistsPageHref
    :: Attribute
checklistsPageHref =
    href_ $ linkToText $ safeLink checklistApi checklistsPageEndpoint

createTaskPageHref :: Attribute
createTaskPageHref =
    href_ $ linkToText $ safeLink checklistApi createTaskPageEndpoint

createEmployeePageHref :: Attribute
createEmployeePageHref =
    href_ $ linkToText $ safeLink checklistApi createEmployeePageEndpoint Nothing Nothing False

taskPageHref
    :: (HasIdentifier t Task)
    => t
    -> Attribute
taskPageHref t =
    href_ $ linkToText $ safeLink checklistApi taskPageEndpoint
        (t ^. identifier)

employeePageHref
    :: (HasIdentifier c Employee)
    => c
    -> Attribute
employeePageHref e =
    href_ $ linkToText $ safeLink checklistApi employeePageEndpoint
        (e ^. identifier)

checklistPageHref :: ChecklistId -> Attribute
checklistPageHref cid =
    href_ $ linkToText $ safeLink checklistApi checklistPageEndpoint cid

applianceHelpHref :: Attribute
applianceHelpHref = href_ $ linkToText $ safeLink checklistApi applianceHelpEndpoint

servicesHelpHref :: Attribute
servicesHelpHref = href_ $ linkToText $ safeLink checklistApi servicesHelpEndpoint

archivePageHref :: Attribute
archivePageHref = href_ $ linkToText $ safeLink checklistApi archivePageEndpoint

personioPageHref :: Attribute
personioPageHref = href_ $ linkToText $ safeLink checklistApi personioPageEndpoint

statsPageHref :: Attribute
statsPageHref = href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByActiveFuture False False

-------------------------------------------------------------------------------
-- Links
-------------------------------------------------------------------------------

employeeLink :: Monad m => Employee -> HtmlT m ()
employeeLink e = a_ [ employeePageHref e ] $ e ^. nameHtml

checklistLink :: Monad m => Checklist -> HtmlT m ()
checklistLink cl = a_ [ checklistPageHref $ cl ^. checklistId ] $ cl ^. nameHtml

taskLink :: Monad m => Task -> HtmlT m ()
taskLink task = a_ [ taskPageHref task ] $ task ^. nameHtml

-------------------------------------------------------------------------------
-- Miscs
-------------------------------------------------------------------------------

locationHtml :: Monad m => Maybe ChecklistId -> Office -> HtmlT m ()
locationHtml mlist o =
    a_ [ href, title_ $ officeToText o ] $ toHtml $ officeShortName o
  where
    href = indexPageHref (Just o) mlist (Nothing :: Maybe Task) False False

roleHtml :: Monad m => Maybe ChecklistId -> TaskRole -> HtmlT m ()
roleHtml mlist role = a_ [ href, title_ roleName ] $ toHtml $ roleName
  where
    roleName = role ^. re _TaskRole
    href = tasksPageHref (Just role) mlist

-- | Permamant status isn't shown, because it's common scenario: other contract
-- types stand up better.
contractTypeHtml :: Monad m => ContractType -> HtmlT m ()
contractTypeHtml ContractTypePermanent    = pure ()
contractTypeHtml ContractTypeExternal     = span_ [title_ "External"]      "Ext"
contractTypeHtml ContractTypeFixedTerm    = span_ [title_ "Fixed term"]    "Fix"
contractTypeHtml ContractTypePartTimer    = span_ [title_ "Part timer"]    "Part"
contractTypeHtml ContractTypeSummerWorker = span_ [title_ "Summer worker"] "Sum"

checklistNameHtml :: Monad m => Maybe Office -> ChecklistId -> Bool -> HtmlT m ()
checklistNameHtml mloc cid notDone =
    a_ [ indexPageHref mloc (Just cid) (Nothing :: Maybe Task) notDone False ] $
        toHtml $ cid ^. checklistIdName . _Wrapped

-------------------------------------------------------------------------------
-- TaskTags extra info
-------------------------------------------------------------------------------

isInPlanmillOrganizationHtml :: Monad m => Maybe PMUser -> Maybe Int -> [HtmlT m ()]
isInPlanmillOrganizationHtml planmillEmployee hrnumber =
    mcase hrnumber [] (\n -> ["HR number: " <> toHtml (show n)]) ++
    case planmillEmployee of
        Nothing ->
            [ b_ "Not " <> " in Planmill"
            -- TODO: Add more info so person can be added to PM
            ]
        Just (PMUser u passive) ->
            [ "In Planmill: " <> toHtml (u ^. PM.identifier)
            , "PM state: " <> toHtml passive
            ]

isInGithubOrganizationHtml :: Monad m => Maybe P.Employee -> Maybe GH.SimpleUser -> [HtmlT m ()]
isInGithubOrganizationHtml p g = return $ runExit $ do
    pEmployee  <- exitIfNothing p $
        "No Personio info found"
    githubUser <- exitIfNothing (pEmployee ^. P.employeeGithub) $
        "No GitHub username in Personio"
    _ <- exitIfNothing g $
        b_ "Not" <> " in Futurice GitHub organization" <> ": " <> toHtml githubUser
    return $ do
        b_ "In" <> " Futurice GitHub organization" <> ": " <> toHtml githubUser

showFirstContactInformationHtml :: Monad m => Maybe P.Employee -> [HtmlT m ()]
showFirstContactInformationHtml = maybe [ "No Personio info found" ] $ \p ->
    [ fromMaybe "No private email" $ do
        e <- p ^. P.employeeHomeEmail
        guard (not $ T.null e)
        return $ "Private email: " <> a_ [ href_ $ "mailto:" <> e ] (toHtml e)
    , case p ^. P.employeeJobOfferAccepted of
        Nothing -> "Has " <> b_ "not" <> " accepted job offer"
        Just d -> "Accepted job offer on " <> toHtml (show d)
    ]

hasFUMLoginHtml :: Monad m => Maybe FUM.Types.Login.Login -> [HtmlT m ()]
hasFUMLoginHtml login = case login of
    Nothing -> pure $ b_ "No" <> " FUM Login found in Personio or Checklist"
    Just l -> pure $ "FUM login: " <> toHtml l

taskInfo_
    :: Monad m
    => Task
    -> Employee
    -> IntegrationData
    -> HtmlT m ()
taskInfo_ task employee idata
    | null (task ^. taskTags) = pure ()
    | otherwise = unless (null infos) $ ul_ $ traverse_ li_ infos
  where
    zeroToNothing (Just 0) = Nothing
    zeroToNothing x        = x
    personioEmployee = (employee ^. employeePersonio) >>= (\x -> idata ^. personioData ^. at x)
    fumLogin = checklistLogin <|> personioLogin
      where
        checklistLogin = employee ^. employeeFUMLogin
        personioLogin = do
            p <- personioEmployee
            p ^. P.employeeLogin
    planmillEmployee = do
        login <- fumLogin
        snd <$> idata ^. planmillData ^. at login
    githubEmployee = do
        pe <- personioEmployee
        g <- pe ^. P.employeeGithub
        idata ^. githubData ^. at g
    employeeHRnumber = checklistHRNumber <|> personioHRNumber
      where
        checklistHRNumber = employee ^. employeeHRNumber
        personioHRNumber = personioEmployee >>= (\p -> zeroToNothing (p ^. P.employeeHRNumber))
    infos = foldMap info (task ^. taskTags)
    info GithubTask       = isInGithubOrganizationHtml personioEmployee githubEmployee
    info PlanmillTask     = isInPlanmillOrganizationHtml planmillEmployee employeeHRnumber
    info FirstContactTask = showFirstContactInformationHtml personioEmployee
    info FUMTask          = hasFUMLoginHtml fumLogin

-------------------------------------------------------------------------------
-- Tasks
-------------------------------------------------------------------------------

taskCheckbox_ :: Monad m => World -> Employee -> Task -> HtmlT m ()
taskCheckbox_ world employee task = do
    checkbox_ checked
        [ id_ megaid
        , futuId_ "task-done-checkbox"
        , data_ "futu-employee" $ employee ^. identifierText
        , data_ "futu-task" $ task ^. identifierText
        ]
    label_ [ attrfor_ megaid ] $ task ^. nameHtml
  where
    checked = flip has world
        $ worldTaskItems
        . ix (employee ^. identifier)
        . ix (task ^. identifier)
        . _AnnTaskItemDone

    megaid :: Text
    megaid =
        "task-checkbox-" <>
        employee ^. identifier . uuid . getter UUID.toText <>
        "_" <>
        task ^. identifier . uuid . getter UUID.toText

taskCommentInput_ :: Monad m => World -> Employee -> Task -> HtmlT m ()
taskCommentInput_ world employee task
    | task ^. taskComment = input_
        [ type_ "text "
        , futuId_ "task-comment-editbox"
        , data_ "futu-employee" $ employee ^. identifierText
        , data_ "futu-task" $ task ^. identifierText
        , value_ $ fromMaybe "" $ world ^? worldTaskItems . ix (employee ^. identifier) . ix (task ^. identifier) . annTaskItemComment
        ]
    | otherwise           = pure ()

-------------------------------------------------------------------------------
-- Headers
-------------------------------------------------------------------------------

viewerItemsHeader :: Monad m => TaskRole -> HtmlT m ()
viewerItemsHeader TaskRoleIT         = th_ [title_ "IT tasks todo/done"]          "IT items"
viewerItemsHeader TaskRoleHR         = th_ [title_ "HR tasks todo/done"]          "HR items"
viewerItemsHeader TaskRoleSupervisor = th_ [title_ "Supervisor tasks todo/done"]  "Supervisor items"

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

-- | If @show-all@ isn't specified, we assume it is.
--
-- /Note:/ Changing this to 'True' won't work, as the absence of flag == False.
defaultShowAll :: Bool
defaultShowAll = False
