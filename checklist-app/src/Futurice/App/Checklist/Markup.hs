{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Markup (
    -- * Structure
    checklistPage_,
    header,
    subheader_,
    -- * Futu id
    futuId_,
    futuForm_,
    -- * Link attributes
    linkToText,
    indexPageHref,
    tasksPageHref,
    checklistsPageHref,
    createChecklistPageHref,
    createTaskPageHref,
    createEmployeePageHref,
    checklistPageHref,
    taskPageHref,
    employeePageHref,
    applianceHelpHref,
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
    isInPlanmillOrganizationHtml,
    isInGithubOrganizationHtml,
    -- * Counter
    TodoCounter (..),
    Counter (..),
    toTodoCounter,
    -- * Tasks
    taskCheckbox_,
    taskCommentInput_,
    -- * Headers
    viewerItemsHeader,
    -- * Defaults
    defaultShowAll,
    ) where

import Control.Lens        (has, non, re, _Wrapped)
import Futurice.Prelude
import Prelude ()
import Servant.Utils.Links (Link, safeLink)
import Web.HttpApiData     (toUrlPiece)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay  (pageParams)
import Futurice.App.Checklist.Types
import Futurice.Lucid.Foundation
import GitHub                       (SimpleUser, simpleUserLogin)

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Personio  as P

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

checklistPage_ :: Text -> AuthUser -> Html () -> HtmlPage sym
checklistPage_ title authUser body =
    page_ (title <> " - Checklist²" ) pageParams $ do
        navigation authUser
        div_ [ futuId_ "error-callout", class_ "callout alert", style_ "display: none" ] $ do
            div_ [ futuId_ "error-callout-content" ] $ pure ()
            button_ [ class_ "button" ] "Close"
        body

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => AuthUser -> HtmlT m ()
navigation (fu, viewerRole) = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Checklist"
                sup_ "2"
            li_ $ a_ [ id_ "futu-reload-indicator", href_ "#", style_ "display: none", title_ "You made changes, refresh page to show" ]  "1"
            li_ $ a_ [ indexPageHref Nothing (Nothing :: Maybe Checklist) (Nothing :: Maybe Task) defaultShowAll False ] "Employees"
            li_ $ a_ [ checklistsPageHref ] "Checklists"
            li_ $ a_ [ tasksPageHref Nothing (Nothing :: Maybe Checklist) ] "Tasks"
            li_ $ a_ [ createChecklistPageHref ] "Create List"
            li_ $ a_ [ createTaskPageHref ] "Create Task"
            li_ $ a_ [ createEmployeePageHref ] "Create Employee"
            li_ $ a_ [ personioPageHref ] "... from Personio"
            li_ $ a_ [ archivePageHref ] "Archive"
        div_ [ class_ "top-bar-right" ] $ span_ [ class_ "menu-text" ] $ do
                "Hello "
                toHtml fu
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
    :: (HasIdentifier c Checklist, HasIdentifier t Task)
    => Maybe Office -> Maybe c -> Maybe t -> Bool -> Bool -> Attribute
indexPageHref mloc mlist mtask showDone showOld =
    href_ $ linkToText $ safeLink checklistApi indexPageEndpoint mloc
        (mlist ^? _Just . identifier)
        (mtask ^? _Just . identifier)
        showDone showOld

tasksPageHref
    :: (HasIdentifier c Checklist)
    => Maybe TaskRole -> Maybe c -> Attribute
tasksPageHref mrole mlist =
    href_ $ linkToText $ safeLink checklistApi tasksPageEndpoint mrole
        (mlist ^? _Just . identifier)

checklistsPageHref
    :: Attribute
checklistsPageHref =
    href_ $ linkToText $ safeLink checklistApi checklistsPageEndpoint

createChecklistPageHref :: Attribute
createChecklistPageHref =
    href_ $ linkToText $ safeLink checklistApi createChecklistPageEndpoint

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

checklistPageHref
    :: (HasIdentifier c Checklist)
    => c
    -> Attribute
checklistPageHref l =
    href_ $ linkToText $ safeLink checklistApi checklistPageEndpoint
        (l ^. identifier)

applianceHelpHref :: Attribute
applianceHelpHref = href_ $ linkToText $ safeLink checklistApi applianceHelpEndpoint

archivePageHref :: Attribute
archivePageHref = href_ $ linkToText $ safeLink checklistApi archivePageEndpoint

personioPageHref :: Attribute
personioPageHref = href_ $ linkToText $ safeLink checklistApi personioPageEndpoint

-------------------------------------------------------------------------------
-- Links
-------------------------------------------------------------------------------

employeeLink :: Monad m => Employee -> HtmlT m ()
employeeLink e = a_ [ employeePageHref e ] $ e ^. nameHtml

checklistLink :: Monad m => Checklist -> HtmlT m ()
checklistLink cl = a_ [ checklistPageHref cl ] $ cl ^. nameHtml

taskLink :: Monad m => Task -> HtmlT m ()
taskLink task = a_ [ taskPageHref task ] $ task ^. nameHtml

-------------------------------------------------------------------------------
-- Miscs
-------------------------------------------------------------------------------

locationHtml
    :: (Monad m, HasIdentifier c Checklist)
    => Maybe c -> Office -> HtmlT m ()
locationHtml mlist l = a_ [ href, title_ locName ] $ locSlug
  where
    href = indexPageHref (Just l) mlist (Nothing :: Maybe Task) False False

    locSlug = case l of
        OffHelsinki  -> "Hel"
        OffTampere   -> "Tre"
        OffBerlin    -> "Ber"
        OffLondon    -> "Lon"
        OffStockholm -> "Sto"
        OffMunich    -> "Mun"
        OffOther     -> "Oth"
    locName = case l of
        OffHelsinki  -> "Helsnki"
        OffTampere   -> "Tampere"
        OffBerlin    -> "Berlin"
        OffLondon    -> "London"
        OffStockholm -> "Stockholm"
        OffMunich    -> "Munich"
        OffOther     -> "Other"

roleHtml
    :: (Monad m, HasIdentifier c Checklist)
    => Maybe c -> TaskRole -> HtmlT m ()
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

-- | TODO: better error
checklistNameHtml :: Monad m => World -> Maybe Office -> Identifier Checklist -> Bool -> HtmlT m ()
checklistNameHtml world mloc i notDone =
    a_ [ indexPageHref mloc (Just i) (Nothing :: Maybe Task) notDone False ] $
        world ^. worldLists . at i . non (error "Inconsisten world") . nameHtml

isInPlanmillOrganizationHtml :: Maybe PMUser -> Maybe Int -> HtmlT Identity ()
isInPlanmillOrganizationHtml planmillEmployee hrnumber = let hntext = maybe "" (\number -> ", Personio HR number: " <> toHtml (show number)) hrnumber in
                                                           case planmillEmployee of
                                                             Nothing -> span_ [class_ "info label"] "Person not in Planmill" <> hntext
                                                             (Just (PMUser _ passive)) ->
                                                                 span_ [class_ "info label"] $ "In Planmill, state: " <> toHtml passive <> hntext

isInGithubOrganizationHtml :: Maybe P.Employee -> Vector SimpleUser -> HtmlT Identity ()
isInGithubOrganizationHtml p gs = case p of
  Nothing -> span_ [class_ "info label"] "No personio info found"
  Just pEmployee -> case pEmployee ^. P.employeeGithub of
    Nothing -> span_ [class_ "info label"]  "No Github username in personio"
    Just githubUser -> case listToMaybe $ filter (\g  -> simpleUserLogin g == githubUser) (toList gs) of
      Nothing -> span_ $ do
          span_ [class_ "info label"] "Not in Futurice Github organization"
          " Username "
          toHtml githubUser
      Just _ -> span_ $ do
          span_ [class_ "info label"] "In Futurice Github organization"
          " Username "
          toHtml githubUser

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
