{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Checklist.Types.World (
    World,
    emptyWorld,
    mkWorld,
    AuthCheck,
    -- * Lenses
    worldEmployees,
    worldTasks,
    worldLists,
    worldTaskItems,
    worldArchive,
    -- * Getters
    worldTaskItems',
    worldTasksSorted,
    worldTasksSortedByName,
    tasksSorted,
    -- * Counters
    toTodoCounter,
    taskItemtoTodoCounter,
    -- * Archive
    ArchivedEmployee (..),
    archiveEmployee,
    archiveTaskMap,
    Archive,
    ) where

-- import Futurice.Generics
import Control.Lens     (contains, filtered, minimumOf)
import Data.Functor.Rep (Representable (..))
import Futurice.Graph   (Graph)
import Futurice.IdMap   (IdMap)
import Futurice.Office
import Futurice.Prelude
import Prelude ()

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Set.Lens   as Set
import qualified Futurice.Graph  as Graph
import qualified Futurice.IdMap  as IdMap

import Futurice.App.Checklist.Types.Basic
import Futurice.App.Checklist.Types.ChecklistId
import Futurice.App.Checklist.Types.Counter
import Futurice.App.Checklist.Types.Identifier
import Futurice.App.Checklist.Types.TaskItem
import Futurice.App.Checklist.Types.TaskRole

{-
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Test.QuickCheck as QC
-}

import qualified FUM.Types.Login as FUM

-- | Primitive ACL. Given possible username, return the actual username, role and location.
type AuthCheck = Maybe FUM.Login -> Maybe (FUM.Login, TaskRole, Office)

data ArchivedEmployee = ArchivedEmployee
    { _archiveEmployee :: !Employee
    , _archiveTaskMap  :: !(Map (Identifier Task) TaskItem)
    }

makeLenses ''ArchivedEmployee

type Archive = Map (Identifier Employee) ArchivedEmployee

-- | World desribes the state of the db.
data World = World
    { _worldEmployees  :: !(IdMap Employee)
    , _worldTasks      :: !(Graph Task)
    , _worldLists      :: !(PerChecklist Checklist)
    , _worldTaskItems  :: !(Map (Identifier Employee) (Map (Identifier Task) AnnTaskItem))
    , _worldArchive    :: !Archive
      -- ^ ACL lookup
    -- lazy fields, updated on need when accessed
    , _worldTaskItems' :: Map (Identifier Task) (Map (Identifier Employee) AnnTaskItem)
      -- ^ isomorphic with 'worldTaskItems'
    , _worldTasksOrder  :: Map (Identifier Task) Int
      -- ^ task order changes rarely, so let's cache it.
    }

worldEmployees :: Lens' World (IdMap Employee)
worldEmployees f (World es ts ls is arc _ _) = f es <&>
    \x -> mkWorld x (Graph.toIdMap ts) ls is arc

worldTasks :: Lens' World (Graph Task)
worldTasks f (World es ts ls is arc _ _) = f ts <&>
    \x -> mkWorld es (Graph.toIdMap x) ls is arc

worldLists :: Lens' World (PerChecklist Checklist)
worldLists f (World es ts ls is arc _ _) = f ls <&>
    \x -> mkWorld es (Graph.toIdMap ts) x is arc

worldTaskItems :: Lens' World (Map (Identifier Employee) (Map (Identifier Task) AnnTaskItem))
worldTaskItems f (World es ts ls is arc _ _) = f is <&>
    \x -> mkWorld es (Graph.toIdMap ts) ls x arc

worldArchive :: Lens' World Archive
worldArchive f (World es ts ls is arc _ _) = f arc <&>
    \x -> mkWorld es (Graph.toIdMap ts) ls is x

worldTaskItems' :: Getter World (Map (Identifier Task) (Map (Identifier Employee) AnnTaskItem))
worldTaskItems' = getter _worldTaskItems'

worldTasksSorted :: TaskRole -> Getter World [Task]
worldTasksSorted tr = getter $ \world ->
    sortOn ((tr /=) . view taskRole) $
    view (tasksSorted world) $
    world ^.. worldTasks . folded

worldTasksSortedByName :: Getter World [Task]
worldTasksSortedByName = getter $ \world -> sortOn (view taskName) (world ^.. worldTasks . folded)

tasksSorted :: World -> Getter [Task] [Task]
tasksSorted world = getter $ sortOn metric where
    metric :: Task -> Maybe Int
    metric t = _worldTasksOrder world ^? ix (t ^. identifier)

orderLookup :: Graph Task -> Map (Identifier Task) Int
orderLookup tasks
    = Map.fromList
    $ flip zip [0 ..]
    $ map fst
    $ sortOn snd
    $ map (\t -> (t ^. identifier, closureMinWeight t))
    $ Graph.revTopSort tasks
  where
    closureMinWeight :: Task -> Integer
    closureMinWeight t = fromMaybe (t ^. taskOffset) $ minimumOf
        (_Just . folded . taskOffset)
        (Graph.revClosure tasks [t ^. identifier])

emptyWorld :: World
emptyWorld = mkWorld mempty mempty emptyChecklists mempty mempty where
    emptyChecklists :: PerChecklist Checklist
    emptyChecklists = tabulate $ \cid -> Checklist
        { _checklistId    = cid
        , _checklistTasks = mempty
        }

-- | Create world from employees, tasks, checklists, and items.
--
-- * Nubs entities by their natural key. /TOOD/
--
-- * Removes 'Task' dependencies to non-existing tasks.
--
-- * Removes non-existing 'Task's from checklists.
--
-- * Removes 'TaskItem's with non-existing employees or tasks.
--
mkWorld
    :: IdMap Employee
    -> IdMap Task
    -> PerChecklist Checklist
    -> Map (Identifier Employee) (Map (Identifier Task) AnnTaskItem)
    -> Archive
    -> World
mkWorld es ts ls is arc =
    let tids            = IdMap.keysSet ts
        -- Validation predicates
        validTid tid     = tids ^. contains tid

        -- Cleaned up inputs
        es' = es

        ts' = ts
            & IdMap.unsafeTraversal . taskPrereqs
            %~ Set.setOf (folded . filtered validTid)

        tsG = Graph.fromIdMap ts'

        ls' = ls
            & traverse . checklistTasks
            %~ Set.filter validTid

        -- TODO: validate is

        swappedIs = swapMapMap is
    in World es' tsG ls' is arc swappedIs (orderLookup tsG)

{-

TODO: AnnTaskItem

-- | Generates consistent worlds.
instance QC.Arbitrary World where
    arbitrary = do
        -- Generate raw data
        es <- QC.arbitrary
        ts <- QC.arbitrary

        let eids = IdMap.keysSet es
            tids = IdMap.keysSet ts
            tidGen = QC.elements (toList tids)

        let checklistItemGen = (,)
                <$> tidGen
                <*> QC.arbitrary

        checkListCount <- QC.choose (5, 10)
        cs <- fmap IdMap.fromFoldable . QC.vectorOf checkListCount $ Checklist
            <$> QC.arbitrary
            <*> QC.arbitrary
            <*> fmap Map.fromList (QC.listOf1 checklistItemGen)

        let cids = IdMap.keysSet cs
            cidGen = QC.elements (toList cids)

        -- Employees
        es' <- flip IdMap.unsafeTraversal es $ \employee -> do
            firstName   <- QC.elements ["Mikko", "Antti", "Ville", "Teemu", "Timo", "Anni", "Laura"]
            lastName    <- QC.elements ["Kikka", "Kukka", "Kukko", "Korhonen", "Virtanen", "Nieminen", "Laine"]
            cid         <- cidGen
            startingDay <- toEnum <$> QC.choose
                (fromEnum $(mkDay "2016-08-01"), fromEnum $(mkDay "2017-01-01"))
            pure $ employee
                & employeeChecklist   .~ cid
                & employeeFirstName   .~ firstName
                & employeeLastName    .~ lastName
                & employeeStartingDay .~ startingDay

        -- Tasks
        -- TODO: we can still generate cyclic tasks!
        ts' <- flip IdMap.unsafeTraversal ts $ \task -> do
            deps <- Set.fromList <$> QC.listOf tidGen
            pure $ task
                & taskPrereqs .~ deps

        -- AnnTaskItems
        -- For all eid, tid pair generate none, todo, done - value
        let is = [ (eid, tid) | eid <- eids ^.. folded , tid <- tids ^.. folded ]
        is' <- traverse (\p -> (,) p <$> QC.arbitrary) is
        let makeTaskItem ((eid, _tid), Nothing)  = (eid, Map.empty)
            makeTaskItem ((eid, tid), Just done) = (eid, Map.singleton tid done)
        let is'' = Map.fromListWith Map.union (map makeTaskItem is')

        -- World
        pure $ mkWorld es' ts' cs is''
-}

-------------------------------------------------------------------------------
-- Counters
-------------------------------------------------------------------------------

toTodoCounter :: World -> Identifier Task -> AnnTaskItem -> TodoCounter
toTodoCounter world tid td =
    case (world ^? worldTasks . ix tid . taskRole, td) of
        (Just role, AnnTaskItemDone {}) -> TodoCounter (Counter 1 1) $ mk role 1
        (Just role, AnnTaskItemTodo {}) -> TodoCounter (Counter 0 1) $ mk role 0
        (Nothing,   AnnTaskItemDone {}) -> TodoCounter (Counter 1 1) $ mempty
        (Nothing,   AnnTaskItemTodo {}) -> TodoCounter (Counter 0 1) $ mempty
  where
    mk :: TaskRole -> Int -> PerTaskRole Counter
    mk role value = tabulate $ \role' -> if role == role'
        then Counter value 1
        else mempty

taskItemtoTodoCounter :: World -> Identifier Task -> TaskItem -> TodoCounter
taskItemtoTodoCounter world tid td =
    case (world ^? worldTasks . ix tid . taskRole, td) of
        (Just role, TaskItemDone {}) -> TodoCounter (Counter 1 1) $ mk role 1
        (Just role, TaskItemTodo {}) -> TodoCounter (Counter 0 1) $ mk role 0
        (Nothing,   TaskItemDone {}) -> TodoCounter (Counter 1 1) $ mempty
        (Nothing,   TaskItemTodo {}) -> TodoCounter (Counter 0 1) $ mempty
  where
    mk :: TaskRole -> Int -> PerTaskRole Counter
    mk role value = tabulate $ \role' -> if role == role'
        then Counter value 1
        else mempty
