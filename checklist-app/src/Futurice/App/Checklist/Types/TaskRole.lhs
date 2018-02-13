\begin{code}
module Futurice.App.Checklist.Types.TaskRole (
    TaskRole (..),
    -- * Prisms
    _TaskRole,
    _TaskRoleIT,
    _TaskRoleHR,
    _TaskRoleSupervisor,
    -- * Conversion functions
    taskRoleToText,
    taskRoleFromText,
    -- * Representable container
    PerTaskRole (..),
    ) where
\end{code}

\imports

\begin{code}
import Control.Lens      (Index, IxValue, Ixed (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep  (Representable (..), distributeRep, liftR2, pureRep)
\end{code}

\begin{code}
-- | "Owners" of the tasks
data TaskRole
    = TaskRoleIT
    | TaskRoleHR
    | TaskRoleSupervisor
\end{code}

\enum{TaskRole}{"IT","HR","Supervisor"}{}

\begin{code}
-------------------------------------------------------------------------------
-- PerTaskRole container
-------------------------------------------------------------------------------

data PerTaskRole a = PerTaskRole !a !a !a
  deriving (Functor)

instance NFData a => NFData (PerTaskRole a) where
    rnf (PerTaskRole x y z) =
        rnf x `seq` rnf y `seq` rnf z

type instance Index (PerTaskRole a) = TaskRole
type instance IxValue (PerTaskRole a) = a

instance Ixed (PerTaskRole a) where
    ix TaskRoleIT         f (PerTaskRole x y z) = (\a -> PerTaskRole a y z) <$> f x
    ix TaskRoleHR         f (PerTaskRole x y z) = (\a -> PerTaskRole x a z) <$> f y
    ix TaskRoleSupervisor f (PerTaskRole x y z) = (\a -> PerTaskRole x y a) <$> f z

instance Semigroup a => Semigroup (PerTaskRole a) where
    (<>) = liftR2 (<>)

instance Monoid a => Monoid (PerTaskRole a) where
    mempty = pureRep mempty
    mappend = liftR2 mappend

instance Distributive PerTaskRole where
    distribute = distributeRep

instance Representable PerTaskRole where
    type Rep PerTaskRole = TaskRole

    tabulate f = PerTaskRole
        (f TaskRoleIT)
        (f TaskRoleHR)
        (f TaskRoleSupervisor)

    index (PerTaskRole x _ _) TaskRoleIT = x
    index (PerTaskRole _ x _) TaskRoleHR = x
    index (PerTaskRole _ _ x) TaskRoleSupervisor = x
\end{code}
