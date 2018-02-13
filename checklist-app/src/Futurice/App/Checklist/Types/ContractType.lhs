\begin{code}
module Futurice.App.Checklist.Types.ContractType (
    ContractType (..),
    -- * Prisms
    _ContractTypePermanent,
    _ContractTypeExternal,
    _ContractTypeFixedTerm,
    _ContractTypePartTimer,
    _ContractTypeSummerWorker,
    _ContractType,
    -- * Conversion functions
    contractTypeToText,
    contractTypeFromText,
    ) where
\end{code}

\imports

\begin{code}
import Futurice.Lucid.Generics   (FieldToHtml)

-- | Contract type affect what's need to be done.
data ContractType
    = ContractTypePermanent
    | ContractTypeExternal
    | ContractTypeFixedTerm
    | ContractTypePartTimer
    | ContractTypeSummerWorker

instance FieldToHtml ContractType
\end{code}

\enum{ContractType}{"permanent","external","fixed-term","part-timer","summer-worker"}{}
