{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.TaskAppliance where

import Prelude ()
import Futurice.Prelude
import Algebra.Lattice
       (BoundedJoinSemiLattice (..), BoundedLattice,
       BoundedMeetSemiLattice (..), JoinSemiLattice (..), Lattice,
       MeetSemiLattice (..), joins1, meets1)
import Control.Applicative       (liftA2)
import Data.Aeson.Compat         (withText)
import Data.Functor.Foldable     (cata, embed)
import Data.Functor.Foldable.TH
import Futurice.Generics
import Futurice.Lucid.Foundation (HtmlT, ToHtml (..), class_, em_, span_)
import Text.Trifecta

import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Futurice.App.Checklist.Types.ContractType
import Futurice.App.Checklist.Types.Location

-- | Task appliance, e.g. this task is /"only for Helsinki and permanent employees"/.
--
-- /TODO;/ define my. maybe depend on 'Contract' and 'Location'.
data TaskAppliance
    = TAAll
    | TANot TaskAppliance
    | TAAnd TaskAppliance TaskAppliance
    | TAOr TaskAppliance TaskAppliance
    | TAContractType ContractType
    | TALocation Location
  deriving (Eq, Ord, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- recursion-schemes
-------------------------------------------------------------------------------

makeBaseFunctor ''TaskAppliance

-------------------------------------------------------------------------------
-- Lattice
-------------------------------------------------------------------------------

instance MeetSemiLattice TaskAppliance where
    TAAll /\ y     = y
    x     /\ TAAll = x
    x     /\ y     = TAAnd x y

instance JoinSemiLattice TaskAppliance where
    TAAll \/ _     = TAAll
    _     \/ TAAll = TAAll
    x     \/ y     = TAOr x y

instance BoundedJoinSemiLattice TaskAppliance where
    bottom = TANot TAAll

instance BoundedMeetSemiLattice TaskAppliance where
    top = TAAll

instance Lattice TaskAppliance
instance BoundedLattice TaskAppliance

-------------------------------------------------------------------------------
-- negation
-------------------------------------------------------------------------------

negateTaskAppliance :: TaskAppliance -> TaskAppliance
negateTaskAppliance (TANot ta) = ta
negateTaskAppliance ta         = TANot ta
{-
  where
    alg :: TaskApplianceF TaskAppliance -> TaskAppliance
    alg (TANotF ta)  = ta
    alg (TAAndF x y) = TAAnd x y
    alg (TAOrF  x y) = TAOr x y
    alg ta           = TANot (embed ta)
-}

-------------------------------------------------------------------------------
-- normalise
-------------------------------------------------------------------------------

normaliseTaskAppliance :: TaskAppliance -> TaskAppliance
normaliseTaskAppliance = cata alg
  where
    alg (TANotF ta)  = negateTaskAppliance ta
    alg (TAAndF x y) = x /\ y
    alg (TAOrF x y)  = x \/ y
    alg ta           = embed ta

-------------------------------------------------------------------------------
-- Predicate
-------------------------------------------------------------------------------

taskApplianceToPredicate :: TaskAppliance -> (ContractType, Location) -> Bool
taskApplianceToPredicate = cata alg
  where
    alg TAAllF               = const True
    alg (TANotF p)           = not . p
    alg (TAAndF p q)         = liftA2 (&&) p q
    alg (TAOrF p q)          = liftA2 (||) p q
    alg (TAContractTypeF ct) = (ct ==) . fst
    alg (TALocationF l)      = (l ==) . snd

-------------------------------------------------------------------------------
-- parse & pretty
-------------------------------------------------------------------------------

parseTaskAppliance :: Text -> Either String TaskAppliance
parseTaskAppliance = p . T.toLower . T.strip
  where
    p :: Text -> Either String TaskAppliance
    p ""    = Right TAAll
    p "all" = Right TAAll
    p t     = case parseByteString taP mempty (TE.encodeUtf8 t) of
        Success q -> Right q
        Failure e -> Left $ PP.displayS (PP.renderCompact  $ _errDoc e) ""

    taP = taP' <* eof
    taP' = orP

    orP       = fmap joins1 $ (:) <$> andP <*> many (symbol "or" *> andP)
    andP      = fmap meets1 $ (:) <$> notP <*> many (symbol "and" *> notP)
    notP      = foldl (\f _ -> f . negateTaskAppliance) id <$> many (symbol "not") <*> litP

    litP      =
            TAContractType <$> contractP
        <|> TALocation <$> locationP
        <|> TAAll <$ symbol "all"
        <|> parens taP'

    contractP :: Parser ContractType
    contractP = choice $
        (\ct -> ct <$ textSymbol (T.toLower $ contractTypeToText ct)) <$> [minBound .. maxBound]

    locationP :: Parser Location
    locationP = choice $
        (\l -> l <$ textSymbol (T.toLower $ locationToText l)) <$> [minBound .. maxBound]

prettyTaskAppliance :: TaskAppliance -> Text
prettyTaskAppliance = go 0
  where
    go :: Int -> TaskAppliance -> Text
    go _ (TAContractType ct) = contractTypeToText ct
    go _ (TALocation l)      = locationToText l
    go _ TAAll               = "all"
    go _ (TANot ta)          = "not " <> go 2 ta
    go d (TAAnd x y)         = pars (d >= 2) $ go 2 x <> " and " <> go 1 y
    go d (TAOr  x y)         = pars (d >= 1) $ go 1 x <> " or "  <> go 0 y

    pars True  t = "(" <> t <> ")"
    pars False t = t

instance ToHtml TaskAppliance where
    toHtmlRaw = toHtml
    toHtml = go 0
      where
        go :: Monad m => Int -> TaskAppliance -> HtmlT m ()
        go _ (TAContractType ct) = span_ [ class_ "contract" ] $ toHtml $ contractTypeToText ct
        go _ (TALocation l)      = span_ [ class_ "location" ] $ toHtml $ locationToText l
        go _ TAAll               = em_ "all"
        go _ (TANot ta)          = em_ "not " <> go 2 ta
        go d (TAAnd x y)         = pars (d >= 2) $ go 2 x <> " " <> em_ "and" <> " " <> go 1 y
        go d (TAOr  x y)         = pars (d >= 1) $ go 1 x <> " " <> em_ "or"  <> " " <> go 0 y

        pars True  t = "(" <> t <> ")"
        pars False t = t


-------------------------------------------------------------------------------
-- Instance
-------------------------------------------------------------------------------

deriveGeneric ''TaskAppliance

-- | Generated values are normalised
instance Arbitrary TaskAppliance where
    arbitrary = normaliseTaskAppliance <$> sopArbitrary
    shrink    = fmap normaliseTaskAppliance . sopShrink

instance ToJSON TaskAppliance where
    toJSON = toJSON . prettyTaskAppliance

instance FromJSON TaskAppliance where
    parseJSON = withText "TaskAppliance" $
        either fail pure . parseTaskAppliance
