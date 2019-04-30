{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.GroupMatch where

import Algebra.Lattice
       (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..),
       joins1, meets1)
import Data.Aeson.Compat         (withText)
import Data.Functor.Foldable     (cata, embed)
import Data.Functor.Foldable.TH
import Futurice.Generics
import Futurice.Generics.SOP
import Futurice.Lucid.Foundation (HtmlT, ToHtml (..), class_, em_, span_)
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()
import Text.Trifecta

import qualified Data.Text as T
import qualified Personio  as P

data GroupMatch
    = GMAll
    | GMNot GroupMatch
    | GMAnd GroupMatch GroupMatch
    | GMOr GroupMatch GroupMatch
    | GMEmplType P.EmploymentType
    | GMOffice Office
    | GMTribe Tribe
  deriving (Eq, Ord, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- recursion-schemes
-------------------------------------------------------------------------------

makeBaseFunctor ''GroupMatch

-------------------------------------------------------------------------------
-- Lattice
-------------------------------------------------------------------------------

instance Lattice GroupMatch where
    GMAll       /\ y                       = y
    x           /\ GMAll                   = x
    GMNot GMAll /\ _                       = GMNot GMAll
    _           /\ GMNot GMAll             = GMNot GMAll
    x           /\ y           | x == y    = x
                               | otherwise = GMAnd x y

    GMAll       \/ _                       = GMAll
    _           \/ GMAll                   = GMAll
    GMNot GMAll \/ x                       = x
    x           \/ GMNot GMAll             = x
    x           \/ y           | x == y    = x
                               | otherwise = GMOr x y

instance BoundedJoinSemiLattice GroupMatch where
    bottom = GMNot GMAll

instance BoundedMeetSemiLattice GroupMatch where
    top = GMAll

-------------------------------------------------------------------------------
-- negation
-------------------------------------------------------------------------------

negateGroupMatch :: GroupMatch -> GroupMatch
negateGroupMatch (GMNot ta) = ta
negateGroupMatch ta         = GMNot ta

-------------------------------------------------------------------------------
-- normalise
-------------------------------------------------------------------------------

normaliseGroupMatch :: GroupMatch -> GroupMatch
normaliseGroupMatch = cata alg
  where
    alg (GMNotF ta)  = negateGroupMatch ta
    alg (GMAndF x y) = x /\ y
    alg (GMOrF x y)  = x \/ y
    alg ta           = embed ta

-------------------------------------------------------------------------------
-- Predicate
-------------------------------------------------------------------------------

groupMatchToPredicate :: GroupMatch -> Maybe P.EmploymentType -> Office -> Tribe -> Bool
groupMatchToPredicate gm e o t = cata alg gm
  where
    alg GMAllF           = True
    alg (GMNotF x)       = not x
    alg (GMAndF x y)     = x && y
    alg (GMOrF x y)      = x || y
    alg (GMEmplTypeF e') = e == Just e'
    alg (GMOfficeF o')   = o == o'
    alg (GMTribeF t')    = t == t'

-------------------------------------------------------------------------------
-- parse & pretty
-------------------------------------------------------------------------------

parseGroupMatch :: Text -> Either String GroupMatch
parseGroupMatch = p . T.toLower . T.strip
  where
    p :: Text -> Either String GroupMatch
    p "all"  = Right top
    p "none" = Right bottom
    p t      = case parseByteString taP mempty (encodeUtf8 t) of
        Success q -> Right q
        Failure e -> Left $ show $ _errDoc e

    taP = taP' <* eof
    taP' = orP

    orP       = fmap joins1 $ (:|) <$> andP <*> many (symbol "or" *> andP)
    andP      = fmap meets1 $ (:|) <$> notP <*> many (symbol "and" *> notP)
    notP      = foldl (\f _ -> f . negateGroupMatch) id <$> many (symbol "not") <*> litP

    litP      =
            GMEmplType <$> contractP
        <|> GMOffice <$> officeP
        <|> GMTribe <$>  tribeP
        <|> GMAll <$ symbol "all"
        <|> parens taP'

    contractP :: Parser P.EmploymentType
    contractP = choice $
        (\t -> t <$ textSymbol (T.toLower $ P.employmentTypeToText t)) <$> [minBound .. maxBound]

    officeP :: Parser Office
    officeP = choice $
        (\l -> l <$ textSymbol (T.toLower $ officeToText l)) <$> [minBound .. maxBound]

    tribeP :: Parser Tribe
    tribeP = choice $
        (\l -> l <$ textSymbol (T.toLower $ tribeToText l)) <$> [minBound .. maxBound]

prettyGroupMatch :: GroupMatch -> Text
prettyGroupMatch = go 0
  where
    go :: Int -> GroupMatch -> Text
    go _ (GMEmplType t) = P.employmentTypeToText t
    go _ (GMOffice o)   = officeToText o
    go _ (GMTribe t)    = tribeToText t
    go _ GMAll          = "all"
    go _ (GMNot GMAll)  = "none"
    go _ (GMNot ta)     = "not " <> go 2 ta
    go d (GMAnd x y)    = pars (d >= 2) $ go 2 x <> " and " <> go 1 y
    go d (GMOr  x y)    = pars (d >= 1) $ go 1 x <> " or "  <> go 0 y

    pars True  t = "(" <> t <> ")"
    pars False t = t

instance ToHtml GroupMatch where
    toHtmlRaw = toHtml
    toHtml = go 0
      where
        go :: Monad m => Int -> GroupMatch -> HtmlT m ()
        go _ (GMEmplType ct) = span_ [ class_ "employment" ] $ toHtml $ P.employmentTypeToText ct
        go _ (GMOffice o)    = span_ [ class_ "office" ]     $ toHtml $ officeToText o
        go _ (GMTribe t)     = span_ [ class_ "tribe"]       $ toHtml $ tribeToText t
        go _ GMAll           = em_ "all"
        go _ (GMNot GMAll)   = em_ "none"
        go _ (GMNot ta)      = em_ "not " <> go 2 ta
        go d (GMAnd x y)     = pars (d >= 2) $ go 2 x <> " " <> em_ "and" <> " " <> go 1 y
        go d (GMOr  x y)     = pars (d >= 1) $ go 1 x <> " " <> em_ "or"  <> " " <> go 0 y

        pars True  t = "(" <> t <> ")"
        pars False t = t

-------------------------------------------------------------------------------
-- Instance
-------------------------------------------------------------------------------

deriveGeneric ''GroupMatch

-- | Generated values are normalised
instance Arbitrary GroupMatch where
    arbitrary = normaliseGroupMatch <$> sopArbitrary
    shrink    = fmap normaliseGroupMatch . sopShrink

instance ToJSON GroupMatch where
    toJSON = toJSON . prettyGroupMatch

instance FromJSON GroupMatch where
    parseJSON = withText "GroupMatch" $
        either fail pure . parseGroupMatch

instance ToHttpApiData GroupMatch where
    toQueryParam = prettyGroupMatch

instance FromHttpApiData GroupMatch where
    parseQueryParam = first T.pack . parseGroupMatch

instance NFData GroupMatch
