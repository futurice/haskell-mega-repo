{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.Lucid.Generics (
    sopToHtml,
    FieldToHtml (..),
    ToHtml (..),
    ) where

import FUM.Types.Login           (Login)
import Futurice.Generics.SOP
import Futurice.Lucid.Foundation
import Futurice.Office           (Office)
import Futurice.Prelude          hiding (from)
import Futurice.Tribe            (Tribe)
import Generics.SOP
       (All, FieldInfo (..), HasDatatypeInfo, IsProductType, from)
import Generics.SOP.Lens         (nsSingleton, sop)
import Prelude ()

sopToHtml
    :: forall a xs m.
       (IsProductType a xs, HasDatatypeInfo a, All FieldToHtml xs, Monad m)
    => a -> HtmlT m ()
sopToHtml x = table_
    $ sopToHtml' (longestFieldPrefix p) (fieldInfos p)
    $ from x ^. sop . nsSingleton
  where
    p = Proxy :: Proxy a

sopToHtml'
    :: forall m xs. (Monad m, All FieldToHtml xs)
    => String -> NP FieldInfo xs -> NP I xs
    -> HtmlT m ()
sopToHtml' prefix = go
  where
    go :: All FieldToHtml ys => NP FieldInfo ys -> NP I ys -> HtmlT m ()
    go Nil Nil = pure ()
    go (FieldInfo f :* fs) (I x :* xs) = do
        vertRow_ (processFieldName prefix f ^. packed) $ fieldToHtml x
        go fs xs
#if __GLASGOW_HASKELL__ < 800
    go _ _ = error "sopToHtml' go: impossible happened"
#endif

-------------------------------------------------------------------------------
-- FieldToHtml
-------------------------------------------------------------------------------

class FieldToHtml a where
    fieldToHtml :: Monad m => a -> HtmlT m ()
    default fieldToHtml :: (ToHtml a, Monad m) => a -> HtmlT m ()
    fieldToHtml = toHtml

instance FieldToHtml Text
instance FieldToHtml Login
instance FieldToHtml Office
instance FieldToHtml Tribe

instance FieldToHtml Bool where fieldToHtml = toHtml . show
instance FieldToHtml Day where fieldToHtml = toHtml . show
instance FieldToHtml Int where fieldToHtml = toHtml . show

instance FieldToHtml a => FieldToHtml (Maybe a) where
    fieldToHtml Nothing  = em_ (fromString "Nothing")
    fieldToHtml (Just x) = fieldToHtml x

instance FieldToHtml a => FieldToHtml (Identity a) where
    fieldToHtml = traverse_ fieldToHtml
