{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.Badge.Templates (
    employeeTemplate,
    externalTemplate,
    ) where

import Codec.Picture       (DynamicImage (ImageRGB8), Image, PixelRGB8)
import Diagrams.Prelude
import Futurice.Prelude
import Prelude ()

import Futurice.App.Badge.Tools

employeeTemplate
    :: forall m b.
       ( MonadReadOnlyFS m, MonadThrow m
       , Renderable (Path V2 Double) b
       , Renderable (DImage Double Embedded) b
       )
    => Image PixelRGB8  -- ^ image
    -> String           -- ^ first name
    -> String           -- ^ last name
    -> m (QDiagram b V2 Double Any)
employeeTemplate = error "hidden"

externalTemplate
    :: forall m b.
       ( MonadReadOnlyFS m, MonadThrow m
       , Renderable (Path V2 Double) b
       , Renderable (DImage Double Embedded) b
       )
    => Image PixelRGB8  -- ^ image
    -> String           -- ^ first name
    -> String           -- ^ last name
    -> m (QDiagram b V2 Double Any)
externalTemplate = error "hidden"
