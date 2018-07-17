{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Proxy.Markup where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), page_)
import Futurice.Prelude
import Futurice.Servant          (HTML)
import Prelude ()
import Servant
import Servant.API.Generic
import Servant.Server.Generic

newtype HtmlRecord route = HtmlRecord
    { recIndex  :: route :- Get '[HTML] (HtmlPage "index")
    }
  deriving Generic

type HtmlAPI = ToServantApi HtmlRecord

htmlApi :: Proxy HtmlAPI
htmlApi = genericApi (Proxy :: Proxy HtmlRecord)

htmlServer :: a -> Server HtmlAPI
htmlServer _ = genericServer $ HtmlRecord
    { recIndex = return indexPage
    }

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

data Nav = NavIndex
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Prox"
    navLink NavIndex = (recordHref_ recIndex, "Proxy")

-------------------------------------------------------------------------------
-- Markup
-------------------------------------------------------------------------------

indexPage ::  HtmlPage "index"
indexPage = page_ mempty (Just NavIndex) $ do
    a_ [ href_ "/swagger-ui/" ] "Swagger UI"
