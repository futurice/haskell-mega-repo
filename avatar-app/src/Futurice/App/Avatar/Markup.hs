{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Avatar.Markup (
    module Futurice.Lucid.Foundation,
    -- * Navigation
    page_,
    Nav (..),
    -- * HTML API
    HtmlRecord (..),
    HtmlAPI,
    htmlApi,
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), page_)
import Futurice.Prelude
import Futurice.Servant          (HTML, SSOUser)
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.Multipart

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

data HtmlRecord route = HtmlRecord
    { recIndex  :: route :- SSOUser :> Get '[HTML] (HtmlPage "index")
    , recUpload :: route :- "upload"
        :> SSOUser
        :> MultipartForm Mem (MultipartData Mem)
        :> Post '[HTML] (HtmlPage "index")
    }
  deriving Generic

type HtmlAPI = ToServantApi HtmlRecord

htmlApi :: Proxy HtmlAPI
htmlApi = genericApi (Proxy :: Proxy HtmlRecord)

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

data Nav
    = NavHome
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Avatar"

    navLink NavHome = (recordHref_ recIndex, "Avatar")
