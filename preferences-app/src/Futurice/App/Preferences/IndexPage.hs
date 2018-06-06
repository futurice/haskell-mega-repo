{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Preferences.IndexPage where

import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.Preferences.API
import Futurice.App.Preferences.Markup
import Futurice.App.Preferences.Types

type API =
    SSOUser :> Get '[HTML] (HtmlPage "index")
    -- commands
    :<|> UpdatePrefPingSMS
    :<|> UpdatePrefPingEmail
    -- machine api:
    :<|> PreferencesAPI

type UpdatePrefPingSMS = SSOUser :> "update-ping-sms" :> Capture "value" Bool :> Post '[JSON] (CommandResponse ())

updatePrefPingSMS :: Proxy UpdatePrefPingSMS
updatePrefPingSMS = Proxy

type UpdatePrefPingEmail = SSOUser :> "update-ping-email" :> Capture "value" Bool :> Post '[JSON] (CommandResponse ())

updatePrefPingEmail :: Proxy UpdatePrefPingEmail
updatePrefPingEmail = Proxy

api :: Proxy API
api = Proxy

indexPage
    :: Preferences
    -> HtmlPage "index"
indexPage pref = page_ "Preferences" (Just NavHome) $ do
    h2_ "Missing hours notifications"
  
    let sms = pref ^. prefHoursPingSMS
    p_ $ do 
        "Receive missing hours notifications via SMS: "
        b_ $ if sms then "On" else "Off"

    button_
        [ data_ "futu-post-button" $ linkToText $ safeLink api updatePrefPingSMS (not sms)
        , class_ "button"
        , disabled_ "disabled"
        ] $
        "Turn " <> if sms then "Off" else "On"

    let email = pref ^. prefHoursPingEmail
    p_ $ do 
        "Receive missing hours notifications via Email: "
        b_ $ if email then "On" else "Off"

    button_
        [ data_ "futu-post-button" $ linkToText $ safeLink api updatePrefPingEmail (not email)
        , class_ "button"
        , disabled_ "disabled"
        ] $
        "Turn " <> if email then "Off" else "On"

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l
