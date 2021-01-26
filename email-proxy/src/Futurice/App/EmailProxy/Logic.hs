{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.EmailProxy.Logic (sendEmail) where

import Data.Aeson       (object, (.=))
import Futurice.Prelude
import Prelude ()
import Servant          (NoContent (..))

import qualified Network.AWS               as AWS
import qualified Network.AWS.SES.SendEmail as AWS
import qualified Network.AWS.SES.Types     as AWS

import Futurice.App.EmailProxy.Ctx
import Futurice.App.EmailProxy.Types

sendEmail :: (MonadIO m, MonadLog m) => Ctx -> EmailType -> Req -> m NoContent
sendEmail ctx emailType req = do
    let env = ctxAwsEnv ctx

    logInfo "Sending email" $ req
        & reqBody .~ "<redacted>"

    let destination = AWS.destination
          & AWS.dToAddresses  .~ (req ^.. reqTo . folded . _EmailAddress)
          & AWS.dCCAddresses  .~ (req ^.. reqCc . folded . folded . _EmailAddress)
          & AWS.dBCCAddresses .~ (req ^.. reqBcc . folded . folded . _EmailAddress)
    let subject = AWS.content (req ^. reqSubject)
          & AWS.cCharset ?~ "UTF-8"
    let content = AWS.content (req ^. reqBody)
          & AWS.cCharset ?~ "UTF-8"
    let message = AWS.message subject (AWS.body & AWS.bText ?~ content)
    let message' =
            case (emailType, req ^. reqHtmlBody) of
              (HtmlEmail, Just b) ->
                  let content' = AWS.content b & AWS.cCharset ?~ "UTF-8"
                  in message & AWS.mBody . AWS.bHTML ?~ content'
              _ -> message

    -- Sender address is hardcoded, as this service is used by machines.
    res <- liftIO $ AWS.runResourceT $ AWS.runAWS env $ do
        AWS.send $ AWS.sendEmail "no-reply@futurice.tech" destination message'

    logInfo "AWS Response" $ object
        [ "messageId" .= (res ^. AWS.sersMessageId)
        , "status"    .= (res ^. AWS.sersResponseStatus)
        ]

    pure NoContent
