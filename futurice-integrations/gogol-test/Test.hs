{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Futurice.Prelude
import Network.Google
import Network.Google.Auth
import Network.Google.People
import Prelude ()
import System.IO             (stdout)

main :: IO ()
main = do
    pure ()
    -- mgr <- newManager tlsManagerSettings

    -- -- todo: use fromFilePath
    -- cred <- serviceAccountUser (Just "ogre@futurice.com") <$> getApplicationDefault mgr
    -- case cred of
    --     FromAccount _ -> putStrLn "FromAccount"
    --     _ -> putStrLn "?"
    -- lgr <- newLogger Debug stdout
    -- env <- newEnvWith cred lgr mgr <&> (envScopes .~ contactsReadOnlyScope)
    -- runResourceT $ runGoogle env $ do
    --     x <- send (peopleConnectionsList "people/me")
    --     liftIO $ print x
