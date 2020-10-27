{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.MegaRepoTool.Config where

import Control.Lens
import Control.Monad.Catch       (handleIOError)
import Control.Monad.Trans.State (StateT, execStateT)
import Futurice.Prelude
import Prelude ()

import qualified Data.ByteString     as BS
import qualified Data.Map            as Map
import qualified Data.Text           as T
import qualified Distribution.Fields as P
import qualified Distribution.Parsec as P
import qualified Text.Microstache    as M

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type AppName = Text
type DebName = Text

data ImageDefinition = ImageDefinition
    { _idDockerImage :: !Text
    , _idExecutable  :: !Text
    , _idRestart     :: !(Maybe AppName)
    }
  deriving (Show, Generic)

instance AnsiPretty ImageDefinition
makeLenses ''ImageDefinition

data LambdaDefinition = LambdaDefinition
    { _ldExecutable :: !Text
    }
  deriving (Show, Generic)

instance AnsiPretty LambdaDefinition
makeLenses ''LambdaDefinition

emptyLambdaDefinition :: LambdaDefinition
emptyLambdaDefinition = LambdaDefinition ""

data MRTConfig' a = MRTConfig
    { _mrtDockerBaseImage :: !Text
    , _mrtApps            :: !(Map AppName ImageDefinition)
    , _mrtLambdas         :: !(Map AppName LambdaDefinition)
    , _mrtDebs            :: [DebName]
    , _mrtDockerfileTmpl  :: a
    , _mrtEnvVars         :: !(Map Text (Either Text Text))
    }
  deriving (Show, Generic, Functor)

type MRTConfig = MRTConfig' M.Template

emptyMRTConfig :: MRTConfig
emptyMRTConfig = MRTConfig "" mempty mempty mempty emptyTemplate mempty

emptyTemplate :: M.Template
emptyTemplate = M.Template "empty" $ Map.singleton "empty" []

instance AnsiPretty a => AnsiPretty (MRTConfig' a)
makeLenses ''MRTConfig'

-------------------------------------------------------------------------------
-- Parse
-------------------------------------------------------------------------------

readConfig :: IO MRTConfig
readConfig = do
    let config0 = emptyMRTConfig

    contents1 <- handleIOError (const $ pure "") $ BS.readFile "./mega-repo-tool.config"
    config1 <- either fail pure $ parseConfig contents1 config0

    contents2 <- handleIOError (const $ pure "") $ BS.readFile "./mega-repo-tool.config.local"
    either fail pure $ parseConfig contents2 config1

parseConfig :: BS.ByteString -> MRTConfig -> Either String MRTConfig
parseConfig contents config = do
    fs <- first show $ P.readFields contents
    execStateT (traverse field fs) config
  where
    field :: P.Field P.Position -> StateT MRTConfig (Either String) ()
    field (P.Field (P.Name pos name) fls)
        | name == "dockerfile-base-image" =
            mrtDockerBaseImage .= T.strip fls'
        | name == "dockerfile-template" = do
            t <- lift $ first show $ M.compileMustacheText "<input>" (fls' ^. lazy)
            mrtDockerfileTmpl .= t
        | name == "deb-packages" = do
            let pkgs = map (T.strip . decodeUtf8Lenient . fieldLineContents) fls
            mrtDebs %= (++ pkgs)

        | otherwise =
            throwError $ "unknown field " ++ show name ++ " at " ++ P.showPos pos
      where
        fls' = T.unlines $ map (decodeUtf8Lenient . fieldLineContents) fls

    field (P.Section (P.Name pos name) args fs)
        | name == "application" = do
            appName <- parseAppName args
            imageDefinition <- execStateT (traverse application fs) (ImageDefinition "" "" Nothing)
            mrtApps . at appName ?= imageDefinition

        | name == "aws-lambda" = do
            lambdaName <- parseAppName args
            lambdaDefinition <- execStateT (traverse lambda fs) emptyLambdaDefinition
            mrtLambdas . at lambdaName ?= lambdaDefinition

        | name == "environment-variables" = do
            envVars <- Map.fromList <$>  traverse parseEnvVar fs
            mrtEnvVars %= (envVars <>)

        | otherwise =
            lift $ Left $ "unknown section " ++ show name ++ " at " ++ P.showPos pos

    application (P.Section (P.Name pos name) _ _) =
        throwError $ "unexpected sub-section in application section " ++ show name ++ " at " ++ P.showPos pos
    application (P.Field (P.Name pos name) fls)
        | name == "docker"     = idDockerImage .=  fls'
        | name == "executable" = idExecutable .=  fls'
        | name == "restart"    = idRestart ?= fls'
        | otherwise =
            throwError $ "unknown application field " ++ show name ++ " at " ++ P.showPos pos
      where
        fls' = T.strip $ decodeUtf8Lenient $ foldMap fieldLineContents fls

    parseAppName [P.SecArgName _ name] = pure $ decodeUtf8Lenient name
    parseAppName _                     = throwError "Invalid or omitten application name"

    lambda (P.Section (P.Name pos name) _ _) =
        throwError $ "unexpected sub-section in lambda section " ++ show name ++ " at " ++ P.showPos pos
    lambda (P.Field (P.Name pos name) fls)
        | name == "executable" = ldExecutable .= fls'
        | otherwise =
            throwError $ "unknown lambda field " ++ show name ++ " at " ++ P.showPos pos
      where
        fls' = T.strip $ decodeUtf8Lenient $ foldMap fieldLineContents fls

    parseEnvVar (P.Section (P.Name pos name) _ _) =
        throwError $ "unexpected subsection in environment-variables section " ++ show name ++ " at " ++ P.showPos pos
    parseEnvVar (P.Field (P.Name _ name) fls) = case T.uncons fls' of
        Just ('$', var) -> return (name', Left var)
        _               -> return (name', Right fls')
      where
        name' = T.toUpper $ decodeUtf8Lenient name
        fls' = T.strip $ decodeUtf8Lenient $ foldMap fieldLineContents fls

fieldLineContents :: P.FieldLine ann -> ByteString
fieldLineContents (P.FieldLine _ bs) = bs
