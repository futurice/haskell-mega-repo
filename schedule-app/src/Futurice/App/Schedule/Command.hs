{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Futurice.App.Schedule.Command where

import Control.Lens       (review)
import Data.Aeson.Types   (parseEither, parseJSON)
import Data.Constraint    (Dict (..))
import Data.Type.Equality
import Futurice.Has       (In, inj)
import Futurice.Prelude
import Futurice.TypeTag
import Generics.SOP       (hcmap, hcollapse)
import Prelude ()

import Futurice.App.Schedule.Command.AddEventTemplates
import Futurice.App.Schedule.Command.AddScheduleTemplate
import Futurice.App.Schedule.Command.CreateSchedule
import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Phase

import qualified Data.Map as Map

type Commands = '[ AddScheduleTemplate
                 , AddEventTemplates
                 , CreateSchedule]

type CT = TT Commands

-- | Existential command, union of all commands.
data SomeCommand where
    SomeCommand :: CT cmd -> cmd 'Done -> SomeCommand

type ICT cmd = In cmd Commands

-- | 'SomeCommand' introduction.
someCommand :: (Command cmd, ICT cmd) => cmd 'Done -> SomeCommand
someCommand = SomeCommand (TT (review inj Refl))

withCT :: forall cmd r. CT cmd -> (Command cmd => r) -> r
withCT ct r = case typeTagDict (Proxy :: Proxy Command) ct of
    Dict -> r

-- | 'SomeCommand' elimination.
withSomeCommand
    :: SomeCommand
    -> (forall cmd. Command cmd => CT cmd -> cmd 'Done -> r)
    -> r
withSomeCommand (SomeCommand tag cmd) f = withCT tag (f tag cmd)

decodeSomeCommand :: Text -> Value -> Either String SomeCommand
decodeSomeCommand name payload = case Map.lookup name typeTagsByName of
    Nothing ->
        Left $ "Unknown  command: " ++ show name ++ " = " ++ show payload
    Just (SomeTT tag) -> withCT tag $
        SomeCommand tag <$> parseEither parseJSON payload

typeTagsByName :: Map Text (SomeTT Commands)
typeTagsByName = Map.fromList $ hcollapse $ hcmap (Proxy :: Proxy Command) f (typeTags :: NP CT Commands)
  where
    f :: forall cmd. Command cmd => TT Commands cmd -> K (Text, SomeTT Commands) cmd
    f tag = K (commandTag tag, SomeTT tag)

commandTag :: forall cmd proxy. Command cmd => proxy cmd -> Text
commandTag _ = view packed (symbolVal p) where
    p = Proxy :: Proxy (CommandTag cmd)

commandTag' :: forall cmd phase. Command cmd => cmd phase -> Text
commandTag' _ = view packed (symbolVal p) where
    p = Proxy :: Proxy (CommandTag cmd)
