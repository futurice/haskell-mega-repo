{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Schedule.Command where

import Control.Lens              (review)
import Data.Aeson.Types          (parseEither, parseJSON)
import Data.Constraint           (Dict (..))
import Data.Type.Equality
import Futurice.Has              (In, inj)
import Futurice.Lomake
import Futurice.Lucid.Foundation (HtmlT)
import Futurice.Prelude
import Futurice.Servant          (SSOUser)
import Futurice.TypeTag
import Generics.SOP              (hcmap, hcollapse)
import Prelude ()
import Servant.API

import Futurice.App.Schedule.Command.AddEmployeesToSchedule
import Futurice.App.Schedule.Command.AddEventTemplate
import Futurice.App.Schedule.Command.AddEventTemplates
import Futurice.App.Schedule.Command.AddScheduleTemplate
import Futurice.App.Schedule.Command.CreateSchedule
import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Command.DeleteTemplate
import Futurice.App.Schedule.Command.RemoveEmployeesFromSchedule
import Futurice.App.Schedule.Types.Phase

import qualified Data.Map as Map

type Commands = '[ AddScheduleTemplate
                 , AddEventTemplates
                 , CreateSchedule
                 , DeleteTemplate
                 , AddEmployeesToSchedule
                 , RemoveEmployeeFromSchedule
                 , AddEventTemplate ]

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


type CommandEndpoint (cmd :: Phase -> *) = CommandTag cmd
    :> SSOUser
    :> ReqBody '[JSON] (LomakeRequest (cmd 'Input))
    :> Post '[JSON] (CommandResponse ())

commandHtmlSubmit
    :: forall cmd m. (Command cmd, Monad m, HasLomake (cmd 'Input))
    => Proxy cmd
    -> Text -> Text
    -> NP V (LomakeCode (cmd 'Input))
    -> HtmlT m ()
commandHtmlSubmit p submitName submitStyle = lomakeHtml
    (Proxy :: Proxy (cmd 'Input))
    commandFormOptions
  where
    commandFormOptions :: FormOptions
    commandFormOptions = FormOptions
        { foName        = commandTag p <> "-form"
        , foUrl         = safeLink ep ep
        , foSubmitStyle = (submitName, submitStyle)
        }

    ep :: Proxy ("commands" :> CommandEndpoint cmd)
    ep = Proxy
