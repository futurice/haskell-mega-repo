{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Futurice.App.FUM.Command.Definition (
    module Futurice.Lomake,
    module Futurice.App.FUM.Command.Definition,
    ) where

import Futurice.Generics
import Futurice.Generics.SOP     (strippedFieldNames)
import Futurice.Lomake
import Futurice.Lucid.Foundation (HtmlT)
import Futurice.Prelude
import Futurice.Servant          (SSOUser)
import Futurice.Stricter         (StricterT)
import GHC.TypeLits              (KnownSymbol, Symbol, symbolVal)
import Prelude ()
import Servant.API

import Futurice.App.FUM.Types

-- | Commands are parameterised by command 'Phase'.
--
-- * User submits 'Input' commands
--
-- * which are amended to the 'Internal' commands, which are stored in persistence layer.
--
-- /Note:/ For some commands there aren't difference between phases!
--
data Phase = Input | Internal

-- | Phased field can be different in the input and what's stored in the db.
type family Phased (phase :: Phase) a b where
    Phased 'Input    a b = a
    Phased 'Internal a b = b

-- | Class describing different commands.
class
    ( HasLomake (cmd 'Input)
    , FromJSON (cmd 'Internal)
    , ToJSON (cmd 'Internal)
    , KnownSymbol (CommandTag cmd)
    )
    => Command (cmd :: Phase -> *)
  where
    -- | Command tag. Used to distinguish commands in command union.
    type CommandTag cmd :: Symbol

    -- | Transform command from 'Input' to 'Internal' phase.
    --
    -- @
    -- internalizeCommand
    --    => cmd 'Input
    --    -> (forall m. (MonadReader World m, MonadError String m, MonadIO m, MonadLog m)
    --        => m (cmd 'Internal)
    --       )
    -- @
    internalizeCommand
        :: UTCTime     -- ^ now
        -> Login       -- ^ submitter of the command
        -> Rights      -- ^ rights of the submitter
        -> cmd 'Input  -- ^ input command
        -> ReaderT World (ExceptT String (LogT IO)) (cmd 'Internal) -- ^ we can do IO and fail.

    -- | Apply 'Internal' command on the 'World'.
    --
    -- /Note:/ we can fail in this phase too.
    -- However fail conditions should be rare and treated as fatal.
    applyCommand
        :: UTCTime        -- ^ now
        -> Login          -- ^ submitter of the command
        -> cmd 'Internal  -- ^ command
        -> StricterT World (Either String) (CommandResponse ())

type CommandEndpoint (cmd :: Phase -> *) = CommandTag cmd
    :> SSOUser
    :> ReqBody '[JSON] (LomakeRequest (cmd 'Input))
    :> Post '[JSON] (CommandResponse ())

requireRights
    :: MonadError String m
    => Rights  -- ^ required
    -> Rights  -- ^ actual
    -> m ()
requireRights req act = when (act < req) $
    throwError $ "Too low rights. Required: " ++ show req ++ "; actual: " ++ show act

commandTag :: forall cmd proxy. Command cmd => proxy cmd -> Text
commandTag _ = view packed (symbolVal p) where
    p = Proxy :: Proxy (CommandTag cmd)

commandTag' :: forall cmd phase. Command cmd => cmd phase -> Text
commandTag' _ = view packed (symbolVal p) where
    p = Proxy :: Proxy (CommandTag cmd)

-- | Render command form with optional arguments.
commandHtml'
    :: forall cmd m. (Command cmd, Monad m)
    => Proxy cmd
    -> NP V (LomakeCode (cmd 'Input))
    -> HtmlT m ()
commandHtml' p = commandHtmlSubmit p "Submit" "success"

commandHtmlSubmit
    :: forall cmd m. (Command cmd, Monad m)
    => Proxy cmd
    -> Text -> Text
    -> NP V (LomakeCode (cmd 'Input))
    -> HtmlT m ()
commandHtmlSubmit p submitName submitStyle = lomakeHtml
    commandFormOptions
    (lomake cmdp)
    (strippedFieldNames cmdp)
  where
    cmdp = Proxy :: Proxy (cmd 'Input)

    commandFormOptions :: FormOptions
    commandFormOptions = FormOptions
        { foName        = commandTag p <> "-form"
        , foUrl         = safeLink ep ep
        , foSubmitStyle = (submitName, submitStyle)
        }

    ep :: Proxy ("commands" :> CommandEndpoint cmd)
    ep = Proxy
