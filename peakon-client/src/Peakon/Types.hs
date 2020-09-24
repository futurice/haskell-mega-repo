{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
module Peakon.Types where

import Data.Aeson

import Futurice.EnvConfig
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data PeakonCfg = PeakonCfg
    { peakonToken   :: !Text
    , peakonBaseUrl :: !Text
    }

instance Configure PeakonCfg where
    configure = PeakonCfg
        <$> envVar "PEAKON_TOKEN"
        <*> envVar "PEAKON_BASEURL"

data Cfg = Cfg
    { peakonServiceToken :: !PeakonCfg
    , manager            :: !Manager
    }

class HasPeakonCfg a where
    peakonCfg :: Lens' a PeakonCfg

instance HasPeakonCfg Cfg where
    peakonCfg f cfg = fmap (\newToken -> cfg { peakonServiceToken = newToken}) (f (peakonServiceToken cfg))

class HasHttpManager a where
    httpManager :: Lens' a Manager

instance HasHttpManager Cfg where
    httpManager f cfg = fmap (\newm -> cfg { manager = newm }) (f (manager cfg))

newtype PeakonError = PeakonError String deriving Show

instance Exception PeakonError

data Employee = Employee
    { _employeeIdentifier      :: !(Maybe Text)
    , _employeeName            :: !Text
    , _employeeTerminationDate :: !(Maybe Day)
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)

instance FromJSON Employee where
    parseJSON = withObject "Employee" $ \obj -> do
      employeeData <- obj .: "attributes"
      Employee
        <$> employeeData .: "identifier"
        <*> employeeData .: "name"
        <*> employeeData .:? "Termination date"
