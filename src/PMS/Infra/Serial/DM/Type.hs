{-# LANGUAGE TemplateHaskell #-}

module PMS.Infra.Serial.DM.Type where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Data.Default
import Data.Aeson.TH
import qualified Control.Concurrent.STM as STM
import Data.Word 
import System.IO 

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.TH as DM

-- |
--
data AppData = AppData {
               _handleAppData :: STM.TMVar (Maybe Handle)
             , _lockAppData :: STM.TMVar ()
             }

makeLenses ''AppData

defaultAppData :: IO AppData
defaultAppData = do
  mgrVar <- STM.newTMVarIO Nothing
  lock    <- STM.newTMVarIO ()
  return AppData {
           _handleAppData = mgrVar
         , _lockAppData = lock
         }

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))

-- |
--
type IOTask = IO


--------------------------------------------------------------------------------------------
-- |
--
data SerialStringToolParams =
  SerialStringToolParams {
    _argumentsSerialStringToolParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "SerialStringToolParams", omitNothingFields = True} ''SerialStringToolParams)
makeLenses ''SerialStringToolParams

instance Default SerialStringToolParams where
  def = SerialStringToolParams {
        _argumentsSerialStringToolParams = def
      }

-- |
--
data SerialIntToolParams =
  SerialIntToolParams {
    _sizeSerialIntToolParams :: Int
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "SerialIntToolParams", omitNothingFields = True} ''SerialIntToolParams)
makeLenses ''SerialIntToolParams

instance Default SerialIntToolParams where
  def = SerialIntToolParams {
        _sizeSerialIntToolParams = def
      }


-- |
--
data SerialWord8ArrayToolParams =
  SerialWord8ArrayToolParams {
    _dataSerialWord8ArrayToolParams :: [Word8]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "SerialWord8ArrayToolParams", omitNothingFields = True} ''SerialWord8ArrayToolParams)
makeLenses ''SerialWord8ArrayToolParams

instance Default SerialWord8ArrayToolParams where
  def = SerialWord8ArrayToolParams {
        _dataSerialWord8ArrayToolParams = def
      }


-- |
--
data SerialCommandToolParams =
  SerialCommandToolParams {
    _commandSerialCommandToolParams   :: String
  , _argumentsSerialCommandToolParams :: Maybe [String]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "SerialCommandToolParams", omitNothingFields = True} ''SerialCommandToolParams)
makeLenses ''SerialCommandToolParams

instance Default SerialCommandToolParams where
  def = SerialCommandToolParams {
        _commandSerialCommandToolParams   = def
      , _argumentsSerialCommandToolParams = def
      }

-- |
--
data SerialToolParams =
  SerialToolParams {
    _deviceSerialToolParams :: String
  , _speedSerialToolParams :: Int
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "SerialToolParams", omitNothingFields = True} ''SerialToolParams)
makeLenses ''SerialToolParams

instance Default SerialToolParams where
  def = SerialToolParams {
        _deviceSerialToolParams   = def
      , _speedSerialToolParams = def
      }

-- |
--
data SerialStringArrayToolParams =
  SerialStringArrayToolParams {
    _argumentsSerialStringArrayToolParams :: [String]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "SerialStringArrayToolParams", omitNothingFields = True} ''SerialStringArrayToolParams)
makeLenses ''SerialStringArrayToolParams

instance Default SerialStringArrayToolParams where
  def = SerialStringArrayToolParams {
        _argumentsSerialStringArrayToolParams = def
      }
