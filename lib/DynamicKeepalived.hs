module DynamicKeepalived (
      Settings(..)
    , State(..)
    , app
    , initialState
    , iteration
    ) where

import Control.Monad (when)
import Data.List (nub, sort)

import DynamicKeepalived.DSL (MonadDSL(..), RecordType, Domain, IP)

data Settings = Settings { settingsInterval :: Word
                         , settingsRecordType :: RecordType
                         , settingsDomain :: Domain
                         }
  deriving (Show, Eq)

data State = State { stateAddresses :: [IP]
                   }
  deriving (Show, Eq)

app :: MonadDSL m => Settings -> m a
app settings = loop initialState
  where
    loop state = iteration settings state >>= loop

initialState :: State
initialState = State []

iteration :: MonadDSL m => Settings -> State -> m State
iteration settings state = do
    addresses <- (nub . sort) <$> resolveDNS (settingsRecordType settings) (settingsDomain settings)

    let newState = State addresses

    when (newState /= state) $ do
        config <- renderConfig $ stateAddresses newState
        writeConfig config
        reloadKeepalived

    sleep (settingsInterval settings)
    return newState
