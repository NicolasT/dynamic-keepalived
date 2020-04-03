{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DynamicKeepalived.DSL.IO (
      runIO
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)

import System.Posix.Signals (signalProcess, sigHUP)

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, reader)

import Data.IP (IP(..))

import Network.DNS (ResolvSeed, withResolver, lookupA, lookupAAAA)

import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)

import Di.Monad (MonadDi)

import DynamicKeepalived.DSL (MonadDSL(..), RecordType(..), ByteString)

data IOTEnv m = IOTEnv { iotEnvConfigPath :: FilePath
                       , iotEnvPidPath :: FilePath
                       , iotEnvResolvSeed :: ResolvSeed
                       , iotEnvRenderConfig :: [IP] -> m ByteString
                       }

newtype IOT m a = IOT { unIOT :: ReaderT (IOTEnv m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask, MonadDi level path message)

instance MonadTrans IOT where
    lift = IOT . lift

runIO :: ResolvSeed -> ([IP] -> m ByteString) -> FilePath -> FilePath -> IOT m a -> m a
runIO resolvSeed renderConfig' configPath pidPath act =
    runReaderT (unIOT act) $ IOTEnv configPath pidPath resolvSeed renderConfig'

instance (MonadIO m, MonadThrow m) => MonadDSL (IOT m) where
    resolveDNS t d = IOT $ do
        resolvSeed <- reader iotEnvResolvSeed
        res <- liftIO $ withResolver resolvSeed $ \resolver ->
            case t of
                A -> fmap (fmap IPv4) <$> lookupA resolver d
                AAAA -> fmap (fmap IPv6) <$> lookupAAAA resolver d
        either throwM pure res
    renderConfig as = IOT $ do
        fn <- reader iotEnvRenderConfig
        lift $ fn as
    writeConfig bs = IOT $ do
        path <- reader iotEnvConfigPath
        liftIO $ atomicWriteFile path bs
    reloadKeepalived = IOT $ do
        pidPath <- reader iotEnvPidPath
        pid <- read <$> liftIO (readFile pidPath)
        liftIO $ signalProcess sigHUP pid
    sleep = IOT . liftIO . threadDelay . fromIntegral
