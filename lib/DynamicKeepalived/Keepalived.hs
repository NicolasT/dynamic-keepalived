module DynamicKeepalived.Keepalived (
      validateKeepalivedConfig
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Catch (MonadMask)

import System.IO (hClose)

import System.Exit (ExitCode(..))

import System.Process (readProcessWithExitCode)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import System.IO.Temp (withSystemTempFile)

validateKeepalivedConfig :: (MonadIO m, MonadMask m) => ByteString -> m ExitCode
validateKeepalivedConfig c = withSystemTempFile "dynamic-keepalived.conf" $ \fp h -> liftIO $ do
    -- It would be nice to just inject the config over stdin and use /dev/stdin as the config file path
    -- passed to `keepalived`. However, `keepalived` has some code which ensures the given path is a
    -- regular file, not a pipe. So we need this tempfile work-around.
    BS.hPut h c
    hClose h

    (rc, _, _) <- readProcessWithExitCode "keepalived" ["--use-file", fp, "--config-test"] ""
    return rc
