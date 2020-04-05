{-# LANGUAGE OverloadedStrings #-}

module DynamicKeepalived.Keepalived.Test (
      spec
    ) where

import System.Directory (findExecutable)
import System.Exit (ExitCode(..))

import Test.Hspec (Spec, runIO, describe, xdescribe, it, shouldReturn)

import DynamicKeepalived.Keepalived (validateKeepalivedConfig)

spec :: Spec
spec = do
    requireKeepalived <- runIO $ maybe xdescribe (const describe) <$> findExecutable "keepalived"

    requireKeepalived "validateKeepalivedConfig" $ do
        it "accepts a valid config" $ do
            let config = ""
            validateKeepalivedConfig config `shouldReturn` ExitSuccess

        it "rejects an invalid config" $ do
            let config = "foo"
            validateKeepalivedConfig config `shouldReturn` ExitFailure 4
