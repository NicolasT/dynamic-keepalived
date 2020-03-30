{-# LANGUAGE OverloadedStrings #-}

module DynamicKeepalived.DSL.IO.Test (
      spec
    ) where

import Data.String (fromString)

import System.IO.Temp (withSystemTempDirectory)

import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), StdStream(..), getPid, proc, withCreateProcess, waitForProcess)
import System.Posix.Signals (sigHUP)

import Network.DNS (DNSError, makeResolvSeed, defaultResolvConf)

import Test.Hspec (
      Spec, around, beforeAll, describe, it
    , shouldBe, shouldReturn, shouldNotReturn, shouldThrow, anyIOException)

import DynamicKeepalived.DSL (MonadDSL(..), RecordType(..))
import DynamicKeepalived.DSL.IO (runIO)

spec :: Spec
spec = do
    beforeAll (makeResolvSeed defaultResolvConf) $
        describe "resolveDNS" $ do
            it "resolves A records" $ \r -> do
                let act = runIO r undefined undefined undefined $
                            resolveDNS A "nicolast.be"
                act `shouldNotReturn` []
            it "resolves AAAA records" $ \r -> do
                let act = runIO r undefined undefined undefined $
                            resolveDNS AAAA "google.com"
                act `shouldNotReturn` []
            it "propagates resolver errors" $ \r -> do
                let act = runIO r undefined undefined undefined $
                            resolveDNS A "nosuchhost.nicolast.be"
                act `shouldThrow` (const True :: DNSError -> Bool)

    describe "renderConfig" $
        it "works" $ do
            let ips = ["127.0.0.1"]
                bs = fromString $ "addresses: " <> show ips
                render ["127.0.0.1"] = bs
                render _ = error "Unexpected input"
                act = runIO undefined render undefined undefined $
                        renderConfig ips
            act `shouldReturn` bs

    around (withSystemTempDirectory "test-dynamic-keepalived") $
        describe "writeConfig" $ do
            it "writes the configuration in the expected file" $ \t -> do
                let path = t <> "keepalived.conf"
                runIO undefined undefined path undefined $
                    writeConfig "test data"
                readFile path `shouldReturn` "test data"
            it "overwrites existing file content" $ \t -> do
                let path = t <> "keepalived.conf"
                writeFile path "original test data"
                readFile path `shouldReturn` "original test data"
                runIO undefined undefined path undefined $
                    writeConfig "new test data"
                readFile path `shouldReturn` "new test data"

    around (withSystemTempDirectory "test-dynamic-keepalived") $
        describe "reloadKeepalived" $ do
            it "errors when PID file doesn't exist" $ \t -> do
                let path = t <> "keepalived.pid"
                    act = runIO undefined undefined undefined path
                            reloadKeepalived
                act `shouldThrow` anyIOException
            it "signals the given process with SIGHUP" $ \t -> do
                let path = t <> "keepalived.pid"
                    p = (proc "cat" []) { std_in = CreatePipe
                                        , close_fds = True
                                        }
                rc <- withCreateProcess p $ \_ _ _ ph -> do
                    pid <- getPid ph
                    case pid of
                        Nothing -> error "Unable to find PID"
                        Just pid' -> do
                            writeFile path (show pid')
                            runIO undefined undefined undefined path
                                reloadKeepalived
                            waitForProcess ph
                rc `shouldBe` ExitFailure (fromIntegral $ negate sigHUP)
