{-# LANGUAGE OverloadedStrings #-}

module DynamicKeepalived.Test (
      spec
    ) where

import System.IO.Error (isUserError, ioeGetErrorString)

import Control.Monad.Writer (runWriter)

import Control.Monad.Catch (throwM)

import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldNotBe, shouldNotContain, shouldThrow)

import DynamicKeepalived (Settings(..), State(..), initialState, iteration)
import DynamicKeepalived.DSL (RecordType(..))

import DynamicKeepalived.DSL.Interpreter (Interpreter(..), Command(..), runInterpreterT, tracer)

spec :: Spec
spec = do
    describe "iteration" $ do
        it "calls 'sleep' at the end when state doesn't change" $ do
            let (_, acts) = trace (stateAddresses initialState) $ iteration settings initialState
            last acts `shouldBe` Sleep 10
        it "calls 'sleep' at the end when state changes" $ do
            let ips = ["127.0.0.1"]
            stateAddresses initialState `shouldNotBe` ips
            let (_, acts) = trace ips $ iteration settings initialState
            last acts `shouldBe` Sleep 10
        it "propagates exceptions" $ do
            let act = runInterpreterT
                        (iteration settings initialState)
                        (mempty { interpretResolveDNS = \_ _ -> pure ["127.0.0.1"]
                                , interpretReloadKeepalived = throwM (userError "test")
                                })
            act `shouldThrow` (\exc -> and [ isUserError exc
                                           , ioeGetErrorString exc == "test"
                                           ])
        it "reloads keepalived when state changes" $ do
            let ips = ["127.0.0.1"]
            stateAddresses initialState `shouldNotBe` ips
            snd (trace ips (iteration settings initialState)) `shouldContain` [ReloadKeepalived]
        it "doesn't reload keepalived when state doesn't change" $ do
            let ips = ["127.0.0.1"]
                state0 = initialState { stateAddresses = ips }
            snd (trace ips (iteration settings state0)) `shouldNotContain` [ReloadKeepalived]
        it "handles re-ordered addresses correctly" $ do
            let ips = ["127.0.0.2", "127.0.0.1", "127.0.0.3"]
                (s, acts) = trace ips (iteration settings initialState)
            acts `shouldContain` [ReloadKeepalived]
            let (_, acts2) = trace (reverse ips <> ips) (iteration settings s)
            acts2 `shouldNotContain` [ReloadKeepalived]
        it "de-duplicates resolved addresses" $ do
            let ips = ["127.0.0.1", "127.0.0.2", "127.0.0.1"]
                (s, acts) = trace ips (iteration settings initialState)
            acts `shouldContain` [RenderConfig ["127.0.0.1", "127.0.0.2"]]
            stateAddresses s `shouldBe` ["127.0.0.1", "127.0.0.2"]
  where
    settings = Settings 10 A "nicolast.be"
    trace ips prog = runWriter $ runInterpreterT prog
                               $ tracer <> mempty { interpretResolveDNS = \_ _ -> pure ips
                                                  }
