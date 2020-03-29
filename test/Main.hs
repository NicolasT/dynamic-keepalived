{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
      main
    ) where

import System.IO.Error (isUserError, ioeGetErrorString)

import Control.Monad.Free (Free(..))

import Control.Monad.Writer (runWriter, tell)

import Control.Monad.Catch (throwM)

import Test.Hspec (describe, hspec, it, shouldBe, shouldContain, shouldNotBe, shouldNotContain, shouldThrow)

import DynamicKeepalived (Settings(..), State(..), initialState, iteration)
import DynamicKeepalived.DSL (RecordType(..), ByteString, Domain, IP)
import qualified DynamicKeepalived.DSL.Free as F

data Command = ResolveDNS RecordType Domain
             | RenderConfig [IP]
             | WriteConfig ByteString
             | ReloadKeepalived
             | Sleep Word
  deriving (Show, Eq)

main :: IO ()
main = hspec $
    describe "iteration" $ do
        it "calls 'sleep' at the end when state doesn't change" $ do
            let (_, acts) = trace (stateAddresses initialState) $ iteration settings initialState
            last acts `shouldBe` Sleep 10
        it "calls 'sleep' at the end when state changes" $ do
            let ips = [read "127.0.0.1"]
            stateAddresses initialState `shouldNotBe` ips
            let (_, acts) = trace ips $ iteration settings initialState
            last acts `shouldBe` Sleep 10
        it "propagates exceptions" $ do
            let act = interpret
                        (\_ _ n -> return (n [read "0"]))
                        (\_ n -> return (n mempty))
                        (\_ n -> return n)
                        (\n -> throwM (userError "test") >> return n)
                        (\_ n -> return n)
                        (iteration settings initialState)
            act `shouldThrow` (\exc -> and [ isUserError exc
                                           , ioeGetErrorString exc == "test"
                                           ])
        it "reloads keepalived when state changes" $ do
            let ips = [read "127.0.0.1"]
            stateAddresses initialState `shouldNotBe` ips
            snd (trace ips (iteration settings initialState)) `shouldContain` [ReloadKeepalived]
        it "doesn't reload keepalived when state doesn't change" $ do
            let ips = [read "127.0.0.1"]
                state0 = initialState { stateAddresses = ips }
            snd (trace ips (iteration settings state0)) `shouldNotContain` [ReloadKeepalived]
        it "handles re-ordered addresses correctly" $ do
            let ips = map read ["127.0.0.1", "127.0.0.2", "127.0.0.3"]
                (s, acts) = trace ips (iteration settings initialState)
            acts `shouldContain` [ReloadKeepalived]
            let (_, acts2) = trace (reverse ips) (iteration settings s)
            acts2 `shouldNotContain` [ReloadKeepalived]
  where
    settings = Settings 10 A "nicolast.be"
    interpret handleResolveDNS handleRenderConfig handleWriteConfig handleReloadKeepalived handleSleep =
        let loop = \case
                Pure r -> return r
                Free f -> case f of
                    F.ResolveDNS t d n -> handleResolveDNS t d n >>= loop
                    F.RenderConfig a n -> handleRenderConfig a n >>= loop
                    F.WriteConfig b n -> handleWriteConfig b n >>= loop
                    F.ReloadKeepalived n -> handleReloadKeepalived n >>= loop
                    F.Sleep l n -> handleSleep l n >>= loop
        in loop
    trace ips prog = runWriter $ interpret
                                   (\t d n -> tell [ResolveDNS t d] >> return (n ips))
                                   (\a n -> tell [RenderConfig a] >> return (n mempty))
                                   (\b n -> tell [WriteConfig b] >> return n)
                                   (\n -> tell [ReloadKeepalived] >> return n)
                                   (\l n -> tell [Sleep l] >> return n)
                                   prog
