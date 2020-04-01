{-# LANGUAGE OverloadedStrings #-}

module DynamicKeepalived.DSL.Logging.Test (
      spec
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, writeTQueue, flushTQueue)

import qualified Di.Core
import Di.Monad (runDiT)

import Test.Hspec (Spec, describe, it, shouldReturn, shouldNotReturn)

import DynamicKeepalived (Settings(..), iteration, initialState)
import DynamicKeepalived.DSL (RecordType(..))
import DynamicKeepalived.DSL.IO (runIO)
import DynamicKeepalived.DSL.Logging (runLoggingT)

import DynamicKeepalived.DSL.Interpreter (runInterpreterT)

spec :: Spec
spec =
    describe "runLoggingT" $ do
        it "compiles with runIO" $ do
            let act = Di.Core.new (const $ return ()) $ \di -> runDiT di $
                        runIO undefined undefined undefined undefined $ runLoggingT $ return ()
            act `shouldReturn` ()

        it "adds logging" $ do
            queue <- newTQueueIO
            let handle = atomically . writeTQueue queue

            _ <- Di.Core.new handle $ \di -> runDiT di $
                flip runInterpreterT mempty $ runLoggingT $ iteration (Settings 10 A "nicolast.be") initialState

            atomically (flushTQueue queue) `shouldNotReturn` []
