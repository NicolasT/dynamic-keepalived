module Main (
      main
    ) where

import Test.Hspec (hspec, describe)

import qualified DynamicKeepalived.Test
import qualified DynamicKeepalived.DSL.IO.Test
import qualified DynamicKeepalived.DSL.Logging.Test
import qualified DynamicKeepalived.Dhall.Test

main :: IO ()
main = hspec $ do
    describe "DynamicKeepalived" DynamicKeepalived.Test.spec
    describe "DynamicKeepalived.DSL.IO" DynamicKeepalived.DSL.IO.Test.spec
    describe "DynamicKeepalived.DSL.Logging" DynamicKeepalived.DSL.Logging.Test.spec
    describe "DynamicKeepalived.Dhall" DynamicKeepalived.Dhall.Test.spec
