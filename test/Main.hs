module Main (
      main
    ) where

import Test.Hspec (hspec, describe)

import qualified DynamicKeepalived.Test
import qualified DynamicKeepalived.DSL.IO.Test

main :: IO ()
main = hspec $ do
    describe "DynamicKeepalived" DynamicKeepalived.Test.spec
    describe "DynamicKeepalived.DSL.IO" DynamicKeepalived.DSL.IO.Test.spec
