module Main (
      main
    ) where

import Test.Hspec (hspec, describe)

import qualified DynamicKeepalived.Test

main :: IO ()
main = hspec $
    describe "DynamicKeepalived" (DynamicKeepalived.Test.spec)
