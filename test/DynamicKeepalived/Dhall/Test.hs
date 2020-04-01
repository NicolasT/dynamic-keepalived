{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DynamicKeepalived.Dhall.Test (
      spec
    ) where

import Data.IP (IP(..))

import Dhall (Encoder, auto, declared, defaultInputSettings, embed, inject, inputWithSettings, inputExprWithSettings, rawInput, substitutions)
import Dhall.Core (Expr(..))
import Dhall.TH (staticDhallExpression)
import Dhall.TypeCheck (typeOf)

import Lens.Family (over)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, shouldReturn)

import DynamicKeepalived.Dhall (addSubstitutions, ipShow)

spec :: Spec
spec = do
    let ipType = declared (inject :: Encoder IP)

    describe "IP" $ do
        it "is mapped to a union of expected tags" $
            ipType `shouldBe` $(staticDhallExpression "< IPv4 : Text | IPv6 : Text >")

        it "converts IPv4 addresses correctly" $ do
            let ip = IPv4 "127.0.0.1"
            embed inject ip `shouldBe` $(staticDhallExpression "< IPv4 : Text | IPv6 : Text >.IPv4 \"127.0.0.1\"")

        it "converts IPv6 addresses correctly" $ do
            let ip = IPv6 "0::1"
            embed inject ip `shouldBe` $(staticDhallExpression "< IPv4 : Text | IPv6 : Text>.IPv6 \"::1\"")

    describe "IP/show" $ do
        it "has the expected type" $
            typeOf ipShow `shouldSatisfy` \case
                Right (Pi _ ipType' Text) -> ipType' == ipType
                _ -> False

        let evalIpShow ip = do
                let dhallIp = embed inject ip
                    program = App ipShow dhallIp
                typeOf program `shouldSatisfy` \case
                    Right Text -> True
                    _ -> False
                return (rawInput auto program)

        it "works with IPv4 addresses" $
            evalIpShow (IPv4 "127.0.0.1") `shouldReturn` Just ("127.0.0.1" :: String)

        it "works with IPv6 addresses" $
            evalIpShow (IPv6 "0::1") `shouldReturn` Just ("::1" :: String)

    describe "addSubstitutions" $ do
        let inputSettings = over substitutions addSubstitutions defaultInputSettings

        it "adds 'IP'" $
            inputExprWithSettings inputSettings "IP" `shouldReturn` ipType

        it "adds 'IP/show'" $
            inputExprWithSettings inputSettings "IP/show" `shouldReturn` ipShow

        it "allows a simple program to work" $ do
            let program = mconcat [ "let List/map = https://prelude.dhall-lang.org/v15.0.0/List/map "
                                  ,     "sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680 "
                                  , "in \\(ips : List IP) -> List/map IP Text IP/show ips"
                                  ]
            fn <- inputWithSettings inputSettings auto program :: IO ([IP] -> [String])
            fn [IPv4 "127.0.0.1", IPv6 "0::1", IPv4 "0.0.0.0"] `shouldBe` ["127.0.0.1", "::1", "0.0.0.0"]
