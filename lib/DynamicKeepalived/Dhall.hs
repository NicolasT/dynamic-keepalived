{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DynamicKeepalived.Dhall (
      ipShow
    , addSubstitutions
    ) where

import Data.Void (Void)
import GHC.Generics (Generic)

import Data.Functor.Contravariant (contramap)

import qualified Data.IP

import Dhall (ToDhall(..), Encoder, declared, inject)
import Dhall.Core (Expr)
import qualified Dhall.Map
import Dhall.Src (Src)
import Dhall.Substitution (Substitutions)
import Dhall.TH (staticDhallExpression)

data IP = IPv4 String
        | IPv6 String
  deriving (Generic)

instance ToDhall IP

instance ToDhall Data.IP.IP where
    injectWith n = let ipToIp' ip = case ip of
                           Data.IP.IPv4 ip' -> IPv4 (show ip')
                           Data.IP.IPv6 ip' -> IPv6 (show ip')
                   in contramap ipToIp' (injectWith n)

ipShow :: Expr s a
ipShow =  $(staticDhallExpression "\\(ip : < IPv4 : Text | IPv6 : Text >) -> let handlers = { IPv4 = \\(t : Text) -> t, IPv6 = \\(t : Text) -> t } in merge handlers ip : Text")

addSubstitutions :: Substitutions Src Void -> Substitutions Src Void
addSubstitutions = Dhall.Map.union ipValues
  where
    ipValues = [ ("IP", declared (inject :: Encoder IP))
               , ("IP/show", ipShow)
               ]
