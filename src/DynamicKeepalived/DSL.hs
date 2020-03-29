{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module DynamicKeepalived.DSL (
      MonadDSL(..)
    , RecordType(..)
    , ByteString
    , IP
    , Domain
    ) where

import Control.Monad.Trans (MonadTrans, lift)

import Data.ByteString.Lazy (ByteString)

import Data.IP (IP)

import Network.DNS (Domain)

data RecordType = A
                | AAAA
  deriving (Show, Eq, Enum, Bounded)

class Monad m => MonadDSL m where
    resolveDNS :: RecordType -> Domain -> m [IP]
    renderConfig :: [IP] -> m ByteString
    writeConfig :: ByteString -> m ()
    reloadKeepalived :: m ()
    sleep :: Word -> m ()

    default resolveDNS :: (MonadTrans t, MonadDSL m', m ~ t m') => RecordType -> Domain -> m [IP]
    resolveDNS r d = lift $ resolveDNS r d
    default renderConfig :: (MonadTrans t, MonadDSL m', m ~ t m') => [IP] -> m ByteString
    renderConfig = lift . renderConfig
    default writeConfig :: (MonadTrans t, MonadDSL m', m ~ t m') => ByteString -> m ()
    writeConfig = lift . writeConfig
    default reloadKeepalived :: (MonadTrans t, MonadDSL m', m ~ t m') => m ()
    reloadKeepalived = lift reloadKeepalived
    default sleep :: (MonadTrans t, MonadDSL m', m ~ t m') => Word -> m ()
    sleep = lift . sleep
