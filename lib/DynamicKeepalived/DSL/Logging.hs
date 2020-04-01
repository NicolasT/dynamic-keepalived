{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module DynamicKeepalived.DSL.Logging (
      runLoggingT
    ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)

import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)

import Data.Text.Encoding (decodeUtf8)

import Di.Monad (MonadDi)
import Di.Df1 (ToValue(..), Message, Level, Path)
import Di.Df1.Monad (attr, push, debug_, info_, notice_)

import DynamicKeepalived.DSL (MonadDSL(..), Domain, IP)

newtype LoggingT m a = LoggingT { unLoggingT :: IdentityT m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadCatch, MonadThrow, MonadMask, MonadDi level path message)

runLoggingT :: LoggingT m a -> m a
runLoggingT = runIdentityT . unLoggingT

newtype IPV = IPV { unIPV :: IP }

instance ToValue IPV where
    value = value . show . unIPV

newtype DomainV = DomainV { unDomainV :: Domain }

instance ToValue DomainV where
    value = value . decodeUtf8 . unDomainV

instance (MonadDSL m, MonadDi Level Path Message m) => MonadDSL (LoggingT m) where
    resolveDNS r d = push "resolve-dns" $ attr "record-type" r $ attr "domain" (DomainV d) $ do
        info_ "Resolving DNS record"
        res <- lift $ resolveDNS r d
        push "result" $ attr "count" (length res) $ do
            debug_ "DNS resolution returned"
            if not (null res)
               then forM_ res $ \ip -> attr "address" (IPV ip) $ debug_ "Resolved domain"
               else notice_ "No addresses returned"
        return res
    renderConfig l = push "render-config" $ attr "count" (length l) $ do
        info_ "Rendering configuration"
        c <- lift $ renderConfig l
        debug_ "Configuration rendering completed"
        return c
    writeConfig c = push "write-config" $ do
        info_ "Writing configuration"
        lift $ writeConfig c
        debug_ "Configuration written"
    reloadKeepalived = push "reload-keepalived" $ do
        info_ "Reloading keepalived"
        lift reloadKeepalived
        debug_ "Keepalived reloaded"
    sleep l = push "sleep" $ attr "time" l $ do
        info_ "Sleeping"
        lift $ sleep l
        debug_ "Waking up again"
