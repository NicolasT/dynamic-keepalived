{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module DynamicKeepalived.DSL.Free (
      CommandF(..)
    ) where

import Control.Monad.Free (Free)
import Control.Monad.Trans.Free (FreeT, liftF)

import DynamicKeepalived.DSL (MonadDSL, ByteString, Domain, IP, RecordType)
import qualified DynamicKeepalived.DSL as DSL

data CommandF next = ResolveDNS RecordType Domain ([IP] -> next)
                   | RenderConfig [IP] (ByteString -> next)
                   | WriteConfig ByteString next
                   | ReloadKeepalived next
                   | Sleep Word next
  deriving (Functor)

instance MonadDSL (Free CommandF) where
    resolveDNS r d = liftF (ResolveDNS r d id)
    renderConfig a = liftF (RenderConfig a id)
    writeConfig bs = liftF (WriteConfig bs ())
    reloadKeepalived = liftF (ReloadKeepalived ())
    sleep l = liftF (Sleep l ())

instance Monad m => MonadDSL (FreeT CommandF m) where
    resolveDNS r d = liftF (ResolveDNS r d id)
    renderConfig a = liftF (RenderConfig a id)
    writeConfig bs = liftF (WriteConfig bs ())
    reloadKeepalived = liftF (ReloadKeepalived ())
    sleep l = liftF (Sleep l ())
