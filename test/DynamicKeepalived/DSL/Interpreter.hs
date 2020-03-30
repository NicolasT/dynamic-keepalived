{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DynamicKeepalived.DSL.Interpreter (
      Interpreter(..)
    , runInterpreterT
    , Command(..)
    , tracer
    ) where

import Control.Applicative (liftA2)

import Control.Monad.Writer.Class (MonadWriter, tell)

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, reader)

import DynamicKeepalived.DSL (MonadDSL(..), RecordType, ByteString, Domain, IP)

data Interpreter m = Interpreter { interpretResolveDNS :: RecordType -> Domain -> m [IP]
                                 , interpretRenderConfig :: [IP] -> m ByteString
                                 , interpretWriteConfig :: ByteString -> m ()
                                 , interpretReloadKeepalived :: m ()
                                 , interpretSleep :: Word -> m ()
                                 }

instance Applicative m => Semigroup (Interpreter m) where
    a <> b = Interpreter { interpretResolveDNS = \t d -> liftA2 (<>) (interpretResolveDNS a t d) (interpretResolveDNS b t d)
                         , interpretRenderConfig = \as -> liftA2 (<>) (interpretRenderConfig a as) (interpretRenderConfig b as)
                         , interpretWriteConfig = \c -> interpretWriteConfig a c *> interpretWriteConfig b c
                         , interpretReloadKeepalived = interpretReloadKeepalived a *> interpretReloadKeepalived b
                         , interpretSleep = \l -> interpretSleep a l *> interpretSleep b l
                         }

instance Applicative m => Monoid (Interpreter m) where
    mempty = Interpreter { interpretResolveDNS = \_ _ -> pure mempty
                         , interpretRenderConfig = \_ -> pure mempty
                         , interpretWriteConfig = \_ -> pure ()
                         , interpretReloadKeepalived = pure ()
                         , interpretSleep = \_ -> pure ()
                         }


data Command = ResolveDNS RecordType Domain
             | RenderConfig [IP]
             | WriteConfig ByteString
             | ReloadKeepalived
             | Sleep Word
  deriving (Show, Eq)

tracer :: (Applicative f, MonadWriter (f Command) m) => Interpreter m
tracer = Interpreter { interpretResolveDNS = \t d -> tell' (ResolveDNS t d) *> pure mempty
                     , interpretRenderConfig = \a -> tell' (RenderConfig a) *> pure mempty
                     , interpretWriteConfig = \b -> tell' (WriteConfig b)
                     , interpretReloadKeepalived = tell' (ReloadKeepalived)
                     , interpretSleep = \l -> tell' (Sleep l)
                     }
  where
    tell' = tell . pure


newtype InterpreterT m a = InterpreterT { unInterpreterT :: ReaderT (Interpreter m) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift

instance Monad m => MonadDSL (InterpreterT m) where
    resolveDNS r d = InterpreterT $ lift . flip ($ r) d =<< reader interpretResolveDNS
    renderConfig as = InterpreterT $ lift . ($ as) =<< reader interpretRenderConfig
    writeConfig bs = InterpreterT $ lift . ($ bs) =<< reader interpretWriteConfig
    reloadKeepalived = InterpreterT $ lift =<< reader interpretReloadKeepalived
    sleep l = InterpreterT $ lift . ($ l) =<< reader interpretSleep

runInterpreterT :: InterpreterT m a -> Interpreter m -> m a
runInterpreterT = runReaderT . unInterpreterT
