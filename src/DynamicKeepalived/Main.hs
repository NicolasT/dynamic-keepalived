{-# LANGUAGE FlexibleContexts #-}

module DynamicKeepalived.Main (
      main
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Version (showVersion)

import Di (Level, Message, MonadDi)
import qualified Di

import Options.Applicative (
      (<**>)
    , ParseError(ShowHelpText)
    , abortOption, execParser, fullDesc, help, hidden, info, infoOption, long, short)

import qualified DynamicKeepalived.CLI as CLI

import qualified Paths_dynamic_keepalived

main' :: (MonadDi Level path Message m) => CLI.Options -> m ()
main' _options = do
    Di.debug "Running mainloop"

main :: IO ()
main = Di.new $ \di -> Di.runDiT di $ do
    options <- liftIO $ execParser opts
    main' options
  where
    opts = info (CLI.parser <**> version <**> helper) fullDesc
    version = infoOption versionMessage $ mconcat [ long "version"
                                                  , short 'v'
                                                  , help "Display the version number"
                                                  , hidden
                                                  ]
    versionMessage = "dynamic-keepalived v" ++ showVersion Paths_dynamic_keepalived.version
    helper = abortOption ShowHelpText $ mconcat [ long "help"
                                                , short 'h'
                                                , help "Display this help message"
                                                , hidden
                                                ]
