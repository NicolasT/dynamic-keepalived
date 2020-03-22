module DynamicKeepalived.Main (
      main
    ) where

import Control.Monad (when)
import Data.Version (showVersion)
import System.Exit (exitSuccess)

import Options.Applicative (
      (<**>)
    , ParseError(ShowHelpText)
    , abortOption, execParser, fullDesc, help, hidden, info, long, short)

import qualified DynamicKeepalived.CLI as CLI

import qualified Paths_dynamic_keepalived

main :: IO ()
main = execParser opts >>= \options -> do
    when (CLI.optionsVersion options) $ do
        putStrLn $ "dynamic-keepalived v" ++ showVersion Paths_dynamic_keepalived.version
        exitSuccess
  where
    opts = info (CLI.parser <**> helper) fullDesc
    helper = abortOption ShowHelpText $ mconcat [ long "help"
                                                , short 'h'
                                                , help "Display this help message"
                                                , hidden
                                                ]
