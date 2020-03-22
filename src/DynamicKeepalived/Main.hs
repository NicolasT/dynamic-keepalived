module DynamicKeepalived.Main (
      main
    ) where

import Data.Version (showVersion)

import Options.Applicative (
      (<**>)
    , ParseError(ShowHelpText)
    , abortOption, execParser, fullDesc, help, hidden, info, infoOption, long, short)

import qualified DynamicKeepalived.CLI as CLI

import qualified Paths_dynamic_keepalived

main :: IO ()
main = execParser opts >>= \CLI.Options -> return ()
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
