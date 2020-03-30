module Main (
      main
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Version (showVersion)

import qualified Di

import Options.Applicative (
      Parser
    , (<**>)
    , ParseError(ShowHelpText)
    , abortOption, execParser, fullDesc, help, hidden, info, infoOption, long, short)

import qualified Paths_dynamic_keepalived

data Options = Options
  deriving (Show, Eq)

parser :: Parser Options
parser = pure Options

main :: IO ()
main = Di.new $ \di -> Di.runDiT di $ do
    _options <- liftIO $ execParser opts
    Di.debug "Running mainloop"
  where
    opts = info (parser <**> version <**> helper) fullDesc
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
