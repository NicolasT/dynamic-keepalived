module DynamicKeepalived.CLI (
      Options(..)
    , parser
    ) where

import Options.Applicative

data Options = Options { optionsVersion :: Bool
                       }
  deriving (Show, Eq)

parser :: Parser Options
parser =  Options
      <$> switch
          ( long "version"
         <> short 'v'
         <> help "Display the version number" )
