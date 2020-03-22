module DynamicKeepalived.CLI (
      Options(..)
    , parser
    ) where

import Options.Applicative (Parser)

data Options = Options
  deriving (Show, Eq)

parser :: Parser Options
parser = pure Options
