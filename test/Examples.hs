{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples (
      spec
    ) where

import System.Exit (ExitCode(..))

import System.Directory (findExecutable)

import System.FilePath (splitFileName)

import Control.Monad.Catch (throwM)

import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.TypeCheck as Dhall

import Lens.Family (over, set)

import qualified Data.Text.IO as Text
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Test.Hspec (Spec, describe, it, xit, runIO, shouldNotBe, shouldSatisfy, shouldReturn)

import DynamicKeepalived.DSL (IP)
import DynamicKeepalived.Dhall (addSubstitutions)
import DynamicKeepalived.Keepalived (validateKeepalivedConfig)

spec :: Spec
spec = do
    let evaluateSettings = over Dhall.substitutions addSubstitutions Dhall.defaultEvaluateSettings
    let inputSettings = over Dhall.substitutions addSubstitutions Dhall.defaultInputSettings

    describe "keepalived.dhall" $ do
        let filename = "examples/keepalived.dhall"

        let loadExpr = do
                src <- Text.readFile filename
                let (dir, name) = splitFileName filename
                let inputSettings' = set Dhall.rootDirectory dir
                                   . set Dhall.sourceName name
                                   $ inputSettings
                Dhall.inputExprWithSettings inputSettings' src

        it "is a valid Dhall script" $ do
            _ <- loadExpr
            return ()

        it "has the expected type" $ do
            expr <- loadExpr
            let ipType = Dhall.declared (Dhall.inject :: Dhall.Encoder IP)
            case Dhall.typeOf expr of
                Left exc -> throwM exc
                Right typ -> typ `shouldSatisfy` \case
                    -- [IP] -> Text
                    Dhall.Pi _ (Dhall.App Dhall.List listItemType) Dhall.Text | listItemType == ipType -> True
                    _ -> False


        let loadScript :: FilePath -> IO ([IP] -> Text)
            loadScript =
                Dhall.inputFileWithSettings evaluateSettings Dhall.auto

        it "can be loaded and executed" $ do
            fn <- loadScript filename
            fn ["127.0.0.1" :: IP] `shouldNotBe` (mempty :: Text)

        requireKeepalived <- runIO $ maybe xit (const it) <$> findExecutable "keepalived"

        requireKeepalived "generates a valid keepalived configuration" $ do
            fn <- loadScript filename
            let config = fn ["127.0.0.1", "127.0.0.2" :: IP]
            validateKeepalivedConfig (encodeUtf8 config) `shouldReturn` ExitSuccess
