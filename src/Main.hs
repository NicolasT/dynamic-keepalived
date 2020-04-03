{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
      main
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (nub, sort)
import Data.Version (showVersion)

import Control.Monad.Catch (MonadMask, throwM)

import System.Exit (ExitCode(..), exitWith)

import qualified Data.ByteString.Lazy as BS

import Network.DNS (FileOrNumericHost(..), defaultResolvConf, makeResolvSeed, resolvInfo)

import qualified Di
import qualified Di.Monad as Di (filter, flush)

import Df1 (Level(..))

import qualified Dhall

import Lens.Family (over)

import Data.Text.Lazy.Encoding (encodeUtf8)

import Options.Applicative (
      Parser
    , (<**>)
    , ParseError(ShowHelpText)
    , abortOption, action, argument, auto, completeWith, execParser, fullDesc
    , help, hidden, info, infoOption, long, metavar, option, short, showDefault
    , str, strOption, switch, value)

import DynamicKeepalived (Settings(..), app)
import DynamicKeepalived.Dhall (addSubstitutions)
import DynamicKeepalived.DSL (MonadDSL(..), RecordType(..), Domain)
import DynamicKeepalived.DSL.IO (runIO)
import DynamicKeepalived.DSL.Logging (runLoggingT)
import DynamicKeepalived.Keepalived (validateKeepalivedConfig)

import qualified Paths_dynamic_keepalived

data Options = Options { optionsConfigFile :: FilePath
                       , optionsPidFile :: FilePath
                       , optionsLogDetail :: Bool
                       , optionsConfigTest :: Bool
                       , optionsDumpConfig :: Bool
                       , optionsDNSRefreshInterval :: Word
                       , optionsDNSRecordType :: RecordType
                       , optionsDNSResolvConf :: FilePath
                       , optionsDNSDomain :: Domain
                       , optionsTemplate :: FilePath
                       }
  deriving (Show, Eq)

parser :: Parser Options
parser = Options <$> strOption ( long "use-file"
                              <> short 'f'
                              <> metavar "FILE"
                              <> help "Use the specified keepalived configuration file"
                              <> value "/etc/keepalived/keepalived.conf"
                              <> showDefault
                              <> action "file")
                 <*> strOption ( long "pid"
                              <> short 'p'
                              <> metavar "FILE"
                              <> help "Use specified keepalived pidfile"
                              <> value "/run/keepalived.pid"
                              <> showDefault
                              <> action "file")
                 <*> switch ( long "log-detail"
                           <> short 'D'
                           <> help "Detailed log messages")
                 <*> switch ( long "config-test"
                           <> short 't'
                           <> help "Check the configuration for obvious errors")
                 <*> switch ( long "dump-conf"
                           <> short 'd'
                           <> help "Dump the configuration data")
                 <*> option auto ( long "dns-refresh-interval"
                                <> metavar "SECONDS"
                                <> help "Interval at which the DNS record gets refreshed"
                                <> value 10
                                <> showDefault)
                 <*> option auto ( long "dns-record-type"
                                <> metavar "TYPE"
                                <> help "DNS record type to query"
                                <> value A
                                <> showDefault
                                <> completeWith (map show [minBound .. maxBound :: RecordType ]))
                 <*> strOption ( long "dns-resolv-conf"
                              <> metavar "FILE"
                              <> help "Path to resolv.conf file to configure DNS resolved"
                              <> value "/etc/resolv.conf"
                              <> showDefault
                              <> action "file"
                               )
                 <*> argument str ( metavar "DOMAIN"
                                 <> help "DNS domain to lookup")
                 <*> argument str ( metavar "FILE"
                                 <> help "Path to Dhall script to generate keepalived configuration"
                                 <> action "file")

parseOptions :: IO Options
parseOptions = execParser opts
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

dumpConfig :: (MonadIO m, MonadMask m, Di.MonadDi Di.Level Di.Path Di.Message m, MonadDSL m) => Bool -> Settings -> m ExitCode
dumpConfig configTest settings = Di.push "dump-config" $ do
    addresses <- nub . sort <$> resolveDNS (settingsRecordType settings) (settingsDomain settings)
    config <- renderConfig addresses

    Di.flush
    liftIO $ BS.putStr config

    if not configTest
       then return ExitSuccess
       else testConfig config

testConfig :: (MonadIO m, MonadMask m, Di.MonadDi Di.Level Di.Path Di.Message m) => BS.ByteString -> m ExitCode
testConfig c = Di.push "test-config" $ do
    Di.info_ "Validating configuration"
    rc <- validateKeepalivedConfig c
    Di.attr "rc" (getExitCode rc) $ do
        let message = "keepalived returned"
        case rc of
            ExitSuccess -> Di.info_ message
            ExitFailure _ -> Di.alert_ message
    return rc
  where
    getExitCode rc = case rc of
        ExitSuccess -> 0
        ExitFailure n -> n

mainloop :: (MonadDSL m, Di.MonadDi Di.Level path Di.Message m) => Settings -> m a
mainloop settings = do
    Di.info_ "Running mainloop"
    app settings

main :: IO ()
main = Di.new $ \di -> Di.runDiT di $ do
    options <- liftIO parseOptions

    let minLevel = if optionsLogDetail options then Debug else Info
    Di.filter (\level _ _ -> level >= minLevel) $ do
        let resolvConf = defaultResolvConf { resolvInfo = RCFilePath (optionsDNSResolvConf options)
                                           }
        resolvSeed <- liftIO $ makeResolvSeed resolvConf

        let settings = Settings { settingsInterval = optionsDNSRefreshInterval options * 1000000
                                , settingsRecordType = optionsDNSRecordType options
                                , settingsDomain = optionsDNSDomain options
                                }

        let evaluateSettings = over Dhall.substitutions addSubstitutions Dhall.defaultEvaluateSettings
        render <- liftIO $ Dhall.detailed $ Dhall.inputFileWithSettings evaluateSettings Dhall.auto (optionsTemplate options)

        let renderAndMaybeValidate as = do
                let rendered = encodeUtf8 $ render as
                if not (optionsConfigTest options)
                   then return rendered
                   else do
                       rc <- testConfig rendered
                       case rc of
                           ExitSuccess -> return rendered
                           ExitFailure n -> Di.attr "rc" n $ do
                               Di.emergency_ "Rendered configuration was rejected by keepalived"
                               throwM rc

        let (configPath, pidPath, render', act) =
                if optionsDumpConfig options
                   then ("/dev/null", "/dev/null", return . encodeUtf8 . render,
                           dumpConfig (optionsConfigTest options) settings >>= liftIO . exitWith)
                   else (optionsConfigFile options, optionsPidFile options, renderAndMaybeValidate,
                           mainloop settings)

        runIO resolvSeed render' configPath pidPath $ runLoggingT act
