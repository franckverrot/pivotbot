{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Lib (trackerApi, Command(..))
import ClientConfig (fetchClientConfig)
import qualified System.Environment as Env
import qualified Options.Applicative as Opts
import Control.Applicative ((<**>))
import Data.String (IsString, fromString)
import qualified Data.Either as Either

import Story


readStates :: Opts.ReadM State
readStates = Opts.eitherReader decode
               where
                 decode :: String -> Either String State
                 decode s = case s of
                   "unstarted" -> Right Unstarted
                   "started" -> Right Started
                   unknown -> Left $ "unknown state \"" ++ unknown ++ "\""

parser :: Opts.Parser Command
parser = showStoryParser Opts.<|> listStoriesParser
  where showStoryParser =
          (ShowStory <$> Opts.option Opts.auto (Opts.long "show-story"))
        listStoriesParser =
          (ListStories <$> Opts.option readStates (Opts.long "list-stories"))

main :: IO ()
main = do
  command <- Opts.execParser opts

  executeCommand command

  where
    opts = Opts.info (parser <**> Opts.helper)
               ( Opts.fullDesc
               <> Opts.progDesc "Instant access to your stories"
               <> Opts.header "pivotbot – A Pivotal Tracker command-line tool" )

    terminateProgram err = do
      showHelpText Opts.defaultPrefs opts

    executeCommand command =
      fetchClientConfig >>= \config -> trackerApi config command

    showHelpText :: Opts.ParserPrefs -> Opts.ParserInfo a -> IO ()
    showHelpText pprefs pinfo =
      Opts.handleParseResult . Opts.Failure $ Opts.parserFailure pprefs pinfo Opts.ShowHelpText mempty
