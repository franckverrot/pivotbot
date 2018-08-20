{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Lib (trackerApi, Command(..))
import ClientConfig (fetchClientConfig)
import qualified System.Environment as Env
import qualified Options.Applicative as Opts
import Control.Applicative ((<**>))
import Data.Semigroup ((<>))
import Data.String (IsString, fromString)
import qualified System.Exit as Exit (die)


type OptionCommand = Either String Command
data Options = Options { action :: OptionCommand
                       }
                       deriving (Show)

instance IsString OptionCommand where
  fromString = \str -> case str of
                         "list-stories" -> Right ListStories
                         unknown -> Left $ unknown ++ " is not a valid command"

parser :: Opts.Parser Options
parser = Options
           <$> Opts.strOption
             ( Opts.long "action"
             <> Opts.metavar "ACTION"
             <> Opts.help "Action to be completed (list-stories, ..)" )

main :: IO ()
main = do
  options <- Opts.execParser opts

  either terminateProgram executeCommand (action options)

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
