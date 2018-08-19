{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( trackerApi
    ) where

import qualified GHC.Generics as Generics
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Text.Printf as Printf
import qualified Data.Aeson as JSON
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Simple as Simple
import Data.Function

hTrackerTokenHeader :: Header.HeaderName
hTrackerTokenHeader = "X-TrackerToken"

makeHeaders :: String -> [Header.Header]
makeHeaders token =
    [
      (hTrackerTokenHeader, BS.pack token)
    ]

data Story = Story { kind :: String
                   , story_type :: String
                   , estimate :: Maybe Int
                   , current_state :: String
                   , name :: String
                   } deriving (Generics.Generic)

instance Show Story where
  show story =
    Printf.printf "[%-8s] %-64s (%s)" storyType storyName storyEstimate
    where
      storyType = story_type story
      storyName = name story
      storyEstimate = (Maybe.fromMaybe "-" (fmap show $ estimate story))

instance JSON.FromJSON Story

trackerApi :: IO ()
trackerApi = do
  manager <- HTTP.newManager TLS.tlsManagerSettings

  let targetUrl = "www.pivotaltracker.com"
  token <- do
    Env.lookupEnv "TRACKER_API_TOKEN" >>=
      maybe (fail $ "missing TRACKER_API_TOKEN") return

  trackerProjectId <- do
    Env.lookupEnv "TRACKER_PROJECT_ID" >>=
      maybe (fail $ "missing TRACKER_PROJECT_ID") return

  let targetPath = "/services/v5/projects/" ++ trackerProjectId ++ "/stories"

  let request = Simple.defaultRequest
                & Simple.setRequestHost targetUrl
                & Simple.setRequestPort 443
                & Simple.setRequestSecure True
                & Simple.setRequestPath (BS.pack targetPath)
                & Simple.setRequestQueryString [("with_state", Just "unstarted")]
                & Simple.setRequestHeader hTrackerTokenHeader [BS.pack token]

  response <- Simple.httpLBS request
  let body = Simple.getResponseBody response
  let stories = JSON.decode body :: Maybe [Story]

  case stories of
    Nothing -> putStrLn "Nothing to show!"
    Just ary -> mapM_ (putStrLn . show) ary
