module Lib
    ( trackerApi
    ) where

import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Simple as Simple

import Data.Function ((&))
import ClientConfig
import Story

hTrackerTokenHeader :: Header.HeaderName
hTrackerTokenHeader = "X-TrackerToken"

makeHeaders :: String -> [Header.Header]
makeHeaders token =
    [
      (hTrackerTokenHeader, BS.pack token)
    ]

makeRequest :: ClientConfig -> Simple.Request
makeRequest (ClientConfig apiToken projectId) = do
  let targetPath = "/services/v5/projects/" ++ show projectId ++ "/stories"
  let targetUrl = "www.pivotaltracker.com"
  Simple.defaultRequest
    & Simple.setRequestHost targetUrl
    & Simple.setRequestPort 443
    & Simple.setRequestSecure True
    & Simple.setRequestPath (BS.pack targetPath)
    & Simple.setRequestQueryString [("with_state", Just "unstarted")]
    & Simple.setRequestHeader hTrackerTokenHeader [BS.pack apiToken]

trackerApi :: IO ()
trackerApi = do
  clientConfig <- ClientConfig.fetchClientConfig

  let request = makeRequest clientConfig

  response <- Simple.httpLBS request
  let body = Simple.getResponseBody response
  let stories = JSON.decode body :: Maybe [Story]

  case stories of
    Nothing -> putStrLn "Nothing to show!"
    Just ary -> mapM_ (putStrLn . show) ary
