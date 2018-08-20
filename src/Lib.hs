module Lib
    ( trackerApi
    , Command(..)
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
import Command
import RequestParameters

hTrackerTokenHeader :: Header.HeaderName
hTrackerTokenHeader = "X-TrackerToken"

makeHeaders :: String -> [Header.Header]
makeHeaders token =
    [
      (hTrackerTokenHeader, BS.pack token)
    ]

makeRequest :: ClientConfig -> RequestParameters -> Simple.Request
makeRequest (ClientConfig apiToken projectId) requestParams = do
  let targetPath = "/services/v5/projects/" ++ show projectId ++ (resource requestParams)
      targetUrl = "www.pivotaltracker.com"
      port = 443
      secure = True
      path = (BS.pack targetPath)
      queryString = fmap (\(x,y) -> (BS.pack x, Just $ BS.pack y)) $ filters requestParams
  Simple.defaultRequest
    & Simple.setRequestHost targetUrl
    & Simple.setRequestPort port
    & Simple.setRequestSecure secure
    & Simple.setRequestPath path
    & Simple.setRequestQueryString queryString
    & Simple.setRequestHeader hTrackerTokenHeader [BS.pack apiToken]


trackerApi :: ClientConfig -> Command -> IO ()
trackerApi clientConfig command = do
  let request = makeRequest clientConfig $ commandToRequestParams command

  response <- Simple.httpLBS request
  let body = Simple.getResponseBody response
  let stories = JSON.decode body :: Maybe [Story]

  case stories of
    Nothing -> putStrLn "Nothing to show!"
    Just ary -> mapM_ (putStrLn . show) ary
