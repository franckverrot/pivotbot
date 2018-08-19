module ClientConfig
  ( ClientConfig(..)
  , fetchClientConfig
  )
where

import qualified System.Environment as Env

data ClientConfig = ClientConfig { apiToken :: String
                                 , projectId :: Int
                                 }

fetchClientConfig :: IO ClientConfig
fetchClientConfig = do
  apiToken <- lookupEnv "TRACKER_API_TOKEN"
  projectId <- lookupEnv "TRACKER_PROJECT_ID"
  return $ ClientConfig apiToken (read projectId :: Int)


lookupEnv :: String -> IO String
lookupEnv envVarName =
    Env.lookupEnv envVarName >>=
      maybe (fail $ "missing " ++ envVarName) return

