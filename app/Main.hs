module Main where

import Lib
import ClientConfig (fetchClientConfig)

main :: IO ()
main = fetchClientConfig >>= trackerApi
