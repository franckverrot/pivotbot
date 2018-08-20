module Main where

import Lib (trackerApi, Command(..))
import ClientConfig (fetchClientConfig)

main :: IO ()
main = fetchClientConfig >>= flip trackerApi ListStories
