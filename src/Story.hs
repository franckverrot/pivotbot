module Story where

import qualified Data.Aeson as JSON
import qualified Data.Maybe as Maybe
import qualified GHC.Generics as Generics
import qualified Text.Printf as Printf

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

