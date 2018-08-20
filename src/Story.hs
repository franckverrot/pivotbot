module Story where

import qualified Data.Aeson as JSON
import qualified Data.Maybe as Maybe
import qualified GHC.Generics as Generics
import qualified Text.Printf as Printf
import qualified Data.Text as Text

{- Story -}
data Story = Story { kind :: String
                   , story_type :: Type
                   , estimate :: Maybe Int
                   , current_state :: State
                   , name :: String
                   } deriving (Generics.Generic)

instance Show Story where
  show story =
    Printf.printf "[%-8s] %-64s (%s)" storyType storyName storyEstimate
    where
      storyType     = show . story_type $ story
      storyName     = name story
      storyEstimate = (Maybe.fromMaybe "-" (fmap show $ estimate story))

instance JSON.FromJSON Story


{- Type -}
data Type = Feature
          | Chore
          | Bug
          | Release
          deriving (Generics.Generic, Show)

instance JSON.FromJSON Type where
  parseJSON = JSON.withText "Type"
                $ \type' ->
                    case type' of
                      "feature" -> return Feature
                      "chore"   -> return Chore
                      "bug"     -> return Bug
                      "release" -> return Release
                      unknown -> fail $ "type " ++ Text.unpack unknown ++ "is unknown"


{- State -}
data State = Unstarted
           | Started
           | Delivered
           | Finished
           | Rejected
           deriving (Generics.Generic, Show)

instance JSON.FromJSON State where
  parseJSON = JSON.withText "State"
                $ \state ->
                    case state of
                      "unstarted" -> return Unstarted
                      unknown -> fail $ "state " ++ Text.unpack unknown ++ "is unknown"
