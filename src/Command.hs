module Command where

import RequestParameters
import Story
import Data.Function ((&))
import qualified Data.Text as Text

type StoryId = Int
data Command = ListStories Story.State
             | ShowStory StoryId
             deriving (Show)

data ResultType = SingleEntry
                | MultipleEntry

class HasResultType a where
  commandType :: a -> ResultType

instance HasResultType Command where
  commandType (ListStories _) = MultipleEntry
  commandType (ShowStory _) = SingleEntry

instance IsRequestParams Command where
  toRequestParams (ListStories state) =
    RequestParameters { resource = "/stories", filters = [filters] }
      where
        filters = (Text.unpack "with_state", stateToString state)

        stateToString Started = Text.unpack "started"
        stateToString Unstarted = Text.unpack  "unstarted"

  toRequestParams (ShowStory storyId) =
    RequestParameters { resource = "/stories/" ++ show storyId, filters = [] }
