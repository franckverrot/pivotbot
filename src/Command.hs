module Command where

import RequestParameters

type StoryId = Int
data Command = ListStories
             | ShowStory StoryId
             deriving (Show)

instance IsRequestParams Command where
  toRequestParams ListStories =
    RequestParameters { resource = "/stories", filters = [("with_state", "unstarted")] }

  toRequestParams (ShowStory storyId) =
    RequestParameters { resource = "/stories/" ++ show storyId, filters = [] }
