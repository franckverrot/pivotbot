module Command where

import RequestParameters

type StoryId = Int
data Command = ListStories
             | ShowStory StoryId
             deriving (Show)

commandToRequestParams :: Command -> RequestParameters
commandToRequestParams ListStories         = RequestParameters { resource = "/stories", filters = [("with_state", "unstarted")] }
commandToRequestParams (ShowStory storyId) = RequestParameters { resource = "/stories/" ++ show storyId, filters = [] }
