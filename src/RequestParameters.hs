module RequestParameters where

data RequestParameters = RequestParameters { filters :: [(String, String)]
                                           , resource :: String
                                           }

class IsRequestParams a where
  toRequestParams :: a -> RequestParameters
