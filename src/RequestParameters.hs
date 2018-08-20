module RequestParameters where

data RequestParameters = RequestParameters { filters :: [(String, String)]
                                           , resource :: String
                                           }

