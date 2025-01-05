{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Reddit.Api where

import Control.Exception
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Vector (Vector)
import Lib.Reddit.Types
import Network.HTTP.Conduit
import Network.HTTP.Simple

userAgent :: Header
userAgent = ("User-Agent", "haskell-tui 0.1.0.0")

baseUrl :: String
baseUrl = "https://oauth.reddit.com"

getEndpoint :: (FromJSON a) => Text -> Text -> Query -> IO a
getEndpoint atoken path q = do
  req <- parseRequest $ baseUrl <> T.unpack path

  getResponseBody
    <$> ( httpJSON $
            addToRequestQueryString
              q
              req
                { requestHeaders =
                    userAgent
                      : ("Authorization", "bearer " <> encodeUtf8 atoken)
                      : requestHeaders req
                , queryString = "raw_json=1"
                }
        )

getSubreddits :: Text -> IO (Vector Subreddit)
getSubreddits atoken = getSubreddits' Nothing `catch` \(_ :: SomeException) -> return mempty
 where
  getSubreddits' a = do
    Listing{..} :: Listing Subreddit <-
      getEndpoint
        atoken
        "/subreddits/mine/subscriber"
        $ [("sr_detail", Just "0"), ("limit", Just "100"), ("where", Just "subscriber")]
          <> maybe mempty (pure . ("after",) . Just . encodeUtf8) a

    case after of
      Nothing -> return children
      after'@(Just _) -> (children <>) <$> getSubreddits' after'
