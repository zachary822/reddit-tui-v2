{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Reddit.Types where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)

data Oauth = Oauth
  { clientId :: !String
  , redirectUri :: !String
  , duration :: !Duration
  , scope :: ![String]
  }
  deriving (Show, Eq)

data Duration = Temporary | Permanent deriving (Eq)

instance Show Duration where
  show Temporary = "temporary"
  show Permanent = "permanent"

data Token = Token
  { accessToken :: !Text
  , tokenType :: !Text
  , expiresIn :: !Int
  , refreshToken :: !Text
  , scope :: !Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Token where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

instance ToJSON Token where
  toEncoding = genericToEncoding defaultOptions{fieldLabelModifier = camelTo2 '_'}

data Listing a = Listing
  { children :: Vector a
  , before :: !(Maybe Text)
  , after :: !(Maybe Text)
  }
  deriving (Generic, Show, Eq)

instance (FromJSON a) => FromJSON (Listing a) where
  parseJSON = withObject "Listing" $ \o -> do
    k :: Text <- o .: "kind"
    guard (k == "Listing")

    d <- o .: "data"

    Listing <$> d .: "children" <*> d .:? "before" <*> d .: "after"

data Post = Post
  { postId :: !String
  , title :: !Text
  , subreddit :: !Text
  , author :: !Text
  , ups :: !Integer
  , downs :: !Integer
  , selftext :: !Text
  , url :: !Text
  , created :: !POSIXTime
  }
  deriving (Generic, Show, Eq)

instance Ord Post where
  Post{postId} `compare` Post{postId = postId'} = postId `compare` postId'

instance FromJSON Post where
  parseJSON = withObject "Post" $ \o -> do
    k :: Text <- o .: "kind"
    guard (k == "t3")
    d <- o .: "data"

    Post
      <$> d .: "id"
      <*> d .: "title"
      <*> d .: "subreddit"
      <*> d .: "author"
      <*> d .: "ups"
      <*> d .: "downs"
      <*> d .: "selftext"
      <*> d .: "url"
      <*> d .: "created_utc"

data Subreddit = Subreddit
  { subredditId :: !Text
  , displayName :: !Text
  , url :: !Text
  }
  deriving (Generic, Show, Eq)

instance Ord Subreddit where
  Subreddit{displayName = n} `compare` Subreddit{displayName = n'} = T.toCaseFold n `compare` T.toCaseFold n'

instance FromJSON Subreddit where
  parseJSON = withObject "Subreddit" $ \o -> do
    k :: Text <- o .: "kind"
    guard (k == "t5")

    d <- o .: "data"

    Subreddit
      <$> d .: "id"
      <*> d .: "display_name"
      <*> d .: "url"

defaultSubreddits :: Vector Subreddit
defaultSubreddits =
  V.fromList
    [ Subreddit
        { subredditId = mempty
        , displayName = "home"
        , url = "/"
        }
    , Subreddit
        { subredditId = mempty
        , displayName = "best"
        , url = "/best"
        }
    , Subreddit
        { subredditId = mempty
        , displayName = "hot"
        , url = "/hot"
        }
    , Subreddit
        { subredditId = mempty
        , displayName = "new"
        , url = "/new"
        }
    , Subreddit
        { subredditId = mempty
        , displayName = "top"
        , url = "/top"
        }
    , Subreddit
        { subredditId = mempty
        , displayName = "controversial"
        , url = "/controversial"
        }
    ]

data User = User
  { userId :: !Text
  , name :: !Text
  , linkKarma :: !Integer
  , commentKarma :: !Integer
  , created :: !POSIXTime
  }
  deriving (Generic, Show, Eq)

instance Ord User where
  User{userId} `compare` User{userId = userId'} = userId `compare` userId'

instance FromJSON User where
  parseJSON = withObject "user" $ \o ->
    User
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "link_karma"
      <*> o .: "comment_karma"
      <*> o .: "created_utc"
