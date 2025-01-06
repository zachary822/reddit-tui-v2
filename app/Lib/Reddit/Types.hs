{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Reddit.Types where

import Control.Applicative
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

instance Semigroup (Listing a) where
  Listing c b a <> Listing c' b' a' = Listing (c <> c') (b <> b') (a <> a')

instance Monoid (Listing a) where
  mempty = Listing mempty mempty mempty

instance (FromJSON a) => FromJSON (Listing a) where
  parseJSON = withObject "Listing" $ \o -> do
    k :: Text <- o .: "kind"
    guard (k == "Listing")

    d <- o .: "data"

    Listing <$> d .: "children" <*> d .:? "before" <*> d .: "after"

data Post = Post
  { postId :: !Text
  , title :: !Text
  , subreddit :: !Text
  , author :: !Text
  , score :: !Integer
  , selftext :: !Text
  , numComments :: !Integer
  , url :: !Text
  , permalink :: !Text
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
      <*> d .: "score"
      <*> d .: "selftext"
      <*> d .: "num_comments"
      <*> d .: "url"
      <*> d .: "permalink"
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

data Comment = Comment
  { commentId :: !Text
  , author :: !Text
  , score :: !Integer
  , body :: !Text
  , depth :: !Integer
  , replies :: !(Listing CommentOrMore)
  }
  deriving (Generic, Show, Eq)

instance Ord Comment where
  Comment{commentId} `compare` Comment{commentId = commentId'} = commentId `compare` commentId'

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    k :: Text <- o .: "kind"
    guard (k == "t1")
    d <- o .: "data"

    Comment
      <$> d .: "id"
      <*> d .: "author"
      <*> d .: "score"
      <*> d .: "body"
      <*> d .: "depth"
      <*> ( d .: "replies"
              <|> return mempty
          )

data More = More
  { moreId :: !Text
  , depth :: !Integer
  , children :: ![Text]
  }
  deriving (Generic, Show, Eq)

instance Ord More where
  More{moreId} `compare` More{moreId = moreId'} = moreId `compare` moreId'

instance FromJSON More where
  parseJSON = withObject "More" $ \o -> do
    k :: Text <- o .: "kind"
    guard (k == "more")
    d <- o .: "data"
    More
      <$> d .: "id"
      <*> d .: "depth"
      <*> d .: "children"

data CommentOrMore
  = Cmt Comment
  | Mr More
  deriving (Generic, Show, Eq, Ord)

instance FromJSON CommentOrMore where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}
