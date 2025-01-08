{-# LANGUAGE TemplateHaskell #-}

module Lib.Types where

import Brick
import Brick.BChan (BChan)
import Brick.Keybindings
import Brick.Widgets.Edit (Editor)
import Brick.Widgets.List
import Control.Concurrent.STM
import Data.Text (Text)
import Data.Time (TimeZone)
import Data.Vector (Vector)
import Lens.Micro.Platform
import Lib.Reddit.Types

data AppState = AppState
  { _accessToken :: !Text
  , _user :: !(Maybe User)
  , _posts :: !(List Name Post)
  , _focusSubredditSearch :: !Bool
  , _searchSubreddit :: !(Editor Text Name)
  , _subreddits :: !(List Name Subreddit)
  , _currentSubreddit :: !Subreddit
  , _postComments :: !(Vector CommentOrMore)
  , _after :: !(TMVar (Maybe Text))
  , _showHelp :: !Bool
  , _showSubreddit :: !Bool
  , _showPost :: !Bool
  , _keyConfig :: !(KeyConfig KeyEvent)
  , _dispatcher :: !(KeyDispatcher KeyEvent (EventM Name AppState))
  , _bchan :: !(BChan CustomEvent)
  , _tz :: !TimeZone
  }

data Name
  = PostsName
  | PostName
  | SubredditsName
  | SubredditSearchName
  deriving (Show, Eq, Ord)

data KeyEvent
  = QuitEvent
  | ShowHelpEvent
  | ShowSubredditEvent
  | RefreshEvent
  | OpenPostUrlEvent
  | OpenPostCommentEvent
  | OpenPostEvent
  deriving (Show, Eq, Ord)
data CustomEvent
  = GetPosts
  | Posts !(Vector Post)
  | GetMorePosts
  | MorePosts !(Vector Post)
  | GetSubreddits
  | Subreddits !(Vector Subreddit)
  | GetUserData
  | UserData !User
  | GetPostComment !Text
  | PostComments !(Vector CommentOrMore)
  deriving (Show, Eq, Ord)

makeLenses ''AppState
