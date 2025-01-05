module Lib.Types where

import Brick
import Brick.BChan (BChan)
import Brick.Keybindings
import Brick.Widgets.List
import Control.Concurrent.STM
import Data.Text (Text)
import Data.Vector (Vector)
import Lib.Reddit.Types

data AppState = AppState
  { accessToken :: !Text
  , user :: !(Maybe User)
  , posts :: !(List Name Post)
  , subreddits :: !(List Name Subreddit)
  , currentSubreddit :: !Subreddit
  , after :: !(TMVar (Maybe Text))
  , showHelp :: !Bool
  , showSubreddit :: !Bool
  , showPostPreview :: !Bool
  , keyConfig :: !(KeyConfig KeyEvent)
  , dispatcher :: !(KeyDispatcher KeyEvent (EventM Name AppState))
  , bchan :: !(BChan CustomEvent)
  }

data Name = PostsName | SubredditsName deriving (Show, Eq, Ord)

data KeyEvent
  = QuitEvent
  | ShowHelpEvent
  | ShowSubredditEvent
  | RefreshEvent
  | OpenPostUrlEvent
  | ShowPostPreviewEvent
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
  deriving (Show, Eq, Ord)
