{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick
import Brick.BChan
import Brick.Keybindings
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit (
  applyEdit,
  editor,
  getEditContents,
  handleEditorEvent,
  renderEditor,
 )
import Brick.Widgets.List
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Zipper
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as Vec
import Data.Vector.Algorithms.Merge qualified as VA
import Graphics.Vty (
  Event (..),
  Key (..),
  Modifier (..),
  setWindowTitle,
  userConfig,
 )
import Graphics.Vty.Attributes
import Graphics.Vty.Platform.Unix (mkVty)
import Lens.Micro.Platform
import Lib.Reddit.Api
import Lib.Reddit.Oauth
import Lib.Reddit.Types
import Lib.Types
import Lib.Utils (openInBrowser)
import Network.HTTP.Simple (JSONException)
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import Text.Printf (printf)

main :: IO ()
main = do
  path <- getXdgDirectory XdgConfig "reddit-tui"
  createDirectoryIfMissing False path

  let tokenPath = path </> "refresh_token"

  let oauth =
        Oauth
          { clientId = "uZXqaoPoTM6rcxDWjO5rhA"
          , redirectUri = "http://localhost:3000/oauth2/callback"
          , duration = Permanent
          , scope =
              ["submit", "save", "read", "history", "subscribe", "mysubreddits", "identity"]
          }

  Token{accessToken = atoken} <- getToken tokenPath oauth

  _bchan <- newBChan 10

  let kc = newKeyConfig allKeyEvents defaultBindings []

  d <- case keyDispatcher kc handlers of
    Right d -> return d
    Left _ -> exitFailure

  _tz <- getCurrentTimeZone
  aftertm <- newTMVarIO Nothing

  let initialState =
        AppState
          { _accessToken = atoken
          , _user = Nothing
          , _posts = list PostsName mempty 1
          , _focusSubredditSearch = False
          , _searchSubreddit = editor SubredditSearchName (Just 1) ""
          , _subreddits = list SubredditsName defaultSubreddits 1
          , _currentSubreddit = Vec.head defaultSubreddits
          , _postComments = mempty
          , _after = aftertm
          , _showHelp = False
          , _showSubreddit = False
          , _showPost = False
          , _keyConfig = kc
          , _dispatcher = d
          , _bchan = _bchan
          , _tz = _tz
          }

  let app =
        App
          { appDraw = drawUi
          , appHandleEvent = appEvent
          , appStartEvent = do
              liftIO $ do
                writeBChan _bchan GetPosts
                writeBChan _bchan GetSubreddits
                writeBChan _bchan GetUserData
          , appAttrMap = \_ ->
              attrMap
                defAttr
                [ (listSelectedAttr, black `on` white)
                , (attrName "score", defAttr `withForeColor` red `withStyle` bold)
                , (attrName "subreddit", defAttr `withForeColor` yellow)
                , (attrName "title", defAttr `withStyle` bold)
                , (attrName "author", defAttr `withForeColor` cyan)
                , (attrName "time", defAttr `withForeColor` linearColor @Int 150 150 150)
                ]
          , appChooseCursor = showFirstCursor
          }

  let buildVty = do
        v <- userConfig >>= mkVty
        setWindowTitle v "Reddit"
        return v
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just _bchan) app initialState

drawUi :: AppState -> [Widget Name]
drawUi AppState{..} = [keybindingHelp, subredditList, postPreview, postList]
 where
  subredditList =
    if _showSubreddit
      then
        borderWithLabel (txt "Subreddits")
          . hLimit 30
          $ renderSearch
            <=> ( withVScrollBars OnRight $
                    renderList renderSubreddit _showSubreddit _subreddits
                )
      else emptyWidget

  renderSubreddit _ Subreddit{..} = txt $ "/r/" <> displayName

  renderSearch = renderEditor (txt . T.unlines) _focusSubredditSearch _searchSubreddit

  postList =
    joinBorders . borderWithLabel (txt "Posts") $
      topBar
        <=> (withVScrollBars OnRight $ renderList renderPost (not _showSubreddit) _posts)

  renderPost _ Post{..} =
    hBox
      [ txt "("
      , attrName "score" `withAttr` str (printf "%5d" score)
      , txt $ ")"
      , str $ printf " [%d] " numComments
      , txt . fst . T.breakOn "\n" $ title
      , attrName "subreddit" `withAttr` txt (" /r/" <> subreddit)
      ]

  postPreview =
    if _showPost
      then case listSelectedElement _posts of
        Just (_, p) ->
          centerLayer
            . hLimit 80
            . joinBorders
            . border
            . withVScrollBars OnRight
            . viewport PostName Vertical
            $ renderPostPreview p
        Nothing -> emptyWidget
      else emptyWidget

  renderPostPreview Post{..} =
    vBox
      [ withAttr (attrName "title") $ txt title
      , padBottom (Pad 1) $
          hBox
            [ str "created on "
            , attrName "time"
                `withAttr` (str . iso8601Show . utcToLocalTime _tz) (posixSecondsToUTCTime created)
            , str " by "
            , attrName "author" `withAttr` txt author
            ]
      , txtWrap selftext
      , hBorder
      , comments
      ]

  comments = vBox . Vec.toList $ Vec.map renderComment _postComments

  renderComment (Mr _) = txt "more ..."
  renderComment (Cmt Comment{replies = Listing{children}, ..}) =
    padLeft (Pad (if depth > 0 then 2 else 0)) . vBox $
      [ hBox
          [ attrName "author" `withAttr` (txt $ "/u/" <> author)
          , str " ("
          , attrName "score" `withAttr` str (show score)
          , str ")"
          ]
      , txtWrap body
      ]
        <> Vec.toList (Vec.map renderComment children)

  renderUser User{..} = do
    hBox
      [ txt name
      , txt " ("
      , str (printf "%d\x2022%d" linkKarma commentKarma)
      , txt ")"
      ]

  topBar =
    (withAttr (attrName "title") $ txt "Reddit")
      <+> ( padLeft Max . padRight (Pad 1) . hBox $
              [ maybe emptyWidget renderUser _user
              ]
          )

  keybindingHelp =
    if _showHelp
      then
        centerLayer . borderWithLabel (txt "Help") . padAll 1 $
          keybindingHelpWidget _keyConfig handlers
      else emptyWidget

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent be@(VtyEvent e@(EvKey k mods)) = do
  AppState{..} <- get

  when (not _showSubreddit && not _showPost) $ do
    zoom posts (handleListEventVi handleListEvent e)
    total <- Vec.length <$> use (posts . listElementsL)
    isLast <- maybe True (== (total - 1)) <$> use (posts . listSelectedL)
    when
      isLast
      (void . liftIO $ writeBChanNonBlocking _bchan GetMorePosts)

  when _showPost $ do
    handleViewportEventVi handleViewportEvent PostName e

  when _showSubreddit $ do
    if _focusSubredditSearch
      then do
        zoom searchSubreddit (handleEditorEvent be)
        case e of
          EvKey KEnter [] -> do
            let sr =
                  Subreddit "" "" $
                    "/r/" <> (T.strip . T.unlines) (getEditContents _searchSubreddit) <> "/"
            modify $ \st@AppState{_searchSubreddit = ne'} ->
              st
                { _currentSubreddit = sr
                , _focusSubredditSearch = False
                , _showSubreddit = False
                , _searchSubreddit = applyEdit clearZipper ne'
                }
            liftIO $ writeBChan _bchan GetPosts
          _ -> return ()
      else do
        zoom subreddits (handleListEventVi handleListEvent e)
        case e of
          EvKey KEnter [] -> do
            preuse (subreddits . listSelectedElementL) >>= \case
              Just sr -> do
                currentSubreddit .= sr
                showSubreddit .= False
                liftIO $ writeBChan _bchan GetPosts
              Nothing -> return ()
          _ -> return ()
    case e of
      EvKey (KChar '/') [] -> do
        focusSubredditSearch .= True
      EvKey (KEsc) [] -> do
        focusSubredditSearch .= False
      _ -> return ()

  case e of
    EvKey (KChar 'c') [MCtrl] -> halt
    _ -> return ()
  unless _focusSubredditSearch $ do
    void $ handleKey _dispatcher k mods
appEvent (AppEvent GetPosts) = do
  AppState{_currentSubreddit = Subreddit{..}, ..} <- get
  void . liftIO . forkIO $ do
    _ <- atomically $ takeTMVar _after
    Listing{children, after = after''} :: Listing Post <-
      getEndpoint
        _accessToken
        url
        [("limit", Just "100")]
        `catch` \(_ :: JSONException) -> return mempty
    writeBChan _bchan $ Posts children
    atomically $ putTMVar _after after''
appEvent (AppEvent (Posts children)) = do
  posts %= \ps -> listReplace children (Just 0) ps
appEvent (AppEvent GetMorePosts) = do
  AppState{_currentSubreddit = Subreddit{..}, ..} <- get

  void . liftIO . forkIO $ do
    after' <- atomically $ takeTMVar _after
    Listing{children, after = after''} :: Listing Post <-
      getEndpoint
        _accessToken
        url
        [("limit", Just "100"), ("after", encodeUtf8 <$> after')]
    writeBChan _bchan $ MorePosts children
    atomically $ putTMVar _after after''
appEvent (AppEvent (MorePosts children)) = do
  posts %= \ps -> listReplace (listElements ps <> children) (listSelected ps <|> Just 0) ps
appEvent (AppEvent GetSubreddits) = do
  AppState{..} <- get
  void . liftIO . forkIO $ do
    srs <- Vec.modify VA.sort <$> getSubreddits _accessToken
    writeBChan _bchan (Subreddits srs)
appEvent (AppEvent (Subreddits srs)) = do
  subreddits %= \srs' -> listReplace (listElements srs' <> srs) (listSelected srs' <|> Just 0) srs'
appEvent (AppEvent GetUserData) = do
  AppState{..} <- get
  void . liftIO . forkIO $ do
    u <- getEndpoint _accessToken "/api/v1/me" []
    writeBChan _bchan (UserData u)
appEvent (AppEvent (UserData u)) = user ?= u
appEvent (AppEvent (GetPostComment pid)) = do
  AppState{..} <- get
  void . liftIO . forkIO $ do
    (_, Listing{children}) :: (Listing Post, Listing CommentOrMore) <-
      getEndpoint
        _accessToken
        ("/comments/" <> pid)
        [("depth", Just "3"), ("limit", Just "500")]
    writeBChan _bchan (PostComments children)
appEvent (AppEvent (PostComments cmts)) = do
  postComments .= cmts
appEvent _ = return ()

defaultBindings :: [(KeyEvent, [Binding])]
defaultBindings =
  [ (QuitEvent, [bind 'q', bind KEsc])
  , (ShowHelpEvent, [bind '?'])
  , (ShowSubredditEvent, [bind 's'])
  , (RefreshEvent, [bind 'r'])
  , (OpenPostUrlEvent, [bind 'o'])
  , (OpenPostCommentEvent, [bind 'c'])
  , (OpenPostEvent, [bind ' '])
  ]

allKeyEvents :: KeyEvents KeyEvent
allKeyEvents =
  keyEvents
    [ ("quit", QuitEvent)
    , ("help", ShowHelpEvent)
    , ("subreddit", ShowSubredditEvent)
    , ("refresh", RefreshEvent)
    , ("open", OpenPostUrlEvent)
    , ("comments", OpenPostCommentEvent)
    ]

handlers :: [KeyEventHandler KeyEvent (EventM Name AppState)]
handlers =
  [ onEvent QuitEvent "Quit the program" halt
  , onEvent
      ShowHelpEvent
      "Show help"
      (showHelp %= not)
  , onEvent
      ShowSubredditEvent
      "Show subreddits"
      (showSubreddit %= not)
  , onEvent
      RefreshEvent
      "Refresh data"
      $ do
        AppState{..} <- get
        liftIO $ do
          writeBChan _bchan GetUserData
          writeBChan _bchan GetPosts
          writeBChan _bchan GetSubreddits
  , onEvent
      OpenPostUrlEvent
      "Open post in browser"
      $ do
        st@AppState{..} <- get
        when (not _showSubreddit) $ do
          case st ^? posts . listSelectedElementL of
            Just Post{..} -> do
              liftIO $ openInBrowser url
            Nothing -> return ()
  , onEvent
      OpenPostCommentEvent
      "Open post comments in browser"
      $ do
        AppState{..} <- get
        when (not _showSubreddit) $ do
          case listSelectedElement _posts of
            Just (_, Post{..}) -> do
              liftIO $ openInBrowser ("https://old.reddit.com" <> permalink)
            Nothing -> return ()
  , onEvent OpenPostEvent "Open post" $ do
      st@AppState{..} <- get
      when (not _showSubreddit) $ showPost %= not
      if (not _showPost)
        then do
          case st ^? posts . listSelectedElementL of
            Just Post{..} ->
              liftIO $ writeBChan _bchan (GetPostComment postId)
            _ -> return ()
        else do
          postComments .= mempty
  ]

handleViewportEvent :: n -> Event -> EventM n s ()
handleViewportEvent n e = do
  let vps = viewportScroll n
  case e of
    EvKey KUp [] -> vScrollBy vps (-1)
    EvKey KDown [] -> vScrollBy vps 1
    EvKey KHome [] -> vScrollToBeginning vps
    EvKey KEnd [] -> vScrollToEnd vps
    EvKey KPageDown [] -> vScrollPage vps Down
    EvKey KPageUp [] -> vScrollPage vps Up
    _ -> return ()

handleViewportEventVi ::
  (n -> Event -> EventM n s ()) -> n -> Event -> EventM n s ()
handleViewportEventVi fallback n e = do
  let vps = viewportScroll n

  case e of
    EvKey (KChar 'k') [] -> vScrollBy vps (-1)
    EvKey (KChar 'j') [] -> vScrollBy vps 1
    EvKey (KChar 'g') [] -> vScrollToBeginning vps
    EvKey (KChar 'G') [] -> vScrollToEnd vps
    EvKey (KChar 'f') [MCtrl] -> vScrollPage vps Down
    EvKey (KChar 'b') [MCtrl] -> vScrollPage vps Up
    _ -> fallback n e
