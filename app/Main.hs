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

  Token{..} <- getToken tokenPath oauth

  bchan <- newBChan 10

  let kc = newKeyConfig allKeyEvents defaultBindings []

  d <- case keyDispatcher kc handlers of
    Right d -> return d
    Left _ -> exitFailure

  tz <- getCurrentTimeZone
  aftertm <- newTMVarIO Nothing

  let initialState =
        AppState
          { accessToken = accessToken
          , user = Nothing
          , posts = list PostsName mempty 1
          , focusSubredditSearch = False
          , searchSubreddit = editor SubredditSearchName (Just 1) ""
          , subreddits = list SubredditsName defaultSubreddits 1
          , currentSubreddit = Vec.head defaultSubreddits
          , postComments = mempty
          , after = aftertm
          , showHelp = False
          , showSubreddit = False
          , showPost = False
          , keyConfig = kc
          , dispatcher = d
          , bchan = bchan
          , tz = tz
          }

  let app =
        App
          { appDraw = drawUi
          , appHandleEvent = appEvent
          , appStartEvent = do
              liftIO $ do
                writeBChan bchan GetPosts
                writeBChan bchan GetSubreddits
                writeBChan bchan GetUserData
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
  void $ customMain initialVty buildVty (Just bchan) app initialState

drawUi :: AppState -> [Widget Name]
drawUi AppState{..} = [keybindingHelp, subredditList, postPreview, postList]
 where
  subredditList =
    if showSubreddit
      then
        borderWithLabel (txt "Subreddits")
          . hLimit 30
          $ renderSearch
            <=> ( withVScrollBars OnRight $
                    renderList renderSubreddit showSubreddit subreddits
                )
      else emptyWidget

  renderSubreddit _ Subreddit{..} = txt $ "/r/" <> displayName

  renderSearch = renderEditor (txt . T.unlines) focusSubredditSearch searchSubreddit

  postList =
    joinBorders . borderWithLabel (txt "Posts") $
      topBar
        <=> (withVScrollBars OnRight $ renderList renderPost (not showSubreddit) posts)

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
    if showPost
      then case listSelectedElement posts of
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
                `withAttr` (str . iso8601Show . utcToLocalTime tz) (posixSecondsToUTCTime created)
            , str " by "
            , attrName "author" `withAttr` txt author
            ]
      , txtWrap selftext
      , hBorder
      , comments
      ]

  comments = vBox . Vec.toList $ Vec.map renderComment postComments

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
              [ maybe emptyWidget renderUser user
              ]
          )

  keybindingHelp =
    if showHelp
      then
        centerLayer . borderWithLabel (txt "Help") . padAll 1 $
          keybindingHelpWidget keyConfig handlers
      else emptyWidget

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent be@(VtyEvent e@(EvKey k mods)) = do
  AppState{..} <- get

  when (not showSubreddit && not showPost) $ do
    nl <- nestEventM' posts (handleListEventVi handleListEvent e)
    let total = Vec.length . listElements $ nl
    let isLast = case listSelected nl of
          Nothing -> True
          Just n -> n == total - 1
    modify $ \st -> st{posts = nl}
    when
      isLast
      (void . liftIO $ writeBChanNonBlocking bchan GetMorePosts)

  when showPost $ do
    handleViewportEventVi handleViewportEvent PostName e

  when showSubreddit $ do
    if focusSubredditSearch
      then do
        ne <- nestEventM' searchSubreddit (handleEditorEvent be)
        modify $ \st -> st{searchSubreddit = ne}
        case e of
          EvKey KEnter [] -> do
            let sr =
                  Subreddit "" "" $
                    "/r/" <> (T.strip . T.unlines) (getEditContents searchSubreddit) <> "/"
            modify $ \st@AppState{searchSubreddit = ne'} ->
              st
                { currentSubreddit = sr
                , focusSubredditSearch = False
                , showSubreddit = False
                , searchSubreddit = applyEdit clearZipper ne'
                }
            liftIO $ writeBChan bchan GetPosts
          _ -> return ()
      else do
        nl <- nestEventM' subreddits (handleListEventVi handleListEvent e)
        modify $ \st -> st{subreddits = nl}
        case e of
          EvKey KEnter [] -> do
            case listSelectedElement nl of
              Just (_, sr) -> do
                modify $ \st -> st{currentSubreddit = sr, showSubreddit = False}
                liftIO $ writeBChan bchan GetPosts
              Nothing -> return ()
          _ -> return ()
    case e of
      EvKey (KChar '/') [] -> do
        modify $ \st -> st{focusSubredditSearch = True}
      EvKey (KEsc) [] -> do
        when focusSubredditSearch $ modify $ \st -> st{focusSubredditSearch = False}
      _ -> return ()

  case e of
    EvKey (KChar 'c') [MCtrl] -> halt
    _ -> return ()
  unless focusSubredditSearch $ do
    void $ handleKey dispatcher k mods
appEvent (AppEvent GetPosts) = do
  AppState{currentSubreddit = Subreddit{..}, ..} <- get
  void . liftIO . forkIO $ do
    _ <- atomically $ takeTMVar after
    Listing{children, after = after''} :: Listing Post <-
      getEndpoint
        accessToken
        url
        [("limit", Just "100")]
        `catch` \(_ :: JSONException) -> return mempty
    writeBChan bchan $ Posts children
    atomically $ putTMVar after after''
appEvent (AppEvent (Posts children)) = do
  modify $ \st@AppState{..} ->
    st
      { posts =
          listReplace
            children
            (Just 0)
            posts
      }
appEvent (AppEvent GetMorePosts) = do
  AppState{currentSubreddit = Subreddit{..}, ..} <- get

  void . liftIO . forkIO $ do
    after' <- atomically $ takeTMVar after
    Listing{children, after = after''} :: Listing Post <-
      getEndpoint
        accessToken
        url
        [("limit", Just "100"), ("after", encodeUtf8 <$> after')]
    writeBChan bchan $ MorePosts children
    atomically $ putTMVar after after''
appEvent (AppEvent (MorePosts children)) = do
  st@AppState{..} <- get
  put
    st
      { posts =
          listReplace
            (listElements posts <> children)
            (listSelected posts <|> Just 0)
            posts
      }
appEvent (AppEvent GetSubreddits) = do
  AppState{..} <- get
  void . liftIO . forkIO $ do
    srs <- Vec.modify VA.sort <$> getSubreddits accessToken
    writeBChan bchan (Subreddits srs)
appEvent (AppEvent (Subreddits srs)) = do
  modify $ \st@AppState{..} ->
    st
      { subreddits =
          listReplace
            (listElements subreddits <> srs)
            (listSelected subreddits <|> Just 0)
            subreddits
      }
appEvent (AppEvent GetUserData) = do
  AppState{..} <- get
  void . liftIO . forkIO $ do
    u <- getEndpoint accessToken "/api/v1/me" []
    writeBChan bchan (UserData u)
appEvent (AppEvent (UserData u)) = do
  modify $ \st -> st{user = Just u}
appEvent (AppEvent (GetPostComment pid)) = do
  AppState{..} <- get
  void . liftIO . forkIO $ do
    (_, Listing{children}) :: (Listing Post, Listing CommentOrMore) <-
      getEndpoint
        accessToken
        ("/comments/" <> pid)
        [("depth", Just "3"), ("limit", Just "1000")]
    writeBChan bchan (PostComments children)
appEvent (AppEvent (PostComments cmts)) = do
  modify $ \st -> st{postComments = cmts}
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
      (modify $ \st@AppState{showHelp} -> st{showHelp = not showHelp})
  , onEvent
      ShowSubredditEvent
      "Show subreddits"
      (modify $ \st@AppState{..} -> st{showSubreddit = not showSubreddit})
  , onEvent
      RefreshEvent
      "Refresh data"
      $ do
        AppState{..} <- get
        liftIO $ do
          writeBChan bchan GetUserData
          writeBChan bchan GetPosts
          writeBChan bchan GetSubreddits
  , onEvent
      OpenPostUrlEvent
      "Open post in browser"
      $ do
        AppState{..} <- get
        when (not showSubreddit) $ do
          case listSelectedElement posts of
            Just (_, Post{..}) -> do
              liftIO $ openInBrowser url
            Nothing -> return ()
  , onEvent
      OpenPostCommentEvent
      "Open post comments in browser"
      $ do
        AppState{..} <- get
        when (not showSubreddit) $ do
          case listSelectedElement posts of
            Just (_, Post{..}) -> do
              liftIO $ openInBrowser ("https://old.reddit.com" <> permalink)
            Nothing -> return ()
  , onEvent OpenPostEvent "Open post" $ do
      AppState{..} <- get
      when (not showSubreddit) $ do
        modify $ \st -> st{showPost = not showPost}
      if (not showPost)
        then do
          case listSelectedElement posts of
            Just (_, Post{..}) ->
              liftIO $ writeBChan bchan (GetPostComment postId)
            _ -> return ()
        else do
          modify $ \st -> st{postComments = mempty}
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
