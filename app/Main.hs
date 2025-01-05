{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick
import Brick.BChan
import Brick.Keybindings
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as Vec
import Data.Vector.Algorithms.Merge qualified as VA
import Graphics.Vty (
  Event (..),
  Key (..),
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

  aftertm <- newTMVarIO Nothing

  let initialState =
        AppState
          { accessToken = accessToken
          , user = Nothing
          , posts = list PostsName mempty 1
          , subreddits = list SubredditsName defaultSubreddits 1
          , currentSubreddit = Vec.head defaultSubreddits
          , after = aftertm
          , showHelp = False
          , showSubreddit = False
          , showPostPreview = False
          , keyConfig = kc
          , dispatcher = d
          , bchan = bchan
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
                , (attrName "ups", defAttr `withForeColor` red `withStyle` bold)
                , (attrName "subreddit", defAttr `withForeColor` yellow)
                , (attrName "title", defAttr `withStyle` bold)
                ]
          , appChooseCursor = neverShowCursor
          }

  let buildVty = do
        v <- userConfig >>= mkVty
        setWindowTitle v "Reddit"
        return v
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just bchan) app initialState

drawUi :: AppState -> [Widget Name]
drawUi AppState{..} = [keybindingHelp, postPreview, subredditList, postList]
 where
  subredditList =
    if showSubreddit
      then
        borderWithLabel (txt "Subreddits") . hLimit 30 . withVScrollBars OnRight $
          renderList renderSubreddit showSubreddit subreddits
      else emptyWidget

  renderSubreddit _ Subreddit{..} = txt $ "/r/" <> displayName

  renderUser User{..} = do
    hBox
      [ txt name
      , txt " ("
      , str (printf "%d\x2022%d" linkKarma commentKarma)
      , txt ")"
      ]

  postPreview =
    if showPostPreview
      then do
        maybe emptyWidget (renderPostPreview . snd) (listSelectedElement posts)
      else emptyWidget

  renderPostPreview Post{..} =
    if not (T.null selftext)
      then
        centerLayer . borderWithLabel (txt title) . vLimitPercent 90 . hLimit 90 $
          txtWrap selftext
      else emptyWidget

  topBar =
    (withAttr (attrName "title") $ txt "Reddit")
      <+> ( padLeft Max . padRight (Pad 1) . hBox $
              [ padRight (Pad 1) $ if showPostPreview then txt "P" else emptyWidget
              , maybe emptyWidget renderUser user
              ]
          )

  postList =
    joinBorders . borderWithLabel (txt "Posts") $
      topBar
        <=> ( withVScrollBars OnRight $
                renderList renderPost (not showSubreddit) posts
            )

  renderPost _ Post{..} =
    hBox
      [ txt "("
      , attrName "ups" `withAttr` str (printf "%5d" ups)
      , txt $ ") "
      , if not (T.null selftext) then txt "[+] " else txt "    "
      , txt . fst . T.breakOn "\n" $ title
      , attrName "subreddit" `withAttr` txt (" /r/" <> subreddit)
      ]

  keybindingHelp =
    if showHelp
      then
        centerLayer . borderWithLabel (txt "Help") . padAll 1 $
          keybindingHelpWidget keyConfig handlers
      else emptyWidget

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent (VtyEvent e@(EvKey k mods)) = do
  AppState{..} <- get

  h <- handleKey dispatcher k mods

  unless h $ do
    when (not showSubreddit) $ do
      nl <- nestEventM' posts (handleListEventVi handleListEvent e)
      let total = Vec.length . listElements $ nl
      let isLast = case listSelected nl of
            Nothing -> True
            Just n -> n == total - 1
      modify $ \st -> st{posts = nl}
      when
        isLast
        (void . liftIO $ writeBChanNonBlocking bchan GetMorePosts)

    when showSubreddit $ do
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
appEvent (AppEvent GetPosts) = do
  AppState{currentSubreddit = Subreddit{..}, ..} <- get
  void . liftIO . forkIO $ do
    _ <- atomically $ takeTMVar after
    Listing{children, after = after''} :: Listing Post <-
      getEndpoint
        accessToken
        url
        [("limit", Just "100")]
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
appEvent _ = return ()

defaultBindings :: [(KeyEvent, [Binding])]
defaultBindings =
  [ (QuitEvent, [bind 'q', bind KEsc])
  , (ShowHelpEvent, [bind '?'])
  , (ShowSubredditEvent, [bind 's'])
  , (RefreshEvent, [bind 'r'])
  , (OpenPostUrlEvent, [bind 'o'])
  , (ShowPostPreviewEvent, [bind 'p'])
  ]

allKeyEvents :: KeyEvents KeyEvent
allKeyEvents =
  keyEvents
    [ ("quit", QuitEvent)
    , ("help", ShowHelpEvent)
    , ("subreddit", ShowSubredditEvent)
    , ("refresh", RefreshEvent)
    , ("open", OpenPostUrlEvent)
    , ("preview", ShowPostPreviewEvent)
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
      ( do
          AppState{..} <- get
          liftIO $ do
            writeBChan bchan GetUserData
            writeBChan bchan GetPosts
            writeBChan bchan GetSubreddits
      )
  , onEvent
      OpenPostUrlEvent
      "Open post in browser"
      ( do
          AppState{..} <- get
          when (not showSubreddit) $ do
            case listSelectedElement posts of
              Just (_, Post{..}) -> do
                liftIO $ openInBrowser url
              Nothing -> return ()
      )
  , onEvent
      ShowPostPreviewEvent
      "Show post preview"
      (modify $ \st@AppState{..} -> st{showPostPreview = not showPostPreview})
  ]
