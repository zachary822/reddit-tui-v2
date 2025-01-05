{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Reddit.Oauth where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import Data.ByteString.Char8 qualified as C8
import Data.Text.Encoding (encodeUtf8)
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lib.Reddit.Types
import Network.HTTP.Client.Conduit (Request (..))
import Network.HTTP.Conduit (RequestBody (RequestBodyBS))
import Network.HTTP.Simple
import Network.URI
import System.Posix.Files
import System.Random.Stateful
import Web.Scotty qualified as S

data TokenException = TokenException deriving (Show, Eq)

instance Exception TokenException

getToken :: FilePath -> Oauth -> IO Token
getToken p oauth@Oauth{..} =
  ( do
      mt <- decodeFileStrict p
      case mt of
        Nothing -> throwIO TokenException
        Just token@Token{expiresIn, refreshToken} -> do
          stat <- getFileStatus p
          let mtime = modificationTimeHiRes stat
          cur <- getPOSIXTime

          if mtime + secondsToNominalDiffTime (fromIntegral expiresIn) < cur
            then
              return token
            else do
              let req =
                    setRequestBasicAuth (C8.pack clientId) "" $
                      "https://www.reddit.com/api/v1/access_token"
              token' :: Token <-
                getResponseBody
                  <$> httpJSON
                    req
                      { method = "POST"
                      , requestHeaders = ("User-Agent", "haskell-tui 0.1.0.0") : requestHeaders req
                      , requestBody =
                          RequestBodyBS
                            ("grant_type=refresh_token&refresh_token=" <> encodeUtf8 refreshToken)
                      }
              encodeFile p token'
              return token'
  )
    `catches` [ Handler $ \(_ :: IOException) -> getTokenCallback p oauth
              , Handler $ \(_ :: TokenException) -> getTokenCallback p oauth
              ]

getTokenCallback :: FilePath -> Oauth -> IO Token
getTokenCallback p oauth@Oauth{..} = do
  state <-
    C8.unpack . convertToBase Base64URLUnpadded
      <$> uniformByteStringM 32 globalStdGen

  putStrLn $ redditOauth oauth state
  tvar <- newEmptyTMVarIO

  race_
    ( S.scotty 3000 $ do
        S.get "/oauth2/callback" $ do
          s <- S.queryParam "state"
          when (s /= state) (throw TokenException)

          code <- S.queryParam "code"

          let req =
                setRequestBasicAuth (C8.pack clientId) "" $
                  "https://www.reddit.com/api/v1/access_token"

          token :: Token <-
            getResponseBody
              <$> httpJSON
                req
                  { method = "POST"
                  , requestHeaders = ("User-Agent", "haskell-tui 0.1.0.0") : requestHeaders req
                  , requestBody =
                      RequestBodyBS $
                        C8.pack . concat $
                          [ "grant_type=authorization_code&code="
                          , code
                          , "&redirect_uri="
                          , redirectUri
                          ]
                  }
          liftIO $ do
            atomically (writeTMVar tvar token)
          S.text "Success! You can close this page."
    )
    ( do
        token <- atomically $ readTMVar tvar
        encodeFile p token
        return token
    )

  atomically $ takeTMVar tvar

redditOauth :: Oauth -> String -> String
redditOauth Oauth{..} state =
  concat
    [ "https://www.reddit.com/api/v1/authorize?client_id="
    , escapeURIString isAllowedInURI $ clientId
    , "&response_type=code&state="
    , escapeURIString isAllowedInURI state
    , "&redirect_uri="
    , escapeURIString isAllowedInURI $ redirectUri
    , "&duration="
    , show duration
    , "&scope="
    , escapeURIString isAllowedInURI . unwords $ scope
    ]
