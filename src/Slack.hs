{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module Slack where

import GHC.Generics

import           Data.Aeson

import           Data.Text as T
import           Data.Text.Lazy as TL (fromStrict)
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Network.Wai as W
import           Network.HTTP.Types (status200, status404, hContentType)

import Web.FormUrlEncoded

import qualified Slack.Response as R

data Command = Command
  { command :: Text
  , user_name :: Text
  , channel_name :: Text
  , text :: Text
  } deriving (Show, Eq, Generic)

instance FromForm Command

slashSimple :: (Command -> IO R.Response) -> W.Application
slashSimple f = do
  slash $ \p req resp -> case p of
    Left _ ->  resp errorResp
    Right c -> (f c) >>= resp . successResp
  where errorResp   = W.responseLBS status404 headers "Something went wrong in your request."
        successResp = W.responseLBS status200 headers . encode
        headers     = [("Content-Type", "application/json")]

slash :: (Either Text Command -> W.Application) -> W.Application
slash f req resp = do
  body <- W.strictRequestBody req
  let params = urlDecodeForm body >>= fromForm

  f params req resp
