{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Slack.Response where

import GHC.Generics

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text

options = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , constructorTagModifier = camelTo2 '_'
  }

data ResponseType = Ephemeral | InChannel deriving (Show, Eq, Generic)

data Response = Response
  { responseType :: ResponseType
  , text         :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ResponseType where
  toEncoding = genericToEncoding options

instance ToJSON Response where
  toEncoding = genericToEncoding options

ephemeral :: Text -> Response
ephemeral = Response Ephemeral

inChannel :: Text -> Response
inChannel = Response InChannel
