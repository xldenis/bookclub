{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module GBooks where

import Control.Exception (throwIO)
import Network.HTTP.Req

import Data.Aeson
import Data.Text hiding (filter)
import Data.Vector as V (filter)
import Control.Lens
import Data.Aeson.Lens

instance MonadHttp IO where
  handleHttpException = throwIO

data Book = Book
  { title :: Text
  , author :: Text
  , description :: Text
  -- , isbn :: Text
  } deriving (Show, Eq)

searchByTitle :: MonadHttp m => Text -> m (Maybe Book)
searchByTitle title = do
  let url = https "www.googleapis.com" /: "books" /: "v1" /: "volumes"

  v <- req GET url NoReqBody jsonResponse $
    "q" =: title
  return $ (responseBody v :: Value) ^? (key "items") . nth 0 . key "volumeInfo" >>= toBook

doSearch title = do
  let url = https "www.googleapis.com" /: "books" /: "v1" /: "volumes"

  req GET url NoReqBody jsonResponse $
    "q" =: title

toBook :: Value -> Maybe Book
toBook value = Book
  <$> value ^? key "title" . _String
  <*> value ^? key "authors" . nth 0 . _String
  <*> value ^? key "description" . _String
  -- <*> value ^? key "industryIdentifiers" . _Array . _Isbn13 . ix 0 . key "identifier" . _String
  -- where _Isbn13 = to (V.filter $ \o -> o ^?! key "type" == "ISBN_13")
