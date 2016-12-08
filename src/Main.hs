{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module Main where

import GHC.Generics

import Data.Aeson

import qualified Data.ByteString.Char8 as B
import           Data.Text.Lazy as TL (fromStrict)
import           Data.Text as T
import           Data.Time.Clock

import qualified Data.Text.Lazy.Encoding as TLE

import Text.Megaparsec as M
import Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

import Web.FormUrlEncoded

import Control.Monad (void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader

import qualified Network.Wai as W
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types (status200, status404, hContentType)

import System.Environment (getArgs, getEnv)

import Database.PostgreSQL.Simple

data Command = Command
  { command :: Text
  , user_name :: Text
  , channel_name :: Text
  , text :: Text
  } deriving (Show, Eq, Generic)

instance FromForm Command

type User = Text

data BookCommand
  = List
  | Add Text
  | Vote Text
  deriving (Show, Eq)

sc :: Parser ()
sc = void spaceChar

sym = symbol sc

lex = lexeme sc

parseCommand :: Parser BookCommand
parseCommand = do
  list <|> add <|> vote
  where list = sym "list" *> return List
        add  = sym "add"  *> ((Add . pack)  <$> manyTill anyChar eol)
        vote = sym "vote" *> ((Vote . pack) <$> manyTill anyChar eol)

interpCommand :: Command -> BookCommand -> ReaderT Connection IO Text
interpCommand c List = do
  conn <- ask
  lift $ do
    results <- query_ conn "select title from books where read = false"
    return $ formatResults results
  where formatResults = T.unlines . fmap (fromOnly)
interpCommand c (Add  b) = do
  conn <- ask
  lift $ do
    time <- getCurrentTime
    execute conn "insert into books values (?, ?)" (b, time)
    return "yolo"
interpCommand c (Vote b) = do
  conn <- ask
  lift $ do
    (userIds :: [Only Integer]) <- query conn "select id from users where name = ?" [user_name c]
    uIds <- case userIds of
      [] -> do
        time <- getCurrentTime
        query conn "insert into users values (?, ?) returning id" (user_name c, time)
      x -> return x

    (bookIds :: [Only Integer]) <- query conn "select id from books where title = ?" [b]

    case bookIds of
      []    -> return "Could not find that book :("
      [bId] -> do
        time <- getCurrentTime
        let uId = fromOnly $ Prelude.head userIds
        execute conn "insert into votes values (?, ?, ?)" (uId, fromOnly bId, time)
        return "yay"
      _ -> return "wtf y'd u do that to me :/"

runInterp :: Command -> IO Text
runInterp c = do
  case M.parseMaybe parseCommand (text c) of
    Nothing -> return "I'm sorry I didn't understand that"
    Just bc -> do
      connInfo <- getEnv "DATABASE_URL"
      conn <- connectPostgreSQL (B.pack connInfo)
      runReaderT (interpCommand c bc) conn

slashSimple :: (Command -> IO Text) -> W.Application
slashSimple f = do
  slash $ \p req resp -> case p of
    Left _ ->  resp errorResp
    Right c -> (f c) >>= resp . successResp
  where errorResp   = W.responseLBS status404 headers "yolooooooo"
        successResp = W.responseLBS status200 headers . TLE.encodeUtf8 . fromStrict
        headers     = [("Content-Type", "text/plain")]

slash :: (Either Text Command -> W.Application) -> W.Application
slash f req resp = do
  body <- W.strictRequestBody req
  let params = urlDecodeForm body >>= fromForm

  f params req resp

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
              [] -> 8080
              x:_ -> read x :: Int
  print $ "Booted with port " ++ show port
  run port (slashSimple runInterp)
