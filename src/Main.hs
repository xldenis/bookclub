{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module Main where

import Data.Aeson

import qualified Data.ByteString.Char8 as B
import           Data.Text as T
import           Data.Time.Clock

import Text.Megaparsec as M
import Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

import Control.Monad (void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader

import           Network.Wai.Handler.Warp

import System.Environment (getArgs, getEnv)

import Database.PostgreSQL.Simple

import Slack

type User = Text

data BookCommand
  = List
  | Add Text
  | Vote Text
  deriving (Show, Eq)

sc :: Parser ()
sc = void $ skipMany spaceChar

sym = symbol sc

lex = lexeme sc

parseCommand :: Parser BookCommand
parseCommand = do
  list <|> add <|> vote
  where list = sym "list" *> return List
        add  = sym "add"  *> ((Add  . strip . pack) <$> manyTill anyChar eof)
        vote = sym "vote" *> ((Vote . strip . pack) <$> manyTill anyChar eof)

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
    return "Added the book to the list!"
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
        return "voted for the book"
      _ -> return "ruhroh multiple books were found"

runInterp :: Connection -> Command -> IO Text
runInterp conn c = do
  case M.parseMaybe parseCommand (text c) of
    Nothing -> return "I'm sorry I didn't understand that"
    Just bc -> do
      runReaderT (interpCommand c bc) conn

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
              [] -> 8080
              x:_ -> read x :: Int
  print $ "Booted with port " ++ show port

  connInfo <- getEnv "DATABASE_URL"
  conn <- connectPostgreSQL (B.pack connInfo)

  run port (slashSimple $ runInterp conn)
