{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveGeneric, ScopedTypeVariables #-}
module Main where

import Data.Aeson
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as B
import           Data.Text as T
import           Data.Time.Clock
import           Data.Maybe (listToMaybe)

import Text.Megaparsec as M
import Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

import Control.Monad (void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Catch

import           Network.Wai.Handler.Warp

import System.Environment (getArgs, getEnv)

import Database.PostgreSQL.Simple hiding (query_, query, execute, execute_)
import qualified Database.PostgreSQL.Simple as P (query, query_, execute, execute_)

import Slack

type User = Text

data BookCommand
  = List
  | Add Text
  | Vote Text
  deriving (Show, Eq)

newtype Bookclub a = Bookclub
  { runBookclub :: ReaderT Connection (ExceptT Text IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadError Text, MonadReader Connection)

sc :: Parser ()
sc = void $ skipMany spaceChar

sym = symbol sc

lex = lexeme sc

query_ :: (MonadReader Connection m, MonadIO m, FromRow r1) => Query -> m [r1]
query_ b = do
  conn <- ask
  liftIO $ P.query_ conn b

query :: (MonadReader Connection m, MonadIO m, FromRow r1, ToRow q) => Query -> q -> m [r1]
query b c = ask >>= \conn -> liftIO $ P.query conn b c

execute :: (MonadReader Connection m, MonadIO m, ToRow q) => Query -> q -> m Int64
execute b c =  ask >>= \conn -> liftIO $ P.execute conn b c

execute_ :: (MonadReader Connection m, MonadIO m) => Query -> m Int64
execute_ b =  ask >>= \conn -> liftIO $ P.execute_ conn b

parseCommand :: Parser BookCommand
parseCommand = do
  list <|> add <|> vote
  where list = sym "list" *> return List
        add  = sym "add"  *> ((Add  . strip . pack) <$> manyTill anyChar eof)
        vote = sym "vote" *> ((Vote . strip . pack) <$> manyTill anyChar eof)


interpCommand :: Command -> BookCommand -> Bookclub Text
interpCommand c List = do
  results <- query_ "select title from books where read = false"
  return $ formatResults results
  where formatResults = (T.append "Books in the list: ") . T.unlines . fmap (fromOnly)
interpCommand c (Add b) = do
  time <- liftIO $ getCurrentTime
  execute "insert into books (title, created_at) values (?, ?)" (b, time)
  return "Added the book to the list!"
interpCommand c (Vote b) = do
  (userIds :: [Only Integer]) <- query "select id from users where name = ?" [user_name c]

  uIds <- maybeList (do
      time <- liftIO $ getCurrentTime
      query "insert into users (name, created_at) values (?, ?) returning id" (user_name c, time)
    ) (return) (userIds)

  (bookIds :: [Only Integer]) <- query "select id from books where title = ?" [b]

  case bookIds of
    []    -> throwError "Could not find that book :("
    [bId] -> do
      time <- liftIO $ getCurrentTime
      let uId = fromOnly $ Prelude.head uIds
      castVote (fromOnly bId) uId time
    _ -> throwError "ruhroh multiple books were found"

maybeList def _ [] = def
maybeList _ f xs   = f xs

type BookId = Integer
type UserId = Integer

castVote :: BookId -> UserId -> UTCTime -> Bookclub Text
castVote bId uId time = do
  execute "insert into votes values (?, ?, ?)" (bId, uId, time) `catch` \(e :: SqlError) ->
    throwError "you can't vote twice you silly goose"
  return "voted for the book"

runInterp :: Connection -> Command -> IO Text
runInterp conn c = do
  case M.parseMaybe parseCommand (text c) of
    Nothing -> return "I'm sorry I didn't understand that"
    Just bc -> do
      response <- runExceptT $ runReaderT  (runBookclub $ interpCommand c bc) conn
      return $ either (id) (id) response


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
