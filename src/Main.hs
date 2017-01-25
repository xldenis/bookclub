{-# LANGUAGE TypeApplications,
             OverloadedStrings,
             DeriveFunctor,
             GeneralizedNewtypeDeriving,
             DeriveGeneric,
             LambdaCase,
             ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as B
import           Data.Text as T
import           Data.Text.Lazy (toStrict)
import           Data.Time.Clock

import           Text.Megaparsec as M
import           Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

import           Text.Read (readMaybe)

import           Control.Monad (void)
import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Catch

import           Network.Wai.Handler.Warp

import           System.Environment (getArgs, getEnv)

import           Database.PostgreSQL.Simple hiding (query_, query, execute, execute_)

import qualified Formatting as F
import           Formatting ((%))

import           Slack
import qualified Slack.Response as R

import           Query
import           GBooks
import qualified GBooks as B

type User = Text

data BookCommand
  = List
  | Add Text
  | Vote Text
  | Unvote Text
  | Menu
  | Info Text
  | Remove Text
  | Finish Text
  deriving (Show, Eq)

newtype Bookclub a = Bookclub
  { runBookclub :: ReaderT Connection (ExceptT Text IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadError Text, MonadReader Connection)

sc :: Parser ()
sc = void $ skipMany spaceChar

sym = symbol sc

lex = lexeme sc

parseCommand :: Parser BookCommand
parseCommand = do
  list <|> add <|> vote <|> unvote <|> info <|> remove <|> menu
  where list   = sym "list" *> return List
        add    = cons "add" Add
        vote   = cons "vote" Vote
        unvote = cons "unvote" Unvote
        menu   = return Menu
        info   = cons "info" Info
        remove = cons "remove" Remove
        finish = cons "finish" Finish
        spack  = strip . pack
        cons s c = sym s *> ((c . spack) <$> manyTill anyChar eof)

interpCommand :: Command -> BookCommand -> Bookclub Text
interpCommand c List = do
  results <- query_ "select b.id, b.title, count(v.book_id)  from books b full join votes v on b.id = v.book_id where b.read = false group by b.id, b.title order by count(v.book_id) desc"
  formatResults <$> mapM (\(bId :: Int, title, votes :: Int) -> do
    return $ F.sformat bookLine bId title votes
                        ) results
  where formatResults = (T.append "Books in the list: \n") . T.unlines
        int2Text = T.pack . show @Int
        bookLine = F.int % ". _" % F.text % "_ (" % F.int % ":+1:)"
interpCommand c (Add b) = do
  time <- liftIO $ getCurrentTime
  execute "insert into books (title, created_at) values (?, ?)" (b, time)
  return "Added book to the list!"
interpCommand c (Vote b) = do
  uId <- findOrCreateUser (user_name c)
  (bId, title) <- bookFromIdOrTitle b
  time <- liftIO $ getCurrentTime
  castVote (bId) uId time
  return $ F.sformat ("you've voted for \"_" % F.stext % "_\"") title
interpCommand c (Unvote b) = do
  uId <- findOrCreateUser (user_name c)
  (bId, title) <- bookFromIdOrTitle b
  hasVoted <- query "select count(*) from votes where user_id = ? and book_id = ?" (uId, bId)
  case Prelude.head hasVoted of
    Only (0 :: Int) -> throwError "You never voted in the first place"
    _               -> do
      execute "delete from votes where user_id = ? and book_id = ?" (uId, bId)
      return "done"
interpCommand c Menu = return "Commands are add <title>, vote|unvote|delete|finish <id|title>"
interpCommand c (Info b) = do
  book <- liftIO (searchByTitle b) >>= \case
    Just x  -> return x
    Nothing -> throwError "Hmm... I wasn't able to find that book online :("
  return $ F.sformat ("_" % F.stext % "_ by " % F.stext % ". " % F.stext) (title book) (author book) (description book)
interpCommand c (Remove t) = do
  (bId, title) <- bookFromIdOrTitle t
  execute "delete from books where id = ?" [bId]
  execute "delete from votes where book_id = ?" [bId]
  return $ F.sformat ("_" % F.stext % "_ was deleted") (title)
interpCommand c (Finish t) = do
  (bId, title) <- bookFromIdOrTitle t
  execute "update books set read = true where id = ?" [bId]
  return $ F.sformat ("_" % F.stext % "_ was finshed!") (title)
runInterp :: Connection -> Command -> IO R.Response
runInterp conn c = do
  case M.parseMaybe parseCommand (text c) of
    Nothing -> return $ R.ephemeral "I'm sorry I didn't understand that"
    Just bc -> do
      response <- runExceptT $ runReaderT  (runBookclub $ interpCommand c bc) conn
      return $ either (R.ephemeral) (R.inChannel) response

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
