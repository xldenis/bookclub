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
import           Command


newtype Bookclub a = Bookclub
  { runBookclub :: ReaderT Connection (ExceptT Text IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadError Text, MonadReader Connection)

interpCommand :: Command -> BookCommand -> Bookclub Text
interpCommand c List = do
  results <- orderBooksByVotes
  formatResults <$> mapM (\(bId :: Int, title, votes :: Int) -> do
    return $ F.sformat bookLine bId title votes
                        ) results
  where formatResults = (T.append "Books in the list: \n") . T.unlines
        int2Text = T.pack . show @Int
        bookLine = F.int % ". _" % F.stext % "_ (" % F.int % ":+1:)"
interpCommand c (Add b) = do
  time <- liftIO $ getCurrentTime
  createBook (b, time)
  return "Added book to the list!"
interpCommand c (Vote b) = do
  uId <- findOrCreateUser (user_name c)
  (bId, title) <- bookFromIdOrTitle b
  time <- liftIO $ getCurrentTime
  castVote (bId) uId time
  return $ F.sformat ("you've voted for \"_" % F.stext % "_\"") title
interpCommand c (Unvote b) = do
  uId <- findOrCreateUser (user_name c)
  bId <- fst <$> bookFromIdOrTitle b
  uncastVote (uId, bId)
  return "Your vote has been removed"
interpCommand c Menu = return "Commands are add <title>, vote|unvote|delete|finish <id|title>"
interpCommand c (Info b) = do
  book <- liftIO (searchByTitle b) >>= \case
    Just x  -> return x
    Nothing -> throwError "Hmm... I wasn't able to find that book online :("
  return $ F.sformat ("_" % F.stext % "_ by " % F.stext % ". " % F.stext) (title book) (author book) (description book)
interpCommand c (Remove t) = do
  (bId, title) <- bookFromIdOrTitle t
  deleteBook bId
  return $ F.sformat ("_" % F.stext % "_ was deleted") (title)
interpCommand c (Finish t) = do
  (bId, title) <- bookFromIdOrTitle t
  markAsRead bId
  return $ F.sformat ("_" % F.stext % "_ was finshed!") (title)

runInterp :: Connection -> Command -> IO R.Response
runInterp conn c = do
  case parseCommand (text c) of
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
