{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
module Query where

import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Catch

import           Database.PostgreSQL.Simple hiding (query_, query, execute, execute_)
import qualified Database.PostgreSQL.Simple as P (query, query_, execute, execute_)

import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Data.Text
import           Data.Time.Clock


type BookId = Int
type UserId = Int

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

findOrCreateUser :: (MonadReader Connection m, MonadIO m, MonadError Text m) => Text -> m UserId
findOrCreateUser uname = do
  (userIds :: [Only Int]) <- query "select id from users where name = ?" [uname]
  uIds <- maybeList (do
      time <- liftIO $ getCurrentTime
      query "insert into users (name, created_at) values (?, ?) returning id" (uname, time)
    ) (return) (userIds)
  return . fromOnly $ Prelude.head uIds

castVote :: (MonadReader Connection m, MonadIO m, MonadCatch m, MonadError Text m) => BookId -> UserId -> UTCTime -> m Text
castVote bId uId time = do
  execute "insert into votes values (?, ?, ?)" (bId, uId, time) `catch` \(e :: SqlError) ->
    throwError "you can't vote twice you silly goose"
  return "voted for the book"

numVotes :: (MonadReader Connection m, MonadIO m, MonadError Text m) => BookId -> m Int
numVotes bId = do
  count <- query "select count(*) from votes where book_id = ?" [bId]
  case listToMaybe count of
    Nothing -> return 0
    Just c  -> return $ fromOnly c

maybeList def _ [] = def
maybeList _ f xs   = f xs
