{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings, TypeApplications, ConstraintKinds #-}
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

import Text.Read (readMaybe)

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

type MonadDB m = (MonadReader Connection m, MonadIO m, MonadError Text m)

bookFromIdOrTitle :: MonadDB m => Text -> m (BookId, Text)
bookFromIdOrTitle s = do
  bookIds <- case readMaybe @Int (unpack s) of
    Just bId -> query "select id, title from books where id = ?" [bId]
    Nothing  -> query "select id, title from books where title = ?" [s]

  case bookIds of
    []    -> throwError "Could not find that book :("
    [x]   -> return x
    _     -> throwError "Multiple books ???"

findOrCreateUser :: MonadDB m => Text -> m UserId
findOrCreateUser uname = do
  (userIds :: [Only Int]) <- query "select id from users where name = ?" [uname]
  uIds <- maybeList (do
      time <- liftIO $ getCurrentTime
      query "insert into users (name, created_at) values (?, ?) returning id" (uname, time)
    ) (return) (userIds)
  return . fromOnly $ Prelude.head uIds

castVote :: (MonadDB m, MonadCatch m) => BookId -> UserId -> UTCTime -> m Text
castVote bId uId time = do
  execute "insert into votes values (?, ?, ?)" (bId, uId, time) `catch` \(e :: SqlError) ->
    throwError "you can't vote twice you silly goose"
  return "voted for the book"

uncastVote :: MonadDB m => (UserId, BookId) -> m ()
uncastVote ids = void $ do
  hasVoted <- query "select count(*) from votes where user_id = ? and book_id = ?" ids
  case Prelude.head hasVoted of
    Only (0 :: Int) -> throwError "You never voted in the first place"
    _               -> do
      execute "delete from votes where user_id = ? and book_id = ?" ids
      return "done"

numVotes :: MonadDB m => BookId -> m Int
numVotes bId = do
  count <- query "select count(*) from votes where book_id = ?" [bId]
  case listToMaybe count of
    Nothing -> return 0
    Just c  -> return $ fromOnly c

markAsRead :: MonadDB m => BookId -> m ()
markAsRead bId = void $ execute "update books set read = true where id = ?" [bId]

deleteBook :: MonadDB m => BookId -> m ()
deleteBook bId = void $ do
  execute "delete from votes where book_id = ?" [bId]
  execute "delete from books where id = ?" [bId]

createBook :: MonadDB m => (Text, UTCTime) -> m ()
createBook b = void $ do
  execute "insert into books (title, created_at) values (?, ?)" b

orderBooksByVotes :: MonadDB m => m [(BookId, Text, Int)]
orderBooksByVotes = do
  query_ "select b.id, b.title, count(v.book_id) \
         \from books b full join votes v on b.id = v.book_id \
         \where b.read = false group by b.id, b.title order by count(v.book_id) desc"

maybeList def _ [] = def
maybeList _ f xs   = f xs
