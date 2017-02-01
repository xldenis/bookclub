module Command where

import           Control.Monad

import           Data.Text
import           Text.Megaparsec as M
import           Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

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

sc :: Parser ()
sc = void $ skipMany spaceChar

sym = symbol sc

lex = lexeme sc

parseCommand :: Text -> Maybe BookCommand
parseCommand = parseMaybe parser

parser :: Parser BookCommand
parser = do
  list <|> add <|> vote <|> unvote <|> info <|> remove <|> finish <|> menu
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
