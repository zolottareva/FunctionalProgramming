module Feedback
  ( findFeedbacksByArticleId
  , addFeedback
  , deleteFeedbackById
  , updateFeedback
  , FeedbackInput(..)
  , Feedback(..)
  ) where

import qualified Data.ByteString.Char8    as BS
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Lib
import           User

data FeedbackInput =
  FeedbackInput
    { inputText       :: String
    , inputArticleId  :: Integer
    , inputAuthorName :: String
    }
  deriving (Show)

data Feedback =
  Feedback
    { uid      :: Integer
    , text     :: String
    , authorId :: Integer
    }

instance Show Feedback where
  show (Feedback uid name authorId) =
    "Feedback #" ++ show uid ++ " text=" ++ show name ++ ", authorId=" ++ show authorId

instance FromSqlMapping Feedback where
  fromSqlValues [SqlInteger uid, SqlInteger articleId, SqlInteger authorId, SqlByteString text] =
    Feedback {Feedback.uid = uid, text = BS.unpack text, authorId = authorId}
  fromSqlValues x = error $ show x

instance ToSqlMapping FeedbackInput where
  toSqlValues feedbackInput = [SqlString $ inputText feedbackInput, SqlInteger $ inputArticleId feedbackInput]

findFeedbacksByArticleId :: IConnection a => Integer -> a -> IO [Feedback]
findFeedbacksByArticleId articleId =
  performListQuery "SELECT * FROM article_feedback WHERE article_id = ? ORDER BY id ASC" [SqlInteger articleId]

addFeedback :: IConnection a => FeedbackInput -> a -> IO ()
addFeedback feedbackInput conn = do
  authorId <- findUserIdByName (inputAuthorName feedbackInput) conn
  runQuery
    "INSERT INTO article_feedback (text, article_id, author_id) VALUES (?, ?, ?)"
    (toSqlValues feedbackInput ++ [SqlInteger $ fromJust authorId])
    conn

deleteFeedbackById :: IConnection a => Integer -> a -> IO ()
deleteFeedbackById id = runQuery "DELETE FROM article_feedback WHERE id = ?" [SqlInteger id]

updateFeedback :: IConnection a => Integer -> FeedbackInput -> Integer -> a -> IO ()
updateFeedback id feedbackInput articleId conn = do
  authorId <- findUserIdByName (inputAuthorName feedbackInput) conn
  runQuery
    "UPDATE article_feedback SET text = ?, article_id = ?, author_id = ? WHERE id = ?"
    (toSqlValues feedbackInput ++ [SqlInteger $ fromJust authorId, SqlInteger id])
    conn
