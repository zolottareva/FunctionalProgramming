module File
  ( findFilesByArticleId
  , deleteFileById
  , addFile
  , File(..)
  , FileInput(..)
  ) where

import qualified Data.ByteString.Char8    as BS
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Lib

data FileInput =
  FileInput
    { inputFileName :: String
    , inputPath     :: String
    }
  deriving (Show)

data File =
  File
    { uid      :: Integer
    , fileName :: String
    , path     :: String
    }

instance Show File where
  show (File uid name path) = "File #" ++ show uid ++ " name=" ++ show name ++ ", path=" ++ show path

instance FromSqlMapping File where
  fromSqlValues [SqlInteger uid, SqlByteString name, SqlByteString path, SqlInteger articleId] =
    File {uid = uid, fileName = BS.unpack name, path = BS.unpack path}
  fromSqlValues x = error $ show x

instance ToSqlMapping FileInput where
  toSqlValues fileInput = [SqlString $ inputFileName fileInput, SqlString $ inputPath fileInput]

findFilesByArticleId :: IConnection a => Integer -> a -> IO [File]
findFilesByArticleId articleId = performListQuery "SELECT * FROM file WHERE article_id = ?" [SqlInteger articleId]

addFile :: IConnection a => FileInput -> Integer -> a -> IO ()
addFile fileInput articleId =
  runQuery
    "INSERT INTO file (name, path, article_id) VALUES (?, ?, ?)"
    (toSqlValues fileInput ++ [SqlInteger articleId])

deleteFileById :: IConnection a => Integer -> a -> IO ()
deleteFileById id = runQuery "DELETE FROM file WHERE id = ?" [SqlInteger id]

updateFile :: IConnection a => Integer -> FileInput -> Integer -> a -> IO ()
updateFile id fileInput articleId =
  runQuery
    "UPDATE file SET name = ?, path = ?, article_id = ? WHERE id = ?"
    (toSqlValues fileInput ++ [SqlInteger articleId, SqlInteger id])