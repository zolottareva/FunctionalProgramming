module Article
  ( showArticles
  , addArticleWithFiles
  , findAllArticles
  , deleteArticleById
  , updateArticle
  , Article(..)
  , ArticleInput(..)
  ) where

import           ArticleView
import qualified Data.ByteString.Char8    as BS
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Feedback
import           File
import           Lib
import           User

data ArticleInput =
  ArticleInput
    { inputName       :: String
    , inputAnnotation :: String
    , inputAuthorName :: String
    }
  deriving (Show)

data Article =
  Article
    { uid        :: Integer
    , name       :: String
    , annotation :: String
    , authorId   :: Integer
    }

instance Show Article where
  show (Article uid name annotation authorId) =
    "Article #" ++
    show uid ++ " name=" ++ show name ++ ", annotation=" ++ show annotation ++ ", authorId=" ++ show authorId

instance FromSqlMapping Article where
  fromSqlValues [SqlInteger uid, SqlByteString name, SqlByteString annotation, SqlInteger authorId] =
    Article {Article.uid = uid, Article.name = BS.unpack name, annotation = BS.unpack annotation, Article.authorId = authorId}
  fromSqlValues x = error $ show x

instance ToSqlMapping ArticleInput where
  toSqlValues articleInput = [SqlString $ Article.inputName articleInput, SqlString $ inputAnnotation articleInput]

findAllArticles :: IConnection a => a -> IO [Article]
findAllArticles = performListQuery "SELECT * FROM article ORDER BY id ASC" []

findArticleWithOneToMany :: IConnection a => a -> Article -> IO String
findArticleWithOneToMany conn article = do
  files <- findFilesByArticleId (Article.uid article) conn
  feedback <- findFeedbacksByArticleId (Article.uid article) conn
  viewsCount <- findViewsCountByArticleId (Article.uid article) conn
  return $ show article ++ ", files=" ++ show files ++ ", feedback=" ++ show feedback ++ ", views=" ++ show viewsCount

showArticles :: IConnection a => a -> IO ()
showArticles conn = do
  putStrLn "--- Articles:"
  articles <- findAllArticles conn
  fullArticles <- mapM (findArticleWithOneToMany conn) articles
  putStr $ unlines fullArticles

addArticleWithFiles :: IConnection a => ArticleInput -> [FileInput] -> a -> IO ()
addArticleWithFiles articleInput fileInputs conn = do
  newId <- performIntegerQuery "SELECT nextval(pg_get_serial_sequence('article', 'id'))" [] conn
  authorId <- findUserIdByName (Article.inputAuthorName articleInput) conn
  runQuery
    "INSERT INTO article (id, author_id, name, annotation) VALUES (?, ?, ?, ?)"
    (SqlInteger newId : (SqlInteger $ fromJust authorId) : toSqlValues articleInput)
    conn
  mapM_ (\f -> addFile f newId conn) fileInputs

deleteArticleById :: IConnection a => Integer -> a -> IO ()
deleteArticleById id = runQuery "DELETE FROM article WHERE id = ?" [SqlInteger id]

updateArticle :: IConnection a => Integer -> ArticleInput -> a -> IO ()
updateArticle id articleInput conn = do
  authorId <- findUserIdByName (Article.inputAuthorName articleInput) conn
  runQuery
    "UPDATE article SET author_id = ?, name = ?, annotation = ? WHERE id = ?"
    ((SqlInteger $ fromJust authorId) : toSqlValues articleInput ++ [SqlInteger id])
    conn
