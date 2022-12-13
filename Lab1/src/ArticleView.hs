module ArticleView
  ( findViewsCountByArticleId
  , addView
  , ArticleViewInput(..)
  ) where

import qualified Data.ByteString.Char8    as BS
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Lib
import           User

data ArticleViewInput =
  ArticleViewInput
    { inputArticleId :: Integer
    , inputUserName  :: String
    }

findViewsCountByArticleId :: (IConnection a) => Integer -> a -> IO Integer
findViewsCountByArticleId articleId =
  performIntegerQuery "SELECT count(*) FROM article_view WHERE article_id = ?" [SqlInteger articleId]

addView :: IConnection a => ArticleViewInput -> a -> IO ()
addView viewInput conn = do
  userId <- findUserIdByName (inputUserName viewInput) conn
  runQuery
    "INSERT INTO article_view (article_id, user_id) VALUES (?, ?)"
    [SqlInteger $ inputArticleId viewInput, SqlInteger $ fromJust userId]
    conn
