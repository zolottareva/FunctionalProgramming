module Main where

import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL (connectPostgreSQL)

import           Article
import           ArticleView
import           Feedback
import           File
import           User

connectionString = "host=localhost dbname=haskelldb user=postgres password=131313"

main :: IO ()
main = do
  connection <- connectPostgreSQL connectionString
  -- Article
  ---- add
  addArticleWithFiles
    Article.ArticleInput
      {Article.inputName = "name", Article.inputAnnotation = "annotation", Article.inputAuthorName = "John Doe"}
    [ File.FileInput {File.inputFileName = "file1", File.inputPath = "http://example.com/1"}
    , File.FileInput {File.inputFileName = "file2", File.inputPath = "http://example.com/2"}
    ]
    connection
  ---- find
  showArticles connection
  ---- update
--  updateArticle
--    1
--    Article.ArticleInput {Article.inputName = "sdfdsf", Article.inputAuthorName = "Bob", Article.inputAnnotation = "sdff"}
--    connection
  ---- delete
--  deleteArticleById 29 connection
  -------------------------------
  -- File
  ---- add
--  addFile File.FileInput {File.inputFileName = "file1", File.inputPath = "http://example.com/1"} 1 connection
  ---- find
  files <- findFilesByArticleId 1 connection
  ---- delete
--  deleteFileById 123 connection
  -------------------------------
  --  User
  ---- add
--  addUser User.UserInput {User.inputName = "New User3"} connection
  ---- find
--  user <- findUserById 1 connection
--  print user
--  userId <- findUserIdByName "John Doe" connection
--  print userId
  ---- update
--  updateUser 1 User.UserInput {User.inputName = "John Doe"} connection
  ---- delete
--  deleteUserById 7 connection
  -------------------------------
  -- Feedback
  ---- add
  addFeedback
    Feedback.FeedbackInput
      {Feedback.inputText = "text", Feedback.inputAuthorName = "John Doe", Feedback.inputArticleId = 1}
    connection
  ---- find
  feedback <- findFeedbacksByArticleId 1 connection
  print feedback
  ---- delete
--  deleteFeedbackById 3 connection
  -------------------------------
  -- ArticleView
  ---- add
--  addView
--    ArticleView.ArticleViewInput {ArticleView.inputUserName = "New User", ArticleView.inputArticleId = 2}
--    connection
  ---- find
--  count <- findViewsCountByArticleId 1 connection
--  print count

  disconnect connection
