import           Test.Tasty               (defaultMain, testGroup)
import           Test.Tasty.HUnit         (assertBool, assertEqual,
                                           assertFailure, testCase)

import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL (connectPostgreSQL)

import           Article
import           Feedback
import           File
import           User

main :: IO ()
main = defaultMain tests

-- Those tests require working database with inserted data from sql/inserts.sql

tests = testGroup "All tests" [articleWithFilesTest, userTest, feedbackTest]

articleWithFilesTest =
  testCase "Test Article and Files addition and reading" $ do
    connection <- connectPostgreSQL "host=localhost dbname=haskelldb user=postgres password=postgres"
    addArticleWithFiles
      Article.ArticleInput
        {Article.inputName = "name", Article.inputAnnotation = "annotation", Article.inputAuthorName = "John Doe"}
      [File.FileInput {File.inputFileName = "file1", File.inputPath = "http://example.com/1"}]
      connection
    articles <- findAllArticles connection
    let newArticle = last articles
    files <- findFilesByArticleId (Article.uid newArticle) connection
    let newFile = last files
    assertEqual "Name is not saved correctly" "name" (Article.name newArticle)
    assertEqual "Annotation is not saved correctly" "annotation" (annotation newArticle)
    assertEqual "File name is not saved correctly" "file1" (fileName newFile)
    assertEqual "File path is not saved correctly" "http://example.com/1" (path newFile)

userTest =
  testCase "Test User addition, deletion and reading" $ do
    connection <- connectPostgreSQL "host=localhost dbname=haskelldb user=postgres password=postgres"
    addUser User.UserInput {User.inputName = "New Test User"} connection
    userIdMaybe <- findUserIdByName "New Test User" connection
    let userId = fromJust userIdMaybe
    user <- findUserById userId connection
    assertEqual "Name is not saved correctly" "New Test User" (User.name $ fromJust user)
    deleteUserById userId connection
    userNothing <- findUserById userId connection
    assertBool "User is unavailable after deletion" (isNothing userNothing)

feedbackTest =
  testCase "Test Feedback addition, deletion and reading" $ do
    connection <- connectPostgreSQL "host=localhost dbname=haskelldb user=postgres password=postgres"
    addFeedback
      Feedback.FeedbackInput
        {Feedback.inputText = "text", Feedback.inputAuthorName = "John Doe", Feedback.inputArticleId = 1}
      connection
    feedbacks <- findFeedbacksByArticleId 1 connection
    let lastFeedback = last feedbacks
    assertEqual "Text is not saved correctly" "text" (text lastFeedback)
    assertEqual "Text is not saved correctly" 1 (Feedback.authorId lastFeedback)
    deleteFeedbackById (Feedback.uid lastFeedback) connection
