module User
  ( showUsers
  , addUser
  , findUserIdByName
  , findUserById
  , deleteUserById
  , updateUser
  , User(..)
  , UserInput(..)
  ) where

import qualified Data.ByteString.Char8    as BS
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Lib

data UserInput =
  UserInput
    { inputName :: String
    }
  deriving (Show)

data User =
  User
    { uid  :: Integer
    , name :: String
    }

instance FromSqlMapping User where
  fromSqlValues [SqlInteger uid, SqlByteString name] = User {uid = uid, name = BS.unpack name}
  fromSqlValues x = error $ show x

instance ToSqlMapping UserInput where
  toSqlValues fileInput = [SqlString $ inputName fileInput]

instance Show User where
  show (User uid name) = "User #" ++ show uid ++ " name=" ++ show name

findAllUsers :: IConnection a => a -> IO [User]
findAllUsers = performListQuery "SELECT * FROM \"user\" ORDER BY id ASC" []

findUserById :: IConnection a => Integer -> a -> IO (Maybe User)
findUserById userId conn = do
  listResult <- performListQuery "SELECT * FROM \"user\" WHERE id = ?" [SqlInteger userId] conn
  return $ listToMaybe listResult

findUserIdByName :: IConnection a => String -> a -> IO (Maybe Integer)
findUserIdByName userName conn = do
  listResult <- performIntegerQuery "SELECT id FROM \"user\" WHERE name = ?" [SqlString userName] conn
  return $ Just listResult

showUsers :: IConnection a => a -> IO ()
showUsers conn = do
  putStrLn "--- Users:"
  users <- findAllUsers conn
  let userStrings = map show users
  putStr $ unlines userStrings

addUser :: IConnection a => UserInput -> a -> IO ()
addUser userInput = runQuery "INSERT INTO \"user\" (name) VALUES (?)" (toSqlValues userInput)

deleteUserById :: IConnection a => Integer -> a -> IO ()
deleteUserById id = runQuery "DELETE FROM \"user\" WHERE id = ?" [SqlInteger id]

updateUser :: IConnection a => Integer -> UserInput -> a -> IO ()
updateUser id userInput =
  runQuery "UPDATE \"user\" SET name = ? WHERE id = ?" (toSqlValues userInput ++ [SqlInteger id])
