module Lib
  ( FromSqlMapping
  , ToSqlMapping
  , toSqlValues
  , fromSqlValues
  , performListQuery
  , performIntegerQuery
  , runQuery
  ) where

import           Database.HDBC

class FromSqlMapping a where
  fromSqlValues :: [SqlValue] -> a

class ToSqlMapping a where
  toSqlValues :: a -> [SqlValue]

instance FromSqlMapping Integer where
  fromSqlValues [SqlInteger result] = result
  fromSqlValues x                   = error $ show x

performIntegerQuery :: (IConnection a) => String -> [SqlValue] -> a -> IO Integer
performIntegerQuery query values conn = do
  result <- performListQuery query values conn
  return $ head result

performListQuery :: (IConnection a, FromSqlMapping b) => String -> [SqlValue] -> a -> IO [b]
performListQuery query values conn = do
  result <- quickQuery' conn query values
  return $ map fromSqlValues result

runQuery :: (IConnection a) => String -> [SqlValue] -> a -> IO ()
runQuery query values conn = do
  result <- run conn query values
  commit conn
  return ()
