module TodoMvc.Data.User
  ( User
  , UserId
  , userId
  ) where

import Prelude

newtype UserId = UserId Int
instance showUserId :: Show UserId where
  show (UserId i) = "<User" <> show i <> ">"

userId :: Int -> UserId
userId = UserId

type User =
  { id :: UserId
  , fname :: String
  , lname :: String
  }
