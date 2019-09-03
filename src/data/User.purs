module TodoMvc.Data.User
  ( User
  , UserId
  , userId
  ) where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Simple.JSON (class ReadForeign)

newtype UserId = UserId Int
derive instance newTypeUserId :: Newtype UserId _
derive newtype instance eqUserId :: Eq UserId
derive newtype instance ordUserId :: Ord UserId
derive newtype instance readForeignUserId :: ReadForeign UserId

userId :: Int -> UserId
userId = wrap

instance showUserId :: Show UserId where
  show (UserId i) = "<User" <> show i <> ">"

type User =
  { id :: UserId
  , fname :: String
  , lname :: String
  }
