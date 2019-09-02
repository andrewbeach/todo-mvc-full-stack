module TodoMvc.Data.User
  ( User
  , UserId
  , userId
  ) where

import Data.Newtype (class Newtype, wrap)
import Simple.JSON (class ReadForeign)

newtype UserId = UserId Int
derive instance newTypeUserId :: Newtype UserId _
derive newtype instance readForeignUserId :: ReadForeign UserId

userId :: Int -> UserId
userId = wrap

type User =
  { id :: UserId
  , fname :: String
  , lname :: String
  }
