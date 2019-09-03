module TodoMvc.Api.Capability where

import Prelude

import Data.Maybe (Maybe)
import Database.Postgres (Pool)
import Effect.Aff.Class (class MonadAff)
import TodoMvc.Data.Todo (Todo, TodoId, TodoWithMetadata)
import TodoMvc.Data.User (User, UserId)

class MonadAff m <= ManagePool m where
  startPool :: m Pool

class MonadAff m <= UsePool m where
  getPool :: m (Maybe Pool)

class Monad m <= ManageTodo m where
  createTodo :: UserId -> Todo -> m (Maybe TodoId)
  getTodo :: TodoId -> m (Maybe TodoWithMetadata)
  getTodos :: UserId -> m (Array TodoWithMetadata)

class Monad m <= ManageUser m where
  getUser :: UserId -> m (Maybe User)
  getUsers :: m (Array User)
