module TodoMvc.Api.Capability where

import Prelude

import Data.Maybe (Maybe(..))
import Database.Postgres (Pool)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Ref (Ref)
import Node.Express.Handler (HandlerM)
import Node.HTTP (Server)
import TodoMvc.Data.Todo (TodoWithMetadata, TodoId)
import TodoMvc.Data.User (User, UserId)

class Monad m <= ManagePool m where
  startPool :: m Pool

class Monad m <= UsePool m where
  getPool :: m (Maybe Pool)

class Monad m <= ManageTodo m where
  -- createTodo :: Todo -> m Todo
  getTodo :: TodoId -> m (Maybe TodoWithMetadata)
  getTodos :: m (Array TodoWithMetadata)

class Monad m <= ManageUser m where
  getUser :: UserId -> m (Maybe User)

-- class Monad m <= ManageServer m where
--   runServer :: ExpressApp -> m Server
--   mkApplication :: m ExpressApp

-- class Monad m <= ManageRoutes m where
--   http :: ExpressApp -> Method -> Route -> HandlerFn -> m Unit
--   get :: ExpressApp -> Route -> HandlerFn -> m Unit

runQuery :: forall m f a. UsePool m => (Pool -> m (f a)) -> f a -> m (f a)
runQuery q def = do
  p <- getPool
  case p of
    (Just pool) -> q pool
    Nothing -> pure def

-- instance usePoolHandlerM :: UsePool HandlerM where
--   getPool = do
--     env <- ask

-- instance manageTodoHandlerM :: ManageTodo HandlerM where
--   getTodo id = do runQuery (Db.getTodo id) Nothing
