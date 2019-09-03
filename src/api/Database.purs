module TodoMvc.Api.Database where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Database.Postgres as PG
import Database.Postgres.SqlValue (toSql)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Foreign (Foreign)
import Simple.JSON as JSON
import TodoMvc.Api.Capability (class UsePool, getPool)
import TodoMvc.Data.Todo (Todo, TodoId, TodoWithMetadata, todoId)
import TodoMvc.Data.User (User, UserId)

read' :: forall a. JSON.ReadForeign a => Foreign -> Either Error a
read' = lmap (error <<< show) <<< JSON.read

defaultPoolConfig :: PG.PoolConfig
defaultPoolConfig = PG.defaultPoolConfig

defaultClientConfig :: PG.ClientConfig
defaultClientConfig =
  { host: "localhost"
  , port: 5432
  , database: "todo_dev"
  , user: "andrewbeach"
  , password: ""
  , ssl: false
  }

mkPool :: forall m. MonadEffect m =>
          PG.ClientConfig -> PG.PoolConfig -> m PG.Pool
mkPool cc pc = liftEffect $ PG.mkPool $ PG.connectionInfoFromConfig cc pc

withPool :: forall m a. UsePool m => a -> (PG.Client -> Aff a) -> m a
withPool default f = do
  pool <- getPool
  liftAff $ case pool of
    (Just p) -> PG.withClient p f
    _ -> pure default

-- Todos

-- TODO: Make these more ergonomic. Having to escape to get camelCase seems wrong
--   (and caused hard-to-debug runtime errors)
-- "Make incorrect states impossible"
getTodo :: forall m. UsePool m => TodoId -> m (Maybe TodoWithMetadata)
getTodo id = withPool Nothing \c -> do
  let query = PG.Query "SELECT id, user_id AS \"userId\", title FROM todos WHERE id = $1"
      params = [ toSql $ unwrap id ]
  PG.queryOne read' query params c

getTodos :: forall m. UsePool m => UserId -> m (Array TodoWithMetadata)
getTodos userId = withPool [] \c -> do
  let query = PG.Query "SELECT id, user_id AS \"userId\", title FROM todos WHERE user_id = $1"
      params = [ toSql $ unwrap userId ]
  PG.query read' query params c

createTodo :: forall m. UsePool m => UserId -> Todo -> m (Maybe TodoId)
createTodo userId todo = withPool Nothing \c -> do
  let query = PG.Query
            $  "INSERT INTO todos (user_id, title)"
            <> "VALUES ($1, $2)"
            <> "RETURNING id"
      params = [ toSql $ unwrap userId
               , toSql todo.title
               ]
  id <- PG.queryValue read' query params c
  pure $ todoId <$> id


-- Users

getUser :: forall m. UsePool m => UserId -> m (Maybe User)
getUser id = withPool Nothing \c -> do
  let query = PG.Query "SELECT * from users WHERE id = $1"
      params = [ toSql $ unwrap id ]
  PG.queryOne read' query params c

getUsers :: forall m. UsePool m => m (Array User)
getUsers = withPool [] \c -> do
  let query = PG.Query "select * from users"
  PG.query_ read' query c
