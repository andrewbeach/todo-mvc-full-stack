module TodoMvc.Api.Database where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Database.Postgres as PG
import Database.Postgres.SqlValue (toSql)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Foreign (Foreign)
import Simple.JSON as JSON
import TodoMvc.Api.Capability (class UsePool, getPool)
import TodoMvc.Data.Todo (TodoId, TodoWithMetadata)
import TodoMvc.Data.User (User, UserId)

read' :: forall a. JSON.ReadForeign a => Foreign -> Either Error a
read' = lmap (error <<< show) <<< JSON.read

type QueryOne m a = PG.Client -> m (Maybe a)
type QueryMany m a = PG.Client -> m (Array a)

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

mkPool :: forall m. MonadEffect m => PG.ClientConfig -> PG.PoolConfig -> m PG.Pool
mkPool cc pc = liftEffect $ PG.mkPool $ PG.connectionInfoFromConfig cc pc

withPool :: forall m a. UsePool m => a -> (PG.Client -> Aff a) -> m a
withPool default f = do
  pool <- getPool
  liftAff $ case pool of
    (Just p) -> PG.withClient p f
    _ -> pure default

getTodo :: forall m. UsePool m => TodoId -> m (Maybe TodoWithMetadata)
getTodo id = withPool Nothing \c -> do
  let query = PG.Query "SELECT * from todos WHERE id = $1"
      params = [ toSql $ unwrap id ]
  PG.queryOne read' query params c

getTodos :: forall m. UsePool m => m (Array TodoWithMetadata)
getTodos = withPool [] \c -> do
  let query = PG.Query "select * from todos"
  PG.query_ read' query c

getUser :: forall m. UsePool m => UserId -> m (Maybe User)
getUser id = withPool Nothing \c -> do
  let query = PG.Query "SELECT * from users WHERE id = $1"
      params = [ toSql $ unwrap id ]
  PG.queryOne read' query params c

getUsers :: forall m. UsePool m => m (Array User)
getUsers = withPool [] \c -> do
  let query = PG.Query "select * from users"
  PG.query_ read' query c
