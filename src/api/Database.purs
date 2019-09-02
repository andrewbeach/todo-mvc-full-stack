module TodoMvc.Api.Database where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Database.Postgres as PG
import Database.Postgres.SqlValue (SqlValue, toSql)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Foreign (Foreign)
import Simple.JSON as JSON
import TodoMvc.Data.Todo (TodoId(..), TodoWithMetadata)

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

mkPool :: forall m. MonadEffect m => PG.ClientConfig -> m PG.Pool
mkPool clientConfig = liftEffect $ PG.mkPool connInfo
  where
    connInfo = PG.connectionInfoFromConfig clientConfig defaultPoolConfig

withPool :: forall m a. MonadAff m => (PG.Client -> Aff a) -> PG.Pool -> m a
withPool f pool = liftAff $ PG.withClient pool f

getTodo :: forall m. MonadAff m => TodoId -> PG.Pool -> m (Maybe TodoWithMetadata)
getTodo id = withPool \c -> do
  let query = PG.Query "SELECT * from todos WHERE id = $1"
      params = [ toSql $ unwrap id ]
  PG.queryOne read' query params c

getTodos :: forall m. MonadAff m => PG.Pool -> m (Array TodoWithMetadata)
getTodos = withPool \c -> do
  let query = PG.Query "select * from todos"
  PG.query_ read' query c
