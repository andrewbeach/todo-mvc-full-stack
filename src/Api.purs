module TodoMvc.Api where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (new)
import Node.Express.App (App)
import Node.HTTP (Server)
import TodoMvc.Api.ApiM (runApiM)
import TodoMvc.Api.Database as Db
import TodoMvc.Api.Routes as Routes
import TodoMvc.Api.Server (serve)

-- | Production express app (using ApiM)
-- | Test express app can be set up with a TestApiM
-- | and corresponding runTestApiM.
app :: App
app = do
  mpool <- Db.mkPool Db.defaultClientConfig Db.defaultPoolConfig
  pool <- liftEffect $ new $ Just mpool
  let env = { pool }
  Routes.todos runApiM env
  Routes.users runApiM env

main :: Effect Server
main = do
  let port = 3000
  serve app port $ \_ ->
    log $ "listening on " <> show port
