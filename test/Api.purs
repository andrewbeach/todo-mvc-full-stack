module TodoMvc.Test.Api where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Express.App (App)
import Node.HTTP (Server)
import TodoMvc.Test.Api.ApiTestM (runApiTestM)
import TodoMvc.Api.Routes as Routes
import TodoMvc.Api.Server (serve)

app :: App
app = do
  let env = { }
  Routes.todos runApiTestM env

main :: Effect Server
main = do
  let port = 4000
  serve app port $ \_ ->
    log $ "listening test on " <> show port
