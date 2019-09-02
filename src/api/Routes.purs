module TodoMvc.Api.Routes where

import Prelude

import Data.Maybe (Maybe(..))
import Node.Express.App (App, get)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Request (getBodyParam, getRouteParam)
import Node.Express.Response (sendJson)
import TodoMvc.Api.Capability (class ManageTodo, class ManageUser, getTodo, getTodos)
import TodoMvc.Data.Todo (todoId)
import TodoMvc.Util.Int (parseInt)

type ApiRunner m env = Functor m => env -> m ~> HandlerM
type ApiHandler m = forall env. ApiRunner m env -> env -> Handler
type TodoHandler = forall m. ManageTodo m => ApiHandler m
type UserHandler = forall m. ManageUser m => ApiHandler m
type UniversalHandler = forall m. ManageTodo m => ManageUser m => ApiHandler m
type Routes m = forall e. ApiRunner m e -> e -> App

getTodoHandler :: TodoHandler
getTodoHandler runM env = do
  idParam <- getRouteParam "id"
  case idParam of
    (Just id) -> do
      todo <- runM env $ getTodo (todoId $ parseInt id)
      sendJson todo
    _ -> pure unit

getTodosHandler :: TodoHandler
getTodosHandler runM env = do
  sendJson $ runM env $ getTodos

todos :: forall m. ManageTodo m => Routes m
todos runM env = do
  get "/todos" (getTodosHandler runM env)
  get "/todos/:id" (getTodoHandler runM env)
