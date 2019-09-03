module TodoMvc.Api.Routes where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Node.Express.App (App, get, post)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Request (getBodyParam, getRouteParam)
import Node.Express.Response (sendJson)
import TodoMvc.Api.Capability
  (class ManageTodo, class ManageUser, createTodo, getTodo, getTodos, getUser, getUsers)
import TodoMvc.Data.Todo (todoId)
import TodoMvc.Data.User (userId)
import TodoMvc.Util.Int (parseInt)

-- TODO: make types more friendly here

type ApiRunner m env = Functor m => env -> m ~> HandlerM
type ApiHandler m = forall env. ApiRunner m env -> env -> Handler

-- | Capability-specific handlers
type TodoHandler = forall m. ManageTodo m => ApiHandler m
type UserHandler = forall m. ManageUser m => ApiHandler m
type UniversalHandler = forall m. ManageTodo m => ManageUser m => ApiHandler m

-- | Set up or extend an app with a set of capability-specific handlers
type HandleRoutes m = forall e. ApiRunner m e -> e -> App

createTodoHandler :: TodoHandler
createTodoHandler runM env = do
  todo <- getBodyParam "todo"
  uid <- getRouteParam "id"
  case Tuple uid todo of
    Tuple (Just id) (Just t) -> do
      newId <- runM env $ createTodo (userId $ parseInt id) t
      sendJson newId
    _ -> do
      sendJson "error" -- improve errors

getTodoHandler :: TodoHandler
getTodoHandler runM env = do
  idParam <- getRouteParam "id"
  case idParam of
    (Just id) -> do
      todo <- runM env $ getTodo (todoId $ parseInt id)
      sendJson todo
    _ -> sendJson "url parameter 'id' required"


getTodosHandler :: TodoHandler
getTodosHandler runM env = do
  uidParam <- getRouteParam "id"
  case uidParam of
    (Just uid) -> do
      ts <- runM env $ getTodos (userId $ parseInt uid)
      sendJson ts
    _ -> sendJson "error"

todos :: forall m env. ManageTodo m => ApiRunner m env -> env -> App
todos runApi env = do
  get  "/users/:id/todos" $ getTodosHandler runApi env
  get  "/todos/:id"       $ getTodoHandler runApi env
  post "/users/:id/todos" $ createTodoHandler runApi env

getUserHandler :: UserHandler
getUserHandler runM env = do
  idParam <- getRouteParam "id"
  case idParam of
    (Just id) -> do
      user <- runM env $ getUser (userId $ parseInt id)
      sendJson user
    _ -> pure unit

getUsersHandler :: UserHandler
getUsersHandler runM env = do
  us <- runM env $ getUsers
  sendJson us

users :: forall m env. ManageUser m => ApiRunner m env -> env -> App
users runApi env = do
  get "/users"     $ getUsersHandler runApi env
  get "/users/:id" $ getUserHandler runApi env
