module TodoMvc.Test.Api.ApiTestM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Maybe (Maybe(..))
import Node.Express.Handler (HandlerM)
import TodoMvc.Api.Capability (class ManageTodo)
import TodoMvc.Data.Todo (TodoWithMetadata, todoId)
import TodoMvc.Data.User (userId)
import Type.Equality (class TypeEquals, from)

type TestEnv = { }

newtype ApiTestM a = ApiTestM (ReaderT TestEnv HandlerM a)
derive newtype instance functorApiTestM :: Functor ApiTestM
derive newtype instance applyApiTestM :: Apply ApiTestM
derive newtype instance applicativeApiTestM :: Applicative ApiTestM
derive newtype instance bindApiTestM :: Bind ApiTestM
derive newtype instance monadApiTestM :: Monad ApiTestM

instance monadAskApiTestM :: TypeEquals e TestEnv => MonadAsk e ApiTestM where
  ask = ApiTestM $ asks from

testTodo1 :: TodoWithMetadata
testTodo1 =
  { id: todoId 12345
  , title: "Test Todo"
  , userId: userId 1
  }

testTodo2 :: TodoWithMetadata
testTodo2 =
  { id: todoId 54321
  , title: "Another one"
  , userId: userId 1
  }

instance manageTodoApiTestM :: ManageTodo ApiTestM where
  createTodo uid todo = pure $ Just $ testTodo1.id
  getTodo id = pure $ Just testTodo1
  getTodos uid = pure $ [ testTodo1, testTodo2 ]

runApiTestM :: TestEnv -> ApiTestM ~> HandlerM
runApiTestM env (ApiTestM m) = runReaderT m env
