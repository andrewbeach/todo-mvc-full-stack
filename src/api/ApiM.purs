module TodoMvc.Api.ApiM where

import Prelude

import Control.Monad.Reader.Trans
  (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Maybe (Maybe)
import Database.Postgres as Pg
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, read)
import Node.Express.Handler (HandlerM)
import TodoMvc.Api.Capability
  (class ManageTodo, class ManageUser, class UsePool)
import TodoMvc.Api.Database as Db
import Type.Equality (class TypeEquals, from)

type Pool = Pg.Pool

type ApiEnv =
  { pool :: Ref (Maybe Pool) }

newtype ApiM a = ApiM (ReaderT ApiEnv HandlerM a)
derive newtype instance functorApiM :: Functor ApiM
derive newtype instance applyApiM :: Apply ApiM
derive newtype instance applicativeApiM :: Applicative ApiM
derive newtype instance bindApiM :: Bind ApiM
derive newtype instance monadApiM :: Monad ApiM
derive newtype instance monadEffectApiM :: MonadEffect ApiM
derive newtype instance monadAffApiM :: MonadAff ApiM

instance monadAskApiM :: TypeEquals e ApiEnv => MonadAsk e ApiM where
  ask = ApiM $ asks from

instance usePoolApiM :: UsePool ApiM where
  getPool = do
    env <- ask
    liftEffect $ read env.pool

instance manageTodoApiM :: ManageTodo ApiM where
  getTodo id = Db.getTodo id
  getTodos = Db.getTodos

instance manageUserApiM :: ManageUser ApiM where
  getUser id = Db.getUser id
  getUsers = Db.getUsers

runApiM :: ApiEnv -> ApiM ~> HandlerM
runApiM env (ApiM m) = runReaderT m env
