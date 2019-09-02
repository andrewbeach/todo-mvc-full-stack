module TodoMvc.Api.ApiM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Maybe (Maybe(..))
import Database.Postgres as Pg
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, read)
import Node.Express.Handler (HandlerM)
import TodoMvc.Api.Capability (class ManageTodo, class UsePool, getPool)
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

runQuery :: forall m f a. UsePool m => (Pool -> m (f a)) -> f a -> m (f a)
runQuery query default = do
  pool <- getPool
  case pool of
    (Just p) -> query p
    Nothing -> pure default

instance manageTodoApiM :: ManageTodo ApiM where
  getTodo id = runQuery (Db.getTodo id) Nothing
  getTodos = runQuery Db.getTodos []

runApiM :: ApiEnv -> ApiM ~> HandlerM
runApiM env (ApiM m) = runReaderT m env
