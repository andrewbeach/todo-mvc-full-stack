module TodoMvc.Api.AppM where

import Prelude

import Control.Monad.Reader.Trans
  (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Database.Postgres (Pool)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, read, write)
import Node.HTTP (Server)
import TodoMvc.Api.Capability
  (class ManageTodo, class ManagePool, class ManageUser, getPool)
import TodoMvc.Api.Express
import TodoMvc.Api.Database (defaultClientConfig, mkPool)
import TodoMvc.Api.Database as Db
import Type.Equality (class TypeEquals, from)

type Env =
  { count :: Int
  , pool  :: Ref (Maybe Pool)
  }

newtype AppM a = AppM (ReaderT Env Aff a)
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance managePoolAppM :: ManagePool AppM where
  startPool = do
    _ <- liftEffect $ log "starting pool"
    env <- ask
    pool <- mkPool defaultClientConfig
    _ <- liftEffect $ write (Just pool) env.pool
    pure pool

-- runQuery :: forall m f a. UsePool m => (Pool -> m (f a)) -> f a -> m (f a)
-- runQuery q default = do
--   mPool <- getPool
--   case mPool of
--     (Just pool) -> q pool
--     Nothing -> pure default

-- instance manageTodoAppM :: ManageTodo AppM where
--   getTodo id = runQuery (Db.getTodo id) Nothing
--   getTodos = runQuery Db.getTodos []

-- instance manageUserAppM :: ManageUser AppM where
--   getUser id =
--     pure $ Just { fname: "Andrew"
--                 , lname: "Beach"
--                 , id
--                 }

-- instance manageServerAppM :: ManageServer AppM where
--   runServer = do
--     liftEffect do
--       app <- mkApplication
--       _listenHttp app 1234 (\_ -> pure unit)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

-- instance manageServerAppM :: ManageServer AppM where
--   runServer app = liftEffect $ _listenHttp app 123 (\_ -> pure unit)
--   mkApplication = liftEffect $ mkApplication

-- instance manageRoutesAppM :: ManageRoutes AppM where
--   http app method route = runFn4 _http app (show method) route
--   get app route = runFn4 _http app "get" route
