module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import TodoMvc.Api as Api
import TodoMvc.Data.Todo (decodeTodo, encodeTodo, todoId)

main :: Effect Unit
main = do
  let todoJson = encodeTodo { id: todoId 1, title: "My todo" }
  log "🍝"
  _ <- Api.main
  pure unit
