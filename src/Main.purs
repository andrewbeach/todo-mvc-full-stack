module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import TodoMvc.Api as Api
import TodoMvc.Data.Todo (todoId)

main :: Effect Unit
main = do
  -- let todoJson = encodeTodo { id: todoId 1, title: "My todo" }
  log "🍝"
  _ <- Api.main
  pure unit
