module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import TodoMvc.Test.Data.Todo (todoSpec)

main :: Effect Unit
main = do
  log "🍝"
  launchAff_ $ runSpec [consoleReporter] do
    todoSpec
