module TodoMvc.Test.Data.Todo where

import Prelude

import Test.QuickCheck (Result, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import TodoMvc.Data.Todo (TodoWithMetadata)
import TodoMvc.Data.Todo as T

encodeTodoBijection :: TodoWithMetadata -> Result
encodeTodoBijection t = (T.decodeTodo <<< T.encodeTodo) t
                    === pure t

todoSpec :: Spec Unit
todoSpec = do
  describe "Data.Todo" do
    describe "JSON" do
      it "encoding is bijective" do
        quickCheck encodeTodoBijection
