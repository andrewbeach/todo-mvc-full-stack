module TodoMvc.Data.Todo
  ( Todo
  , TodoId
  , TodoMetadataRep
  , TodoRep
  , TodoWithMetadata
  , decodeTodo
  , encodeTodo
  , todoId
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Newtype (class Newtype, wrap)
import Simple.JSON (class ReadForeign)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)
import Type.Row (type (+))

newtype TodoId = TodoId Int

todoId :: Int -> TodoId
todoId = wrap

derive instance newTypeTodoId :: Newtype TodoId _
derive newtype instance eqTodoId :: Eq TodoId
derive newtype instance ordTodoId :: Ord TodoId

instance arbitraryTodoId :: Arbitrary TodoId where
  arbitrary = todoId <$> chooseInt 1 1000

derive newtype instance readForeignTodoId :: ReadForeign TodoId

instance showTodoId :: Show TodoId where
  show (TodoId i) = "<Todo" <> show i <> ">"

instance decodeJsonTodoId :: DecodeJson TodoId where
  decodeJson = decodeJson >=> pure <<< TodoId

instance encodeJsonTodoId :: EncodeJson TodoId where
  encodeJson (TodoId i) = encodeJson i

type TodoRep row =
  ( title :: String
  | row
  )

type TodoMetadataRep row =
  ( id :: TodoId
  | row
  )

type Todo = { | TodoRep () }
type TodoWithMetadata = { | TodoRep + TodoMetadataRep () }

decodeTodo :: Json -> Either String TodoWithMetadata
decodeTodo = decodeJson

encodeTodo :: TodoWithMetadata -> Json
encodeTodo = encodeJson
