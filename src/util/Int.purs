module TodoMvc.Util.Int where

import Prelude

import Data.Int (fromString)
import Data.Maybe (fromMaybe)

parseInt :: String -> Int
parseInt = fromMaybe 0 <<< fromString
