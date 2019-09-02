module TodoMvc.Api.Server where

import Prelude

import Effect (Effect)
import Node.Express.App (App, listenHttp)
import Node.Express.Types (Event)
import Node.HTTP (Server)

serve :: App -> Int -> (Event -> Effect Unit) -> Effect Server
serve app port callback = do
  listenHttp app port callback
