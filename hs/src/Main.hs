{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Main (main, app, server) where

import Debug.Breakpoint

import Optics
import Prelude

import Colog
import Html
import Network.Wai.Handler.Warp qualified as Http
import Web.Twain as Http

import Database

main :: IO ()
main = server

logger :: MonadIO m => LoggerT Message m a -> m a
logger = usingLoggerT $ cmap fmtMessage logTextStdout

server :: IO ()
server = Http.runEnv 8080 app

app :: Application
app = foldr ($) (notFound missing) routes

routes :: [Middleware]
routes =
  [ Http.get "/" index
  , Http.get "/echo/:name" echoName
  ]

page :: Html () -> Html ()
page body = [hsx|
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>My Simple HTML Page</title>
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css">
      <script src="https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js" integrity="sha384-Akqfrbj/HpNVo8k11SXBb6TlBWmXXlYQrCSqEWmyKJe+hDm3Z/B2WVG4smwBkRVm" crossorigin="anonymous"></script>
    </head>
    <body>
      <main class="container">
      {body}
      </main>
    </body>
  </html>
|]

render :: Html () -> ResponderM a
render = send . html . renderBS

index :: ResponderM a
index = render $ page [hsx|
    <button hx-get="/echo/world" hx-target="#hello">
        Welcome
    </button>
    <h1 id="hello"></h1>
|]

echoName :: ResponderM a
echoName = logger do
  name <- lift $ param @Text "name"
  logInfo $ "name: " <> name
  -- breakpointM
  lift $ render [hsx|
    <h1 id="hello">Hello, {name}!</h1>
  |]

missing :: ResponderM a
missing = send $ text "Not found..."
