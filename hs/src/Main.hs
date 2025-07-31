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
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/basecoat-css@0.3.0/dist/basecoat.cdn.min.css">
      <script src="https://cdn.jsdelivr.net/npm/basecoat-css@0.3.0/dist/js/all.min.js" defer></script>
      <script src="https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js" integrity="sha384-Akqfrbj/HpNVo8k11SXBb6TlBWmXXlYQrCSqEWmyKJe+hDm3Z/B2WVG4smwBkRVm" crossorigin="anonymous"></script>
    </head>
    <body>
      {body}
    </body>
  </html>
|]

render :: Html () -> ResponderM a
render = send . html . renderBS

index :: ResponderM a
index = render $ page [hsx|
    <button class="btn" hx-get="/echo/world" hx-target="#hello">
        Hi
    </button>
    <h1 id="hello">Welcome</h1>
|]

echoName :: ResponderM a
echoName = logger do
  name <- lift $ param @Text "name"
  logInfo $ "name: " <>  name
  -- breakpointM
  lift $ render [hsx|
    <h1 id="hello">Hello, {name}!</h1>
  |]

missing :: ResponderM a
missing = send $ text "Not found..."
