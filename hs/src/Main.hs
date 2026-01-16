{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import Debug.Breakpoint
import Rapid

import Optics
import Prelude

import Colog
import Html
import Network.Wai.Handler.Warp qualified as Http
import Web.Twain as Twain

-- import Web.Atomic.CSS
import Web.Hyperbole as H

import Database

-- $> main
main :: IO ()
main =
  rapid 0 \r -> do
    restart r "http" server
    restart r "hyperbole" $ H.run 8081 $ H.liveApp H.quickStartDocument (H.runPage hello)

logger :: MonadIO m => LoggerT Message m a -> m a
logger = usingLoggerT $ cmap fmtMessage logTextStdout

server :: IO ()
server = Http.runEnv 8080 app

app :: Application
app = foldr ($) (Twain.notFound missing) routes

routes :: [Middleware]
routes =
  [ Twain.get "/" index
  , Twain.get "/hello/:name" echoName
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
  who <- lift $ Twain.param @Text "name"
  logInfo $ "name: " <> who
  -- breakpointM
  lift $ render [hsx|
    <h1 id="hello">Hello, {who}!</h1>
  |]

missing :: ResponderM a
missing = send $ Twain.text "Not found..."


hello :: Page es '[Event]
hello = do
  pure $ do
    hyper Event1 $ messageView "Hello"
    hyper Event2 $ messageView "World!"

data Event = Event1 | Event2
  deriving (Generic, ViewId)

instance HyperView Event es where
  data Action Event
    = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pure $ messageView new

messageView :: Text -> View Event ()
messageView msg = do
  button (Louder msg) (H.text msg)
