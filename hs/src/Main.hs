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

import Optics
import Prelude

import Debug.Breakpoint
import Rapid

import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.UrlMap qualified as Wai
import Web.Twain as Twain

import Web.Atomic.CSS
import Web.Hyperbole as H

import Colog
import Database
import Htmx

-- $> main
main :: IO ()
main = do
  rapid 0 \r -> restart r "server" $
    Wai.run 8080 app

app :: Application
app = Wai.mapUrls $
        Wai.mount "hyper" hyperApp
    <|> Wai.mountRoot (foldr ($) (Twain.notFound page404) routes)

hyperApp :: Application
hyperApp  = H.liveApp H.quickStartDocument (H.runPage hyperView)

routes :: [Middleware]
routes =
  [ Twain.get "/" index
  , Twain.get "/hello/:name" echoName
  ]

htmx :: Html () -> Html ()
htmx body = [hsx|
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>My Simple HTML Page</title>
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css">
      <script src="https://cdn.jsdelivr.net/npm/htmx.org@4.0.0-alpha6/dist/htmx.js"></script>
    </head>
    <body>
      <main class="container">
      {body}
      </main>
    </body>
  </html>
|]

logger :: MonadIO m => LoggerT Message m a -> m a
logger = usingLoggerT $ cmap fmtMessage logTextStdout

render :: Html () -> ResponderM a
render = Twain.send . Twain.html . renderBS

index :: ResponderM a
index = render $ htmx [hsx|
    <button hx-get="/hello/world" hx-target="#hello">
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

page404 :: ResponderM a
page404 = send $ Twain.html "<h1>Not found...</h1>"


hyperView :: Page es '[Event]
hyperView = do
  pure $ do
    hyper Event1 $ hyperButton "Hello"
    hyper Event2 $ hyperButton "World!"

data Event = Event1 | Event2
  deriving (Generic, ViewId)

instance HyperView Event es where
  data Action Event
    = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pure $ hyperButton new

hyperButton :: Text -> View Event ()
hyperButton msg = do
  button (Louder msg) (H.text msg)
