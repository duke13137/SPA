{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2024               #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeApplications      #-}
module Main
  ( module Main
  ) where

import Optics
import Prelude hiding (Handler, id)

import Colog
import Network.HTTP.Media ((//), (/:))
import Network.Wai (Middleware, rawPathInfo, requestMethod)
import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.UrlMap qualified as Wai
import Rapid
import Servant.API
import Servant.Server
import Web.Twain as Twain

import Database
import Htmx
import Todo

-- $> main
main :: IO ()
main = do
  bracket acquirePool releasePool \pool ->
    rapid 0 \r -> restart r "server" $
      Wai.run 8080 (app pool)

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

type HelloAPI = "hello" :> Capture "name" Text :> Get '[HTML] (Html ())

servant :: Pool -> Server HelloAPI
servant = helloHandler

helloHandler :: Pool -> Text -> Handler (Html ())
helloHandler _pool name = pure [hsx|<h1 id="hello">Hello, {name}!</h1>|]

htmxApp :: Pool -> Application
htmxApp pool = serve (Proxy @HelloAPI) (servant pool)

app :: Pool -> Application
app pool = Wai.mapUrls $
        Wai.mount "htmx" (htmxApp pool)
    <|> Wai.mountRoot (foldr ($) (Twain.notFound page404) (routes pool))

logger :: MonadIO m => LoggerT Message m a -> m a
logger = usingLoggerT $ cmap fmtMessage logTextStdout

logRequest :: Middleware
logRequest nextApp req sendResponse = do
  let method = requestMethod req
  let path = rawPathInfo req
  logger $ logInfo $ "REQ " <> decodeUtf8 method <> " " <> decodeUtf8 path
  nextApp req sendResponse

routes :: Pool -> [Middleware]
routes pool =
  [ logRequest
  , Twain.get "/" index
  ] <> todoRoutes pool

render :: Html () -> ResponderM a
render = Twain.send . Twain.html . renderBS

index :: ResponderM a
index  = send $ Twain.html "<h1>Welcome!</h1>"

page404 :: ResponderM a
page404 = send $ Twain.html "<h1>Not found...</h1>"
