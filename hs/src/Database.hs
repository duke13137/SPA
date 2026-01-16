{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement

runDB :: Session.Session a -> IO (Either ConnectionError (Either SessionError a))
runDB sess = withConnection $ \conn -> Connection.use conn sess

withConnection :: (Connection.Connection -> IO a) -> IO (Either ConnectionError a)
withConnection handler = do
  setting <- getConnectionSetting
  runExceptT $ acquire setting >>= \connection -> use connection <* release connection
  where
    acquire settings = ExceptT $ Connection.acquire settings
    use connection = lift $ handler connection
    release connection = lift $ Connection.release connection

withPool :: (Pool.Pool -> IO a) -> IO a
withPool handler = do
  setting <- getConnectionSetting
  withPoolSetting 3 10 1800 1800 setting handler
  where
    withPoolSetting poolSize acqTimeout maxLifetime maxIdletime connectionSetting =
      bracket
        ( Pool.acquire
            ( Pool.settings
                [ Pool.size poolSize,
                  Pool.acquisitionTimeout acqTimeout,
                  Pool.agingTimeout maxLifetime,
                  Pool.idlenessTimeout maxIdletime,
                  Pool.staticConnectionSettings connectionSetting
                ]
            )
        )
        Pool.release

getConnectionSetting :: IO Connection.Settings
getConnectionSetting = do
  host <- lookupEnv "PGHOST"
  port <- lookupEnv "PGPORT"
  dbname <- lookupEnv "PGDATABASE"
  user <- lookupEnv "PGUSER"
  password <- lookupEnv "PGPASSWORD"
  return $ mconcat
            [ Connection.hostAndPort (maybe "127.0.0.1" toText host) (fromMaybe 5432 (readMaybe (fromMaybe "" port))),
              Connection.dbname (maybe "postgres" toText dbname),
              Connection.user (maybe "postgres" toText user),
              Connection.password (maybe (error "PGPASSWORD") toText password)
            ]
