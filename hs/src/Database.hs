{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Database where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting
import Hasql.Connection.Setting.Connection.Param qualified as Setting
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))

import Hasql.TH qualified as Sql

runSession :: Session.Session a -> IO (Either Connection.ConnectionError (Either Session.SessionError a))
runSession = withConnection . Session.run

withConnection :: (Connection.Connection -> IO a) -> IO (Either Connection.ConnectionError a)
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

getConnectionSetting :: IO [Setting.Setting]
getConnectionSetting = do
  host <- lookupEnv "PGHOST"
  port <- lookupEnv "PGPORT"
  dbname <- lookupEnv "PGDATABASE"
  user <- lookupEnv "PGUSER"
  password <- lookupEnv "PGPASSWORD"
  return [ Setting.connection $ Setting.params
            [ Setting.host (maybe "127.0.0.1" toText host),
              Setting.port (fromMaybe 5432 (readMaybe (fromMaybe "" port))),
              Setting.dbname (maybe "postgres" toText dbname),
              Setting.user (maybe "postgres" toText user),
              Setting.password (maybe (error "PGPASSWORD") toText password)
            ]
         ]


-- $> test
test :: IO ()
test = do
  runSession (sumAndDivModSession  3 8 3) >>= print
  result <- withPool \pool -> do
    Pool.use pool (sumAndDivModSession 30 8 3)
  print result

sumAndDivModSession :: Int64 -> Int64 -> Int64 -> Session (Int64, Int64)
sumAndDivModSession a b c = do
  -- Get the sum of a and b
  sumOfAAndB <- Session.statement (a, b) sumStatement
  -- Divide the sum by c and get the modulo as well
  Session.statement (sumOfAAndB, c) divModStatement

sumStatement :: Statement (Int64, Int64) Int64
sumStatement =
  [Sql.singletonStatement|
    select ($1 :: int8 + $2 :: int8) :: int8
  |]

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement =
  [Sql.singletonStatement|
    select
      (($1 :: int8) / ($2 :: int8)) :: int8,
      (($1 :: int8) % ($2 :: int8)) :: int8
  |]
