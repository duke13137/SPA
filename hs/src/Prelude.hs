{-# LANGUAGE NoImplicitPrelude #-}
module Prelude (
  module Relude
, module Classes
, module Data.Conduit
, module Data.Strict.Wrapper
, sinkFold, sinkFoldM, sinkFold_, sinkFoldM_
) where

import Relude hiding (STM, atomically, throwSTM)

import Control.Monad.Class.MonadAsync as Classes
import Control.Monad.Class.MonadFork as Classes
import Control.Monad.Class.MonadSay as Classes
import Control.Monad.Class.MonadSTM as Classes
import Control.Monad.Class.MonadThrow as Classes
import Control.Monad.Class.MonadTime as Classes
import Control.Monad.Class.MonadTimer as Classes

import Control.Foldl qualified as L
import Data.Conduit hiding (yield)
import Data.Conduit.Combinators qualified as C
import Data.Strict.Wrapper


sinkFold :: Monad m => L.Fold a b -> ConduitT a o m b
sinkFold = L.purely sinkFold_

sinkFoldM :: Monad m => L.FoldM m a b -> ConduitT a o m b
sinkFoldM = L.impurely sinkFoldM_

sinkFold_ :: Monad m => (x -> a -> x) -> x -> (x -> b) -> ConduitT a o m b
sinkFold_ combine seed extract = fmap extract (C.foldl combine seed)

sinkFoldM_ :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> ConduitT a o m b
sinkFoldM_ combine seed extract =
  lift . extract =<< C.foldM combine =<< lift seed
