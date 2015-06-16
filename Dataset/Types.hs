{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Dataset.Types (
  advance, step, from, exec, perform, invoke,
  WhenM(..), CacheM(..), RoomM(..), ItemM(..), MixM(..), RoundM(..),
  DSWhenM, DSCacheM, DSRoomM, DSItemM, DSMixM, DSRoundM
  ) where

import Control.Monad hiding (when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Prelude hiding (round)

import Types
import Util

advance :: (Num s, Monad m) => s -> StateT s m ()
advance = modify . (+)

step :: (Num s, Monad m) => StateT s m ()
step = advance 1

from :: (Monad m) => m s -> StateT s m a -> m a
from = flip ((=<<) . evalStateT)

exec :: (Monad m) => StateT s m a -> m s -> m s
exec = (=<<) . execStateT

perform :: (Num s, Monad m) => StateT s m a -> m a
perform = from $ return 0

class WhenM t where
  when :: t Integer

class CacheM t where
  cache :: t Cache

class RoomM t where
  room :: t Integer

class ItemM t where
  item :: t Integer

class MixM t where
  mix :: t [EthWord]
  storeMix :: [EthWord] -> t ()

class RoundM t where
  round :: t Integer

-- When
type DSWhenM = Reader Integer

instance WhenM DSWhenM where
  when = ask

-- Cache
type DSCacheM = ReaderT Cache DSWhenM

instance CacheM DSCacheM where
  cache = ask

invoke :: DSCacheM a -> Cache -> Integer -> a
invoke = runReader .* runReaderT

-- Room
type DSRoomM = StateT Integer DSCacheM

instance RoomM DSRoomM where
  room = get

-- Item
type DSItemM = StateT Integer DSRoomM

instance ItemM DSItemM where
  item = get

-- Mix
type DSMixM = StateT [EthWord] DSItemM

instance MixM DSMixM where
  mix = get
  storeMix = put

-- Round
type DSRoundM = StateT Integer DSMixM

instance RoundM DSRoundM where
  round = get

-- Inductive instances

instance (Monad m, WhenM m, MonadTrans t) => WhenM (t m) where
  when = lift when

instance (Monad m, CacheM m, MonadTrans t) => CacheM (t m) where
  cache = lift cache

instance (Monad m, RoomM m, MonadTrans t) => RoomM (t m) where
  room = lift room

instance (Monad m, ItemM m, MonadTrans t) => ItemM (t m) where
  item = lift item

instance (Monad m, MixM m, MonadTrans t) => MixM (t m) where
  mix = lift mix
  storeMix = lift . storeMix

instance (Monad m, RoundM m, MonadTrans t) => RoundM (t m) where
  round = lift round
