{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.Total.FSNF
  ( TotalMapFSNF(..)
  ) where

import Control.Lens
import Data.Map (Map)
import Data.Align
import qualified Data.Map as Map
import Data.Containers.NonEmpty

newtype TotalMapFSNF k v = MkTotalMapFSNF { getTotalMapFSNF :: Map k (NE v) }

underNonEmpty
  :: (HasNonEmpty s, HasNonEmpty t)
  => (s -> t)
  -> NE s -> Maybe (NE t)
underNonEmpty f = nonEmpty . f . fromNonEmpty

underNonEmptyA
  :: (HasNonEmpty s, HasNonEmpty t, Applicative f)
  => (s -> f t)
  -> NE s -> f (Maybe (NE t))
underNonEmptyA f = fmap nonEmpty . f . fromNonEmpty

--

map
  :: (HasNonEmpty a, HasNonEmpty b)
  => (a -> b)
  -> TotalMapFSNF k a -> TotalMapFSNF k b
map f = over _Wrapped $ Map.mapMaybe (underNonEmpty f)

traverseWithKey
  :: (HasNonEmpty a, HasNonEmpty b, Applicative f)
  => (k -> a -> f b)
  -> TotalMapFSNF k a -> f (TotalMapFSNF k b)
traverseWithKey f = _Wrapped $ Map.traverseMaybeWithKey (fmap underNonEmptyA f)

liftA2
  :: (HasNonEmpty a, HasNonEmpty b)
  => (a -> b -> c)
  -> TotalMapFSNF k a -> TotalMapFSNF k b -> TotalMapFSNF k c
liftA2 f (MkTotalMapFSNF x) (MkTotalMapFSNF y) = MkTotalMapFSNF
  $ Map.mapMaybe id
  $ alignWith (underNonEmpty $ uncurry f) x y

--

makeWrapped ''TotalMapFSNF
