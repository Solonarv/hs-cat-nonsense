{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language DefaultSignatures #-}
{-# language FunctionalDependencies #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
{-# language TypeInType #-}
{-# language StandaloneDeriving #-}
{-# language DeriveFunctor #-}
module Finite where

import Data.Foldable (toList)
import qualified Data.List
import qualified Control.Monad.State as St

-- | A simple natural number type
data N = Z | S N deriving (Eq, Ord)

-- | A simple length-indexed "vector"
data Vec n a where
  VZ ::                 Vec Z     a
  VS :: a -> Vec n a -> Vec (S n) a

deriving instance Functor (Vec n)
instance Foldable (Vec n) where
  foldr k z VZ = z
  foldr k z (VS x xs) = x `k` foldr k z xs
instance Traversable (Vec n) where
  traverse f VZ = pure VZ
  traverse f (VS x xs) = VS <$> (f x) <*> (traverse f xs)

-- the classic unsafe "traversable sort"
sort :: Ord a => Vec n a -> Vec n a
sort v = St.evalState (traverse replace v) (Data.List.sort (toList v))
  where
    replace _ = do (x:xs) <- St.get; St.put xs; pure x

-- | A simple finite integral type
data Fin n where
  FZ ::          Fin n
  FS :: Fin n -> Fin (S n)

instance Eq (Fin n) where
  FZ   == FZ   = True
  FS x == FS y = x == y
  _    == _    = False
instance Ord (Fin n) where
  FZ <= _ = True
  FS x <= FS y = x <= y