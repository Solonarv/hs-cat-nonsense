{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language NoImplicitPrelude #-}
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
{-# language ConstraintKinds #-}
{-# language ViewPatterns #-}
module OrderedCat where

import Prelude hiding (Functor, id, (.), toEnum, fromEnum)
import qualified Prelude

import Categories -- https://gist.github.com/ekmett/b26363fc0f38777a637d

import Finite -- finite type-indexed vectors and naturals

-- The type of monotonic functions.
data TO a b where
  MkTO :: (Ord a, Ord b) => (a -> b) -> TO a b -- don't export this constructor

runTO :: TO a b -> a -> b
runTO (MkTO f) = f

-- The category of totally-ordered types
instance Category TO where
  type Ob TO = Ord
  
  id = MkTO id
  (MkTO f) . (MkTO g) = MkTO (f . g)
  
  source (MkTO _) = Dict
  target (MkTO _) = Dict

instance Functor (TO a) where
  type Dom (TO a) = TO
  type Cod (TO a) = (->)
  fmap = (.)

instance Functor TO where
  type Dom TO = Y TO
  type Cod TO = Nat TO (->)
  fmap (Y f) = Nat (. f)

-- one of the TO primitives
gte :: Ord a => a -> TO a Bool
gte a = MkTO (a <=)

branch :: Ord a
       => a -- ^ value for False
       -> a -- ^ value for True
       -> Maybe (TO Bool a)
branch f t | f <= t = Just . MkTO $ \b -> if b then t else f
           | otherwise = Nothing

-- Operates under the assumption that Enum instances are monotonic.
toEnum :: (Ord a, Enum a) => TO Int a
toEnum = MkTO Prelude.toEnum

fromEnum :: (Ord a, Enum a) => TO a Int
fromEnum = MkTO Prelude.fromEnum

-- * The real workhorses of TO
-- Note: these could be written using less stringent
-- types, but that would rather defeat the point
-- of this exercise!

-- | Given a vector of n values, constructs an arrow
-- which maps @a@ to the number of values in the vector
-- strictly less than it.
partition :: Ord a => Vec n a -> TO a (Fin n)
partition (sort -> v) = MkTO (part v)
  where
    part :: Ord a' => Vec n' a' -> a' -> Fin n'
    part v a = case v of
      VZ -> FZ
      VS x xs -> if a <= x then FZ else FS (part xs a)

-- | Given a vector of (n+1) values, constructs an
-- arrow which maps @n@ to the n-th smallest value
-- in the vector.
select :: Ord a => Vec (S n) a -> TO (Fin n) a
select (sort -> v) = MkTO (sel v)
  where
    sel :: Vec (S n') a' -> Fin n' -> a'
    sel (VS x _)  FZ     = x
    sel (VS _ xs) (FS n) = sel xs n