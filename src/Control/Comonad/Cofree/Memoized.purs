module Control.Comonad.Cofree.Memoized
  ( Cofree
  , head
  , tail
  , buildCofree
  , explore
  , forceCofree
  ) where

import Prelude
import Control.Comonad.Cofree as C
import Control.Monad.Free (Free, runFreeM)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend)
import Control.Monad.State (State, runState, state)
import Data.Lazy (Lazy, force, defer)
import Data.Tuple (Tuple(..))
import Data.Function.Memoize (memoize, class Tabulate)
import Data.Foldable (class Foldable)
import Data.Array (fromFoldable)

newtype Cofree f a
  = Cofree (C.Cofree f a)

unCofree :: forall f a. Cofree f a -> C.Cofree f a
unCofree (Cofree v) = v

foreign import mapUncofree :: forall f a. f (C.Cofree f a) -> f (Cofree f a)

head :: forall f a. Cofree f a -> a
head = C.head <<< unCofree

-- | Returns the "subtrees" of a tree.
tail :: forall f a. Cofree f a -> f (Cofree f a)
tail = mapUncofree <<< C.tail <<< unCofree

-- | Recursively unfolds a `Cofree` structure given a seed.
buildCofree ::
  forall f s a.
  Tabulate s =>
  Functor f =>
  (s -> Tuple a (f s)) ->
  s ->
  Cofree f a
buildCofree k = Cofree <<< go
  where
  go = memoize \v -> buildCofree' v

  buildCofree' s = C.deferCofree \_ -> map go <$> k s

-- | Explore a value in the cofree comonad by using an expression in a
-- | corresponding free monad.
-- |
-- | The free monad should be built from a functor which pairs with the
-- | functor underlying the cofree comonad.
explore ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f (a -> b) ->
  Cofree g a ->
  b
explore pair m w = case runState (runFreeM step m) w of
  Tuple f cof -> f (extract cof)
  where
  step :: f (Free f (a -> b)) -> State (Cofree g a) (Free f (a -> b))
  step ff = state \cof -> pair (map Tuple ff) (tail cof)

instance functorCofree :: Functor f => Functor (Cofree f) where
  map f = Cofree <<< go
    where
    go = memoize \x -> loop x

    loop v = C.deferCofree \_ -> Tuple (f $ head v) $ go <$> tail v

instance extendCofree :: Functor f => Extend (Cofree f) where
  extend f = Cofree <<< go
    where
    go = memoize \x -> loop x

    loop v = C.deferCofree \_ -> Tuple (f v) $ go <$> tail v

instance comonadCofree :: Functor f => Comonad (Cofree f) where
  extract = head

foreign import refTabulate :: forall a r. (forall x. (Unit -> x) -> Lazy x) -> (a -> r) -> a -> Lazy r

refMemoize :: forall a b. (a -> b) -> a -> b
refMemoize f = force <<< f1
  where
  f1 = refTabulate defer f

instance tabulateCofree :: Tabulate (Cofree f a) where
  tabulate = refTabulate defer

foreign import refOnce :: forall a. (a -> Array a) -> a -> Unit

-- | Note that the constraint here is causing it to not have a global WeakSet
-- | which is fine
forceCofree :: forall a f. Foldable f => Cofree f a -> Unit
forceCofree = refOnce (\v -> fromFoldable $ C.tail $ v) <<< unCofree
