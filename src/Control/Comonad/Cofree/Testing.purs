module Control.Comonad.Cofree.Testing where

import Prelude
import Control.Monad.Free (Free, runFreeM)
import Control.Comonad (class Extend, class Comonad, extract)
import Data.Tuple (Tuple(..), fst, snd)
import Control.Monad.State (State, runState, state)
import Control.Monad.Rec.Class (tailRecM, Step(..))
-- import Data.Function.Memoize (memoize, class Tabulate)
import Data.Array (mapWithIndex, unsafeIndex)
import Data.Array as A
import Data.Array.ST as STA
import Control.Monad.ST.Ref as STR
import Control.Monad.ST as ST
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse, sequence, class Traversable)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

foreign import data Cofree :: (Type -> Type) -> Type -> Type

type CofreeInternal f a
  = { index :: Int
    , xs :: Array a
    , ys :: Array (f Int)
    }

-- could i existentially hide the mutable reference inside here?
-- does that even make sense? not really because you have to ST.run, but I wonder if you could pass it on to a different region
foreign import mkCofree :: forall a f. CofreeInternal f a -> Cofree f a

foreign import unCofree :: forall a f. Cofree f a -> CofreeInternal f a

head :: forall f a. Cofree f a -> a
head = go <<< unCofree
  where
  go { xs, index } = unsafePartial (unsafeIndex xs index)

-- | Returns the "subtrees" of a tree.
-- TODO: Store the functor instance inside CofreeInternal
tail :: forall f a. Functor f => Cofree f a -> f (Cofree f a)
tail = go <<< unCofree
  where
  go r =
    let
      y = unsafePartial (unsafeIndex r.ys r.index)
    in
      (\i -> mkCofree $ r { index = i }) <$> y

buildCofree :: forall s f a. Ord s => Traversable f => (s -> Tuple a (f s)) -> s -> Cofree f a
buildCofree f is = ST.run (buildCofreeST f is)

buildCofreeST :: forall r s f a. Ord s => Traversable f => (s -> Tuple a (f s)) -> s -> ST.ST r (Cofree f a)
buildCofreeST f initialSeed = do
  xs <- STA.empty
  ys <- STA.empty
  indexMapRef <- STR.new M.empty
  let
    -- Replace with Dissectable traverseP eventually
    -- For now, convert the functor to a list and push to a stack
    -- then 'after' we can map a lookup with guaranteed success
    go =  {-tailRecM-} \seed -> do
      indexMap <- STR.read indexMapRef
      case M.lookup seed indexMap of
        Just index -> pure ( {-Done-}index)
        Nothing -> do
          let
            index = M.size indexMap
          _ <- STR.write (M.insert seed index indexMap) indexMapRef
          _ <- STA.push (undefined) xs
          _ <- STA.push (undefined) ys
          --let (Tuple value children) = f seed
          --let asdf = children -- functor containing next seeds f s
          --let (asdf2 :: ?help) = traverse (\a -> [a]) children
          v <- sequence (traverse go <$> f seed)
          _ <- STA.poke index (fst v) xs
          _ <- STA.poke index (snd v) ys
          -- ?
          pure ( {-Done-}index)
  _ <- go initialSeed
  xs' <- STA.freeze xs
  ys' <- STA.freeze ys
  pure $ mkCofree { index: 0, xs: xs', ys: ys' }

-- MAYBE offset the mapping by the current index?
instance functorCofree :: Functor f => Functor (Cofree f) where
  map f = mkCofree <<< go <<< unCofree
    where
    go r = r { xs = map f r.xs }

instance extendCofree :: Functor f => Extend (Cofree f) where
  extend f = mkCofree <<< go <<< unCofree
    where
    go r = r { xs = mapWithIndex (\i _ -> f $ mkCofree $ r { index = i }) r.xs }

instance comonadCofree :: Functor f => Comonad (Cofree f) where
  extract = head

-- Didn't expect this to work first time
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

--explore pair m w = case runState (runFreeM step m) (unCofree w) of
--  Tuple f cof -> f (extract $ mkCofree cof)
--  where
--  step :: f (Free f (a -> b)) -> State (CofreeInternal g a) (Free f (a -> b))
--  step ff = state \cof -> pair (map Tuple ff) (tail cof)
reassociate ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f a ->
  Cofree g b ->
  Tuple a (Cofree g b)
reassociate pair m w = runState (runFreeM step m) w
  where
  step :: f (Free f a) -> State (Cofree g b) (Free f a)
  step ff = state \cof -> pair (map Tuple ff) (tail cof)

-- | Given a walk (with jumps) through the Cofree structure, collect the heads
-- | I have no idea how it works for anything other than what I'm using it for
collect ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free (Free f) a ->
  Cofree g b ->
  Array b
collect pair m w = case runState (runFreeM step m) { s1: w, s2: [] } of
  Tuple _ { s2 } -> s2
  where
  step :: Free f (Free (Free f) a) -> State ({ s1 :: Cofree g b, s2 :: Array b }) (Free (Free f) a)
  step ff = state \{ s1, s2 } -> (\y -> { s1: y, s2: s2 <> [ extract y ] }) <$> reassociate pair ff s1

modify ::
  forall a g.
  Functor g =>
  (a -> a) ->
  Cofree g a ->
  Cofree g a ->
  Cofree g a
modify f target = mkCofree <<< go <<< unCofree
  where
  { index } = unCofree target

  go r = r { xs = unsafePartial (fromJust (A.modifyAt index f r.xs)) }

modify' ::
  forall a f g.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f (a -> a) ->
  Cofree g a ->
  Cofree g a
modify' combine path world = modify f root world
  where
  Tuple f root = reassociate combine path world
