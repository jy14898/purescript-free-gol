module Control.Comonad.Cofree.Testing where

import Prelude
import Control.Monad.Free (Free, runFreeM, hoistFree)
import Control.Comonad (class Extend, class Comonad, extract)
import Data.Tuple (Tuple(..), snd, swap)
import Control.Monad.State (State, runState, state)
import Control.Monad.RWS (RWS, rws, RWSResult(..), runRWS)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Data.Function.Memoize (class Tabulate)
import Data.Lazy (Lazy, force, defer)
import Data.Array (mapWithIndex, unsafeIndex)
import Data.Array as A
import Data.Array.ST as STA
import Control.Monad.ST.Ref as STR
import Control.Monad.ST as ST
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (class Traversable, for)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

-- itd be nice if i could make a way of indexing such that if the cofree has the same region as the index
-- then it's a guaranteed read. I could just make helper functions which guarantee index is always set to
-- something from the arrays?
foreign import data Cofree :: (Type -> Type) -> Type -> Type

-- weird thought:
--   index is a program counter
--   xs is the memory
--   ys is the machine code
-- 
-- and extend evaluates them all in parallel

-- interesting idea:
--   like how some languages use bits of a pointer to indicate type and align them into one region
--   you could have multiple xs + ys which have different types
type CofreeInternal f a
  = { index :: Int
    , xs :: Array a
    , ys :: Array (f Int)
    }

-- could i existentially hide the mutable reference inside here?
-- does that even make sense? not really because you have to ST.run, but I wonder if you could pass it on to a different region
foreign import mkCofree :: forall a f. CofreeInternal f a -> Cofree f a

foreign import unCofree :: forall a f. Cofree f a -> CofreeInternal f a

-- You can do better here, and check if xs,ys,index are the same by reference/value?
foreign import refTabulate :: forall a r. (forall x. (Unit -> x) -> Lazy x) -> (a -> r) -> a -> Lazy r

refMemoize :: forall a b. (a -> b) -> a -> b
refMemoize f = force <<< f1
  where
  f1 = refTabulate defer f

instance tabulateCofree :: Tabulate (Cofree f a) where
  tabulate = refTabulate defer

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

-- | While this is stack safe, the order in which indexes are assigned does not
-- | line up well with GOL grid
buildCofreeST :: forall r s f a. Ord s => Traversable f => (s -> Tuple a (f s)) -> s -> ST.ST r (Cofree f a)
buildCofreeST f initialSeed = do
  xs <- STA.empty
  _ <- STA.push undefined xs
  ys <- STA.empty
  _ <- STA.push undefined ys
  indexMapRef <- STR.new (M.singleton initialSeed 0)
  stack <- STA.empty
  _ <- STA.push initialSeed stack
  let
    go =
      tailRecM \_ -> do
        mSeed <- STA.pop stack
        case mSeed of
          Nothing -> pure (Done unit)
          Just seed -> do
            let
              (Tuple x children) = f seed
            y <-
              for children \child -> do
                indexMap <- STR.read indexMapRef
                case M.lookup child indexMap of
                  Just index -> pure index
                  Nothing -> do
                    let
                      index = M.size indexMap
                    _ <- STR.write (M.insert child index indexMap) indexMapRef
                    _ <- STA.push undefined xs
                    _ <- STA.push undefined ys
                    _ <- STA.push child stack
                    pure index
            indexMap <- STR.read indexMapRef
            let
              index = unsafePartial (fromJust (M.lookup seed indexMap))
            _ <- STA.poke index x xs
            _ <- STA.poke index y ys
            pure (Loop unit)
  _ <- go unit
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

-- Note that xs is ignored here
foreign import myMemoize :: forall f r. ({ index :: Int, ys :: Array (f Int) } -> r) -> { index :: Int, ys :: Array (f Int) } -> r

-- is f not always the same, given a certain functor?
-- remember that g has to be traversable, so what does that mean for f?
-- that it has to be distributive?
-- this is actually used in the neighbour8 function
-- idk if we can make use of the memoization here easily though
explore ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f (a -> b) ->
  Cofree g a ->
  b
explore pair m = final <<< unCofree
  where
  final { index, xs, ys } =
    let
      Tuple f index = once { index, ys }
    in
      f (unsafePartial (fromJust (A.index xs index)))

  once = myMemoize go

  go { index: initialIndex, ys } = runState (runFreeM step m) initialIndex
    where
    step :: f (Free f (a -> b)) -> State Int (Free f (a -> b))
    step ff = state \index -> pair (map Tuple ff) (unsafePartial (fromJust (A.index ys index)))

-- For the snd of the tuple, the above optimisation is also true
reassociate ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f a ->
  Cofree g b ->
  Tuple a (Cofree g b)
reassociate pair m = final <<< unCofree
  where
  once = reassociate' pair m
  final { index, xs, ys } = (\index -> mkCofree { index, xs, ys }) <$> once { index, ys }

reassociate' ::
  forall f g a.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f a ->
  { index :: Int, ys :: Array (g Int) } ->
  Tuple a Int
reassociate' pair m = final
  where
  final { index, ys } = once { index, ys }

  once = myMemoize go

  go { index: initialIndex, ys } = runState (runFreeM step m) initialIndex
    where
    step :: f (Free f a) -> State Int (Free f a)
    step ff = state \index -> pair (map Tuple ff) (unsafePartial (fromJust (A.index ys index)))

-- And here
-- | Given a walk (with jumps) through the Cofree structure, collect the heads
-- | I have no idea how it works for anything other than what I'm using it for
type Helper a b
  = RWS (Array (a Int)) Unit Int b

type Helper2 a
  = RWS (Array (a Int)) Unit Int

collect ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free (Free f) a ->
  Cofree g b ->
  Array b
collect pair m = final <<< unCofree
  where
  trap :: forall x. Free f x -> Helper g x
  trap fr = rws \ys index -> bap (once { index, ys })
    where
    once = reassociate' pair fr

    bap (Tuple a b) = RWSResult b a unit

  asdf :: Free (Helper2 g) a
  asdf = hoistFree trap m

  final { index, xs, ys } =(\i -> unsafePartial (fromJust (A.index xs i))) <$> (_.s2 $ snd $ once { index, ys })

  once = myMemoize go

  go { index: initialIndex, ys } = runState (runFreeM step asdf) { s1: initialIndex, s2: [] }
    where
    step ::
      Helper g (Free (Helper2 g) a) ->
      State
        { s1 :: Int
        , s2 :: Array Int
        }
        (Free (Helper2 g) a)
    step v = state \{ s1, s2 } -> let RWSResult s r _ = runRWS v ys s1 in Tuple r { s1: s, s2: s2 <> [ s ] }

-- step :: Free f (Free (Free f) a) -> State ({ s1 :: Cofree g b, s2 :: Array b }) (Free (Free f) a)
--step ::
--  State (Cofree g b) (Free (State (Cofree g b)) a) ->
--  State ({ s1 :: Int, s2 :: Array b }) (Free (State (Cofree g b)) a)
--step st1 = state \{ s1, s2 } -> let qqq = runState st1 s1 in ?help -- (\y -> { s1: y, s2: s2 <> [ extract y ] }) <$> qqq
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

-- Could optimise this, but modify and reassociate are pretty fast now
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
