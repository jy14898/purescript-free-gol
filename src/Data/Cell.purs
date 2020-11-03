module Data.Cell where

import Prelude
import Control.Comonad.Cofree.Memoized (Cofree, buildCofree, explore, head, modify') as M
import Control.Monad.Free (Free, liftF, runFree, hoistFree)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Comonad (extend)
import Data.Tuple.Nested ((/\))
import Data.Variant (SProxy(..), Variant, inj, match)
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (class Foldable, foldl, foldMapDefaultR)

-- | The two functors, Cell and CoCell, defined in this file are used for 
-- | representing the game grid as a Cofree monad and for representing walks
-- | through the grid as a Free monad
type Directions a
  = ( north :: a, south :: a, east :: a, west :: a )

newtype Cell a
  = Cell { | Directions a }

derive instance newtypeCell :: Newtype (Cell a) _

instance functorCell :: Functor Cell where
  map f (Cell r) =
    Cell
      { north: f r.north, south: f r.south, west: f r.west, east: f r.east }

instance foldableCell :: Foldable Cell where
  foldr f b (Cell r) = f r.north $ f r.east $ f r.south $ f r.west b
  foldl f b (Cell r) = f (f (f (f b r.north) r.east) r.south) r.west
  foldMap = foldMapDefaultR

data CoCell a
  = CoCell (Variant (Directions Unit)) a

instance showCoCell :: (Show a) => Show (CoCell a) where
  show (CoCell var a) = show var <> " -> " <> show a

instance functorCoCell :: Functor CoCell where
  map f (CoCell v a) = CoCell v $ f a

type Path a
  = Free CoCell a

type JumpPath a
  = Free (Free CoCell) a

showDirectionVariant :: forall a. Variant (Directions a) -> String
showDirectionVariant v =
  match
    { north: const "N"
    , east: const "E"
    , south: const "S"
    , west: const "W"
    }
    v

showPath :: forall a. Show a => Path a -> String
showPath p = runFree go $ show <$> p
  where
  go :: CoCell (Path String) -> Path String
  go (CoCell v a) = (\s -> showDirectionVariant v <> " -> " <> s) <$> a

showJumpPath :: forall a. Show a => JumpPath a -> String
showJumpPath p = runFree go $ show <$> p
  where
  go :: Path (JumpPath String) -> JumpPath String
  go = (map \s -> "READ -> " <> s) <$> runFree go2

  go2 (CoCell v a) = (map (\s -> showDirectionVariant v <> " -> " <> s)) <$> a

type World a
  = M.Cofree Cell a

move :: { | Directions (Path Unit) }
move =
  unwrap
    $ (\f -> liftF $ CoCell (f unit) unit)
    <$> Cell
        { north: inj (SProxy :: _ "north")
        , south: inj (SProxy :: _ "south")
        , east: inj (SProxy :: _ "east")
        , west: inj (SProxy :: _ "west")
        }

move' :: Int -> Int -> Path Unit
move' targetX targetY = tailRecM go { x: targetX, y: targetY }
  where
  go { x: 0, y } = case compare y 0 of
    LT -> Loop { x: 0, y: y + 1 } <$ move.south
    GT -> Loop { x: 0, y: y - 1 } <$ move.north
    EQ -> pure $ Done unit

  go { x, y } = case compare x 0 of
    LT -> Loop { x: x + 1, y } <$ move.west
    GT -> Loop { x: x - 1, y } <$ move.east
    EQ -> pure $ Done unit

combine :: forall x y. CoCell (x -> y) -> Cell x -> y
combine (CoCell a k) b = match (unwrap $ (\v _ -> k v) <$> b) a

explore' :: forall a b. Path (a -> b) -> World a -> b
explore' = M.explore combine

modifyTest :: forall a. Int -> Int -> (a -> a) -> World a -> World a
modifyTest x y f world = M.modify' combine (const f <$> move' x y) world

-- I can't believe this function doesn't already exist as part of Variant
-- https://discourse.purescript.org/t/using-a-variant-to-update-a-single-label-in-a-record/379
setCell :: forall x. CoCell x -> Cell x -> Cell x
setCell (CoCell a k) (Cell r) =
  Cell
    $ match
        { north: \_ -> r { north = k }
        , south: \_ -> r { south = k }
        , east: \_ -> r { east = k }
        , west: \_ -> r { west = k }
        }
        a

neighbours8 :: forall a. World a -> Array a
neighbours8 = \root -> dirs <#> \dir -> explore' dir root
  where
  dirs =
    [ move.north
    , move.east
    , move.south
    , move.west
    , move.north <* move.east
    , move.south <* move.east
    , move.south <* move.west
    , move.north <* move.west
    ]
      <#> (identity <$ _)

count :: Array Boolean -> Int
count = foldl toInt 0
  where
  toInt acc = case _ of
    true -> acc + 1
    false -> acc

step :: World Boolean -> World Boolean
step = extend $ rule <$> count <<< neighbours8 <*> M.head
  where
  rule = case _, _ of
    2, true -> true
    3, _ -> true
    _, _ -> false

emptyWorld :: Int -> World Boolean
emptyWorld size =
  M.buildCofree
    (\(x /\ y) -> false /\ (wrap <$> next x y))
    (0 /\ 0)
  where
  wrap (x /\ y) = x `mod` size /\ y `mod` size

  next x y =
    Cell
      { north: x /\ (y + 1)
      , south: x /\ (y - 1)
      , east: (x - 1) /\ y
      , west: (x + 1) /\ y
      }

foreign import safeIndex :: forall a. Array a -> Int -> a

initializeWorld :: forall a. Array a -> Int -> World a
initializeWorld xs size =
  M.buildCofree
    (\(x /\ y) -> safeIndex xs (x + y * size) /\ (wrap <$> next x y))
    (0 /\ 0)
  where
  wrap (x /\ y) = x `mod` size /\ y `mod` size

  next x y =
    Cell
      { north: x /\ (y + 1)
      , south: x /\ (y - 1)
      , east: (x - 1) /\ y
      , west: (x + 1) /\ y
      }

gridHamiltonianPath :: Int -> Int -> JumpPath Unit
gridHamiltonianPath width height = tailRecM (\i -> go i <* easts) (height - 1)
  where
  -- North and south are flipped it seems, doesn't matter
  diagonal :: JumpPath Unit
  diagonal = liftF $ move.north <* move.east

  lineReset :: JumpPath Unit
  lineReset = liftF $ move' ((-width) + 1) 1

  read :: JumpPath Unit
  read = liftF $ pure unit

  easts :: JumpPath Unit
  easts = hoistFree liftF $ move' (width - 1) 0

  go :: Int -> JumpPath (Step Int Unit)
  go n
    | n == height - 1 = read $> Loop (n - 1)

  --go n = diagonal $> if n == 0 then Done unit else Loop (n - 1)
  go n = lineReset $> if n == 0 then Done unit else Loop (n - 1)
