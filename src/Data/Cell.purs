module Data.Cell where

import Prelude
import Control.Comonad.Cofree.Memoized (Cofree, buildCofree, explore, forceCofree, head) as M
--import Effect.Aff (Fiber, launchAff)
--import Effect (Effect)
--import Effect.Class.Console (logShow)
import Control.Monad.Free (Free, liftF)
import Control.Comonad (extend)
import Data.Tuple.Nested ((/\))
import Data.Variant (SProxy(..), Variant, inj, match)
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (class Foldable, foldl, foldMapDefaultR)

type Directions a
  = ( north :: a, south :: a, east :: a, west :: a )

data CoCell a
  = CoCell (Variant (Directions Unit)) a

instance functorCocell :: Functor CoCell where
  map f (CoCell v a) = CoCell v $ f a

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

move :: { | Directions (Free CoCell Unit) }
move =
  unwrap
    $ (\f -> liftF $ CoCell (f unit) unit)
    <$> Cell
        { north: inj (SProxy :: _ "north")
        , south: inj (SProxy :: _ "south")
        , east: inj (SProxy :: _ "east")
        , west: inj (SProxy :: _ "west")
        }

explore' :: forall a b. Free CoCell (a -> b) -> M.Cofree Cell a -> b
explore' = M.explore f
  where
  f :: forall x y. CoCell (x -> y) -> Cell x -> y
  f (CoCell a k) b = match (unwrap $ (\x _ -> k x) <$> b) a

neighbours8 :: forall a. M.Cofree Cell a -> Array a
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

step :: M.Cofree Cell Boolean -> M.Cofree Cell Boolean
step = extend $ rule <$> count <<< neighbours8 <*> M.head
  where
  rule = case _, _ of
    2, true -> true
    3, _ -> true
    _, _ -> false

world :: M.Cofree Cell Boolean
world =
  M.buildCofree
    (\(x /\ y) -> (x `mod` 2 == 0) /\ (wrap <$> next x y))
    (0 /\ 0)
  where
  wrap (x /\ y) = x `mod` 256 /\ y `mod` 256

  next x y =
    Cell
      { north: x /\ (y + 1)
      , south: x /\ (y - 1)
      , east: (x - 1) /\ y
      , west: (x + 1) /\ y
      }

-- this will run the function 2^n number of times
myIterate :: forall a. Int -> (a -> a) -> a -> a
myIterate 0 f = f

myIterate n f = myIterate (n - 1) (f <<< f)

-- applies it 1024 times on the stack (without forcing in between)
thousand :: M.Cofree Cell Boolean -> M.Cofree Cell Boolean
thousand x = y
  where
  --y = myIterate 10 step x
  y = myIterate 0 step x

  _ = M.forceCofree y

--main :: Effect (Fiber Unit)
--main =
--  launchAff do
--    logShow "Waiting..."
--    --delay $ Milliseconds 10000.0
--    let
--      path :: forall a. Free CoCell (a -> a)
--      path = pure identity
--
--      --{head: world', tail: tail' = unsafePartial $ fromJust $ uncons $ iterate thousand world
--      world' = thousand world
--    logShow $ explore' path world'
--    let
--      world'' = thousand world'
--    logShow $ explore' path world''
--    let
--      world''' = thousand world''
--    logShow $ explore' path world'''
--    let
--      world'''' = thousand world'''
--    logShow $ explore' path world''''
--    let
--      world''''' = thousand world''''
--    logShow $ explore' path world'''''
