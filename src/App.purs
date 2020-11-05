module App where

import Prelude
import Data.Maybe (fromJust)
import Data.Cell (World, JumpPath, step, modifyTest, gridHamiltonianPath, combine, initializeWorld, emptyWorld)
--import Control.Comonad.Cofree.Memoized as M
import Control.Comonad.Cofree.Testing as M
import Data.Array ((..), index)
import SDOM (SDOM, text, withAsync, mapChannel)
import SDOM.Elements as E
import SDOM.Attributes as A
import SDOM.Events as Events
import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Partial.Unsafe (unsafePartial)
import Data.Function.Memoize (memoize)
import Record (insert)
import Data.Symbol (SProxy(..))
import Prim.Row as Row
import FRP.Event (Event)
import FRP.Event.Time (interval)

import Unsafe.Coerce (unsafeCoerce)

type State
  = { world :: World Boolean
    , autoStep :: Boolean
    }

data Action
  = Init
  | Step
  | Toggle Int Int

size :: Int
size = 32

-- BUG if grid path is smaller than map size, there's a failed pattern match
gridPath :: JumpPath Unit
gridPath = gridHamiltonianPath size size

-- Apply the combine and path ahead of time so memoization succeeds
collectPath :: forall a. World a -> Array a
collectPath = M.collect combine gridPath

indexes :: Array Int
indexes = 0 .. (size * size - 1)

initializeState :: Array Boolean -> State
initializeState rands =
  { world: initializeWorld rands size
  , autoStep: true
  }

-- I think this could be optimised to not have to walk around
-- now that we know the underlying thing can be referenced by index
-- as in, can I make M.collect combine gridPath efficient?
compWalk :: forall a. World a -> Array a
compWalk = memoize (\world -> collectPath world)

unpack ::
  forall a r.
  Row.Lacks "walk" r =>
  { world :: World a | r } ->
  { walk :: Array a
  , world :: World a
  | r
  }
unpack r = insert (SProxy :: _ "walk") (compWalk r.world) r

cell ::
  forall channel context r.
  Int ->
  SDOM channel context
    { walk :: Array Boolean
    , world :: World Boolean
    | r
    }
    { walk :: Array Boolean
    , world :: World Boolean
    | r
    }
cell i =
  E.div
    [ A.className \_ _ -> "cell-container" ]
    [ Events.click \_ _ -> pure \r -> r { world = modifyTest x y not r.world } ]
    [ E.div
        [ A.className \_ { walk } -> if unsafePartial (fromJust (index walk i)) then "cell cell-true" else "cell cell-false" ]
        []
        []
    ]
  where
  x = i `mod` size

  y = i `div` size

component' ::
  forall context.
  SDOM Boolean context State State
component' =
  E.div
    [ A.className \_ _ -> "app" ]
    []
    [ E.div
        [ A.className \_ _ -> "app-controls" ]
        []
        [ E.button
            []
            [ Events.click \_ _ -> pure \st -> st { world = step st.world } ]
            [ text \_ _ -> "Step" ]
        , E.input
            [ A.type_ \_ _ -> "checkbox" ]
            [ Events.change \_ e -> Left $ (unsafeCoerce e).target.checked ] --pure \st -> st { autoStep = not st.autoStep } ]
            [] -- [ text \_ _ -> "Toggle Auto Step" ]
        ]
    , dimap (unpack) (\{ world, autoStep } -> { world, autoStep })
        $ E.div
            [ A.className \_ _ -> "world" ]
            []
            (cell <$> indexes)
    ]

component :: forall channel context. SDOM channel context State State
component = withAsync (mapChannel (map Right <<< interpreter) component')
  where
  interpreter :: Boolean -> Event (State -> State)
  interpreter true = interval 250 $> \r -> r { autoStep = true, world = step r.world }
  interpreter false = pure \r -> r { autoStep = false }
