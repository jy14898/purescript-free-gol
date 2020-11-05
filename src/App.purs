module App where

import Prelude
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomBool)
import Effect.Exception (error)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Halogen.Query.EventSource as ES
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Cell (World, JumpPath, emptyWorld, step, modifyTest, gridHamiltonianPath, combine, move, initializeWorld)
--import Control.Comonad.Cofree.Memoized as M
import Control.Comonad.Cofree.Testing as M
import Control.Monad.Rec.Class (tailRecM, Step(..), forever)
import Data.Array (mapWithIndex, cons)
import Data.Tuple (snd)
import Data.Time.Duration (Milliseconds(..))

type State
  = { world :: World Boolean
    , viewSize :: Int
    , gridPath :: JumpPath Unit
    , animate :: Boolean
    , autoStep :: Maybe H.SubscriptionId
    }

data Action
  = Init
  | ZoomIn
  | ZoomOut
  | Step
  | HandleKey H.SubscriptionId KeyboardEvent
  | Toggle Int Int
  | ToggleAnimation
  | ToggleAutoStep

initialWorld :: World Boolean
initialWorld = emptyWorld 32

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { world: initialWorld
          , viewSize: 32
          , gridPath: gridHamiltonianPath 32 32
          , animate: true
          , autoStep: Nothing
          }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "app" ] ]
    [ HH.text "" -- (showJumpPath gridPath)
    , HH.div
        [ HP.classes [ HH.ClassName "app-controls" ] ]
        [ HH.button
            [ HE.onClick \_ -> Just Step ]
            [ HH.text "Step" ]
        , HH.button
            [ HE.onClick \_ -> Just ZoomIn ]
            [ HH.text "+" ]
        , HH.button
            [ HE.onClick \_ -> Just ZoomOut ]
            [ HH.text "-" ]
        , HH.button
            [ HE.onClick \_ -> Just ToggleAnimation ]
            [ HH.text "Toggle Animation" ]
        , HH.button
            [ HE.onClick \_ -> Just ToggleAutoStep ]
            [ HH.text "Toggle Auto Step" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "world" ]
        , HP.style
            ( ("grid-template-columns: repeat(" <> show state.viewSize <> ", 5fr);")
                <> ("grid-template-rows: repeat(" <> show state.viewSize <> ", 5fr);")
            )
        ]
        ( mapWithIndex
            ( \i a ->
                let
                  x = i `mod` state.viewSize

                  y = i `div` state.viewSize
                in
                  HH.div
                    [ HE.onClick \_ -> Just (Toggle x y)
                    , HP.classes [ HH.ClassName "cell-container" ]
                    ]
                    [ HH.div
                        [ HP.classes
                            ( [ HH.ClassName "cell"
                              , HH.ClassName
                                  if a then "cell-true" else "cell-false"
                              ]
                                <> if state.animate then [ HH.ClassName "cell-animated" ] else []
                            )
                        ]
                        []
                    ]
            )
            (M.collect combine state.gridPath state.world)
        )
    ]

runAutoStep :: forall m. MonadAff m => ES.EventSource m Action
runAutoStep =
  ES.affEventSource \emitter -> do
    fiber <-
      Aff.forkAff
        $ forever do
            Aff.delay $ Milliseconds 250.0
            ES.emit emitter Step
    pure
      $ ES.Finalizer do
          Aff.killFiber (error "Event source finalized") fiber

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Init -> do
    window <- H.liftEffect Web.window
    document <- H.liftEffect $ Web.document window
    H.subscribe' \sid ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
    let
      go { n: 0, xs } = pure $ Done xs -- $ Done xs

      go { n, xs } = do
        x <- randomBool
        pure $ Loop { n: n - 1, xs: cons x xs }
    rands <- H.liftEffect $ tailRecM go { n: 32 * 32, xs: [ false ] }
    H.modify_ \st -> st { world = initializeWorld rands 32 }
    --sId <- H.subscribe runAutoStep
    --H.modify_ \st -> st { autoStep = Just sId }
    pure unit
  ToggleAnimation -> H.modify_ \st@{ animate } -> st { animate = not animate }
  ToggleAutoStep -> do
    { autoStep } <- H.get
    autoStep' <- case autoStep of
      Just sId -> do
        H.unsubscribe sId
        pure Nothing
      Nothing -> do
        sId <- H.subscribe runAutoStep
        pure $ Just sId
    H.modify_ \st -> st { autoStep = autoStep' }
  ZoomIn ->
    H.modify_ \st@{ viewSize } ->
      let
        viewSize' = max (viewSize - 1) 3
      in
        st { viewSize = viewSize', gridPath = gridHamiltonianPath viewSize' viewSize' }
  ZoomOut ->
    H.modify_ \st@{ viewSize } ->
      let
        viewSize' = viewSize + 1
      in
        st { viewSize = viewSize', gridPath = gridHamiltonianPath viewSize' viewSize' }
  Toggle x y -> H.modify_ \st@{ world } -> st { world = modifyTest x y not world }
  Step ->
    H.modify_ \st@{ world } ->
      let
        world' = step world
      -- forceCofree is necessary for larger size worlds, as lazy chains can
      -- get too long and cause stack overflows
      --_ = M.forceCofree world'
      in
        st { world = world' }
  HandleKey sid ev
    | KE.key ev == "c" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ (\st -> st { world = initialWorld })
    | KE.key ev == "ArrowRight" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ (\st -> st { world = snd $ M.reassociate combine move.east st.world })
    | KE.key ev == "ArrowLeft" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ (\st -> st { world = snd $ M.reassociate combine move.west st.world })
    | KE.key ev == "ArrowUp" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ (\st -> st { world = snd $ M.reassociate combine move.south st.world })
    | KE.key ev == "ArrowDown" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ (\st -> st { world = snd $ M.reassociate combine move.north st.world })
    | otherwise -> pure unit
