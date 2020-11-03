module App where

import Prelude
import Effect.Aff.Class (class MonadAff)
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
import Data.Cell (World, JumpPath, emptyWorld, step, modifyTest, gridHamiltonianPath, combine, move)
import Control.Comonad.Cofree.Memoized as M
import Data.Array (mapWithIndex)
import Data.Tuple (snd)

gridPath :: JumpPath Unit
gridPath = gridHamiltonianPath 32 32

type State
  = { world :: World Boolean }

data Action
  = Init
  | Step
  | HandleKey H.SubscriptionId KeyboardEvent
  | Toggle Int Int

initialWorld :: World Boolean
initialWorld = emptyWorld 32

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { world: initialWorld }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.text "" -- (showJumpPath gridPath)
    , HH.button
      [ HE.onClick \_ -> Just Step ]
      [ HH.text "Step" ]
    , HH.div
       [ HP.classes [ HH.ClassName "world" ] ]
       
       (mapWithIndex (\i a -> let x = i `mod` 32
                                  y = i `div` 32
                              in 
                                HH.div
                                  [ HP.classes [ HH.ClassName if a then "cell-true" else "cell-false" ]
                                  , HE.onClick \_ -> Just (Toggle x y) ]
                                  [  ]
                    ) (M.collect combine gridPath state.world))
    ]
    

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Init -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
    let go n = randomBool
    rands <- H.liftEffect $ tailRecM go 32*32

  Toggle x y -> H.modify_ \{ world } -> { world: modifyTest x y not world }
  Step ->
    H.modify_ \{ world } ->
      let
        world' = step world

        _ = M.forceCofree world'
      in
        { world: world' }
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
    | otherwise ->
        pure unit
