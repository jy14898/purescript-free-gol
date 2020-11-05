module Main where

import Prelude
import App as App

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import SDOM (attach)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Effect.Random (randomBool)
import Data.Array (cons)

nRand { n: 0, xs } = pure $ Done xs -- $ Done xs
nRand { n, xs } = do
  x <- randomBool
  pure $ Loop { n: n - 1, xs: cons x xs }

main :: Effect Unit
main = do
  document <- map toNonElementParentNode (window >>= document)
  container <- getElementById "container" document
  case container of
    Just el ->
      void do
         rands <- tailRecM nRand { n: 32 * 32, xs: [ false ] }
         attach el (App.initializeState rands)  App.component
    Nothing -> throw "No 'container' node!"
