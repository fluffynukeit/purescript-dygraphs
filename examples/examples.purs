module Examples where

import DyGraphs
import DOM
import Control.Monad.Eff
import Data.Maybe
import Data.Function

import Debug.Trace

main = do
  tryWithNode "#graph1" $ \d -> newDyGraph d plotData defaultDyOpts
  tryWithNode "#graph2" $ \d -> 
    newDyGraph d plotData2 defaultDyOpts
      { stepPlot = Just true 
      , clickCallback = Just $ \e x pts -> do
          tryWithNode "#msg" $ setText "Canvas clicked!"
      , highlightCallback = Just $ \e x pts r s -> do
          tryWithNode "#msg" $ setText "Point highlighted!"
      , title = Just $ "Chart with non-default options"
      , xlabel = Just $ "X label here"
      , ylabel = Just $ "Y label here!"
      }

tryWithNode divName fun = do
  maybeDiv <- querySelector divName
  case maybeDiv of
    Just div -> void $ fun div
    Nothing -> trace $ "Cannot find node " ++ divName

plotData = CSV "0,1,10\n1,2,20\n2,3,30\n3,4,40"

plotData2 = Array2D $ [ [0, 0, 0, 0]
                      , [0.1, 0.1, -0.2, 0.3]
                      , [0.2, -0.1, 0.2, 0.3]
                      , [0.3, 0.15, -0.25, -0.3] 
                      ]

foreign import querySelectorImpl
  "function querySelectorImpl(r, f, s) {\
  \ return function() {\
  \ var result = document.querySelector(s);\
  \ return result ? f(result) : r;\
  \ };\
  \}" :: forall eff r. Fn3 r (Node -> r) String (Eff (dom :: DOM | eff) r)

querySelector :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s


foreign import setText
  "function setText(text) {\
  \ return function(node) {\
  \ return function() {\
  \ node.textContent = text;\
  \ return node;\
  \ };\
  \ };\
  \}" :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node 
