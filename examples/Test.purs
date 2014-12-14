module Test.VirtualDom.Typed where

import qualified VirtualDOM as V
import VirtualDOM.VTree
import VirtualDOM.Typed
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Maybe
import Data.Function
import Prelude

main :: forall e. Eff (dom :: V.DOM, ref :: Ref | e) Unit
main = do
  refSt <- initState
  (State st) <- readRef refSt
  documentBodyAppendChild(st.node)
  update refSt st.app

foreign import documentBodyAppendChild """
  function documentBodyAppendChild(node) {
    return function() {
      window.onload = function() {
        document.body.appendChild(node);
      };
    };
  }
""" :: forall e. V.DOMNode -> Eff (dom :: V.DOM | e) Unit

initState :: forall e. Eff (ref :: Ref | e) (RefVal State)
initState = do
  let tree = (node "div" ([] :: [Attr]) [] Nothing Nothing) :: VTree
  let node = V.createElement tree :: V.DOMNode
  newRef $ State { app: 0, tree: tree, node: node }

type AppState = Number

newtype State = State { app :: AppState
                      , tree :: VTree
                      , node :: V.DOMNode
                      }

render :: RefVal State -> AppState -> VTree
render refSt app = node "div" ([] :: [Attr])
  [ node "p" ([] :: [Attr]) [ vtext $ show app ] Nothing Nothing
  , node "button"
      [ handler "onclick" \e -> increment refSt ]
      [ vtext "++" ]
      Nothing Nothing
  ] Nothing Nothing

increment :: forall e. RefVal State -> Eff (dom :: V.DOM, ref :: Ref | e) Unit
increment refSt = do
  (State st) <- readRef refSt
  update refSt (st.app + 1)

update :: forall e. RefVal State -> AppState -> Eff (dom :: V.DOM, ref :: Ref | e) Unit
update refSt app = do
  let newTree = render refSt app
  (State st) <- readRef refSt
  let patchObj = V.diff st.tree newTree
  V.patch patchObj st.node
  writeRef refSt $ State st { app = app, tree = newTree }
  return unit
