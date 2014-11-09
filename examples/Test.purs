module Test.VirtualDom.Typed where

import qualified VirtualDOM as V
import VirtualDOM.VTree
import VirtualDOM.Typed
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Maybe
import Data.Function
import Prelude
import DOM

main :: forall e. Eff (dom :: DOM, ref :: Ref | e) Unit
main = do
  refSt <- initState
  (State st) <- readRef refSt
  documentBodyAppendChild(st.node)
  update refSt st.app

foreign import documentBodyAppendChild " \
  \ function documentBodyAppendChild(node) { \
  \   return function() { \
  \     window.onload = function() { \
  \       document.body.appendChild(node); \
  \     }; \
  \   }; \
  \ } " :: forall e. DOM.Node -> Eff (dom :: DOM | e) Unit

initState :: forall e. Eff (ref :: Ref | e) (RefVal State)
initState = do
  let tree = (node "div" ([] :: [Attr]) [] Nothing Nothing) :: VTree
  let node = createElement tree :: DOM.Node
  newRef $ State { app: 0, tree: tree, node: node }

type AppState = Number

newtype State = State { app :: AppState
                      , tree :: VTree
                      , node :: DOM.Node
                      }

render :: RefVal State -> AppState -> VTree
render refSt app = node "div" ([] :: [Attr])
  [ node "p" ([] :: [Attr]) [ vtext $ show app ] Nothing Nothing
  , node "button"
      [ handler "onclick" \e -> increment refSt ]
      [ vtext "++" ]
      Nothing Nothing
  ] Nothing Nothing

increment :: forall e. RefVal State -> Eff (dom :: DOM, ref :: Ref | e) Unit
increment refSt = do
  (State st) <- readRef refSt
  update refSt (st.app + 1)

update :: forall e. RefVal State -> AppState -> Eff (dom :: DOM, ref :: Ref | e) Unit
update refSt app = do
  let newTree = render refSt app
  (State st) <- readRef refSt
  let patches = diff st.tree newTree
  patch st.node patches
  writeRef refSt $ State st { app = app, tree = newTree }
  return unit

foreign import data Patches :: *

foreign import patchImpl " \
  \ function patchImpl(node, patches) { \
  \   return function() { require('virtual-dom/patch')(node, patches); } \
  \ } " :: forall e. Fn2 DOM.Node Patches (Eff (dom :: DOM | e) Unit)

patch :: forall e. DOM.Node -> Patches -> Eff (dom :: DOM | e) Unit
patch = runFn2 patchImpl

foreign import diffImpl
  "var diffImpl = require('virtual-dom/diff');"
  :: Fn2 VTree VTree Patches

diff :: VTree -> VTree -> Patches
diff = runFn2 diffImpl

foreign import createElement
  "var createElement = require('virtual-dom/create-element');"
  :: VTree -> DOM.Node
