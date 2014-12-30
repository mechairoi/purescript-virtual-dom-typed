module VirtualDOM.VTree.Typed
  ( VTree()
  , TagName()
  , Key()
  , Namespace()
  , vtext
  , vnode
  -- , thunk
  -- , widget

  , Attribute()
  , AttributeName()
  , Handler()
  -- , CssProperty()
  -- , Hook()
  , attr
  , toggle
  -- , style
  , handler
  -- , hook
  -- , css
  ) where

import VirtualDOM
import qualified VirtualDOM.VTree as VT
import Control.Monad.Eff
import Data.Maybe
import Data.StrMap
import Data.Foreign
import Data.Tuple
import Data.Function

type VTree = VT.VTree

type TagName = String
type Key = String
type Namespace = String

-- *** VTree constructors ***

foreign import vnode' """
  var vnode$prime = (function() {
    var VNode = require('vtree/vnode');
    return function (name, props, children, key, ns) {
      return new VNode(name, props, children, key, ns);
    };
  }());
  """ :: Fn5 TagName (StrMap Foreign) [VTree] Key Namespace VTree

vnode :: TagName -> [Attribute] -> [VTree] -> Maybe Key -> Maybe Namespace -> VTree
vnode tagName attrs children key ns =
  runFn5 vnode' tagName (Data.StrMap.fromList attrs) children (fromMaybe unsafeNull key) (fromMaybe unsafeNull ns)

foreign import unsafeNull
  "var unsafeNull = null;" :: forall a. a

vtext = VT.vtext

-- TODO implement
-- thunk :: ??? -> VTree
-- widget :: ??? -> VTree

-- *** VTree attributes ***

type Handler eff = Foreign -> Eff eff Unit

-- type Hook eff = VTree -> String -> Eff eff Unit

type AttributeName = String

type Attribute = Tuple AttributeName Foreign

attr :: AttributeName -> String -> Attribute
attr name value = Tuple name $ toForeign value

toggle :: AttributeName -> Boolean -> Attribute
toggle name value = Tuple name $ toForeign value

-- style :: forall eff. [CssProperty] -> Attribute
-- style = StyleAttribute

handler :: forall eff. AttributeName -> Handler eff -> Attribute
handler name handler = Tuple name $ handlerWrapper handler

foreign import handlerWrapper """
  function handlerWrapper(handler) {
    return function(event) {
      handler(event)();
    };
  }
  """ :: forall eff. Handler eff -> Foreign

-- hook :: AttributeName -> Hook eff -> Attribute

-- data CssProperty = CssProperty String String

-- css :: String -> String -> CssProperty
-- css = CssProperty

unsafeCoerce :: forall a b. a -> b
unsafeCoerce x = unsafeFromForeign (toForeign x)
