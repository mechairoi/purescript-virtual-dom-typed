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
  , CssProperty()
  -- , Hook()
  , attr
  , toggle
  -- , style
  , handler
  -- , hook
  , css
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
  """ :: forall props. Fn5 TagName { | props } [VTree] Key Namespace VTree

vnode :: TagName -> [Attribute] -> [VTree] -> Maybe Key -> Maybe Namespace -> VTree
vnode tagName attrs children key ns =
  runFn5 vnode' tagName (attrsToRecord attrs) children (fromMaybe unsafeNull key) (fromMaybe unsafeNull ns)
  where attrsToRecord :: forall props. [Attribute] -> { | props }
        attrsToRecord attrs = unsafeCoerce $ Data.StrMap.fromList $ Data.Array.map attrToTuple attrs
        unsafeStrMapToRecord :: forall props. StrMap Foreign -> { | props }
        unsafeStrMapToRecord = unsafeCoerce
        attrToTuple :: Attribute -> Tuple AttributeName Foreign
        attrToTuple a = Tuple (attrName a) (attrValue a)

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

data Attribute = StringAttribute AttributeName String
               | ToggleAttribute AttributeName Boolean
               -- | StyleAttribute [CssProperty]
               | HandlerAttribute AttributeName (forall eff. Handler eff)
               -- | HookAttribute -- TODO implement

attr :: AttributeName -> String -> Attribute
attr = StringAttribute

toggle :: AttributeName -> Boolean -> Attribute
toggle = ToggleAttribute

-- style :: forall eff. [CssProperty] -> Attribute
-- style = StyleAttribute

handler :: forall eff. AttributeName -> Handler eff -> Attribute
handler name handler = HandlerAttribute name (unsafeCoerce handler)
                       -- XXX should not use unsafeCoerce

-- hook :: AttributeName -> Hook eff -> Attribute
-- hook = HookAttribute

data CssProperty = CssProperty String String

css :: String -> String -> CssProperty
css = CssProperty

attrName (StringAttribute name value) = name
attrName (ToggleAttribute name value) = name
-- attrName (StyleAttribute a) = "style"
attrName (HandlerAttribute name value) = name
-- attrName (HookAttribute name value) = name

attrValue (StringAttribute name value) = toForeign value
attrValue (ToggleAttribute name value) = toForeign value
-- attrValue (StyleAttribute a) = toForeign $ cssPropertiesToRecord a.props
  where cssPropertiesToRecord :: forall props a. [CssProperty] -> { | props }
        cssPropertiesToRecord ps = unsafeStrMapToRecord
                               $ Data.StrMap.fromList
                               $ Data.Array.map (\(CssProperty k v) -> Tuple k v) ps
        unsafeStrMapToRecord :: forall props. StrMap String -> { | props }
        unsafeStrMapToRecord = unsafeCoerce
attrValue (HandlerAttribute name value) = handlerWrapper value
-- attrValue (HookAttribute name value) = hookWrapper value

foreign import handlerWrapper """
  function handlerWrapper(handler) {
    return function(event) {
      handler(event)();
    };
  }
  """ :: forall eff. Handler eff -> Foreign

-- XXX: not implemented
-- foreign import hookWrapper """
--  var hookWrapper = (function() {
--    function _Hook(hook) { this.value = hook; };
--    _Hook.prototype.hook = function(node, propertyName) {
--      (this.value)(node)(propertyName)();
--    };
--    return function(hook) {
--      return new _Hook(hook);
--    };
--  })();
-- """ :: forall eff a. (Hook eff) -> Foreign

unsafeCoerce :: forall a b. a -> b
unsafeCoerce x = unsafeFromForeign (toForeign x)
