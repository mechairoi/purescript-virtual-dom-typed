module VirtualDOM.Typed
  ( node
  , attr
  , toggle
  , key
  , style
  , prop
  , hook
  , handler
  , CssProperty()
  , Attr()
  , Toggle()
  , Style()
  , AttributeName()
  , Handler()
  , Hook()
  , Attribute
  , attrName
  , attrValue
  ) where

import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import Data.Maybe
import Data.StrMap
import Data.Foreign
import Data.Tuple

newtype Attr = Attr { attrName :: AttributeName, attrValue :: String }
newtype Toggle = Toggle { attrName :: AttributeName, attrValue :: Boolean }
newtype CssProperty = CssProperty (Tuple String String)
newtype Style = Style { props :: [CssProperty] }
newtype Handler eff a = Handler { attrName :: AttributeName
                          , handler :: Foreign -> Eff eff a }
newtype Hook eff a = Hook { attrName :: AttributeName
                          , hook :: VTree -> String -> Eff eff a }

class Attribute a where
  attrName :: a -> AttributeName
  attrValue :: a -> Foreign

type AttributeName = String

attrToTuple :: forall a. (Attribute a) => a -> Tuple AttributeName Foreign
attrToTuple a = Tuple (attrName a) (attrValue a)

instance attributeAttr :: Attribute Attr where
  attrName (Attr a) = a.attrName
  attrValue (Attr a) = toForeign a.attrValue

instance attributeToggle :: Attribute Toggle where
  attrName (Toggle a) = a.attrName
  attrValue (Toggle a) = toForeign a.attrValue

instance attributeStyle :: Attribute Style where
  attrName (Style a) = "style"
  attrValue (Style a) = toForeign $ cssPropertiesToRecord a.props
    where cssPropertiesToRecord :: forall props a. [CssProperty] -> { | props }
          cssPropertiesToRecord ps = unsafeStrMapToRecord
                                 $ Data.StrMap.fromList
                                 $ Data.Array.map (\(CssProperty p) -> p) ps
          unsafeStrMapToRecord :: forall props. StrMap String -> { | props }
          unsafeStrMapToRecord = unsafeCoerce

instance attributeHandler :: Attribute (Handler eff a) where
  attrName (Handler h) = h.attrName
  attrValue h = handlerWrapper h

instance attributeHook :: Attribute (Hook eff a) where
  attrName (Hook h) = h.attrName
  attrValue h = hookWrapper h

foreign import handlerWrapper """
 function handlerWrapper(handler) {
   return function(event) {
     handler.handler(event)();
   };
 }
""" :: forall eff a. (Handler eff a) -> Foreign

-- XXX node type is VTree?
foreign import hookWrapper """
 var hookWrapper = (function() {
   function _Hook(hook) { this.value = hook; };
   _Hook.prototype.hook = function(node, propertyName) {
     (this.value)(node)(propertyName)();
   };
   return function(hook) {
     return new _Hook(hook.hook);
   };
 })();
""" :: forall eff a. (Hook eff a) -> Foreign

node :: forall a. (Attribute a) => TagName -> [a] -> [VTree] -> Maybe Key -> Maybe Namespace -> VTree
node tagName attrs contents key ns = vnode tagName (attrsToRecord attrs) contents key ns
  where attrsToRecord :: forall props a. (Attribute a) => [a] -> { | props }
        attrsToRecord attrs = unsafeCoerce $ Data.StrMap.fromList $ Data.Array.map attrToTuple attrs
        unsafeStrMapToRecord :: forall props. StrMap Foreign -> { | props }
        unsafeStrMapToRecord = unsafeCoerce

unsafeCoerce :: forall a b. a -> b
unsafeCoerce x = unsafeFromForeign (toForeign x)

attr :: AttributeName -> String -> Attr
attr attrName attrValue = Attr { attrName: attrName, attrValue: attrValue }

toggle :: AttributeName -> Boolean -> Toggle
toggle attrName attrValue = Toggle { attrName: attrName, attrValue: attrValue }

key :: String -> Key
key k = k

style :: [CssProperty] -> Style
style props = Style { props: props }

prop :: String -> String -> CssProperty
prop property value = CssProperty $ Tuple property value

handler :: forall eff a. AttributeName -> (Foreign -> Eff eff a) -> Handler eff a
handler attrName h = Handler { attrName: attrName, handler: h }

hook :: forall eff a. AttributeName -> (VTree -> String -> Eff eff a) -> Hook eff a
hook attrName h = Hook { attrName: attrName, hook: h }
