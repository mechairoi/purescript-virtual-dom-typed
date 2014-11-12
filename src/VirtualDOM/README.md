# Module Documentation

## Module VirtualDOM.Typed

### Types

    newtype Attr

    type AttributeName  = String

    newtype CssProperty

    newtype Handler eff a

    newtype Hook eff a

    newtype Style

    newtype Toggle


### Type Classes

    class Attribute a where
      attrName :: a -> AttributeName
      attrValue :: a -> Foreign


### Type Class Instances

    instance attributeAttr :: Attribute Attr

    instance attributeHandler :: Attribute (Handler eff a)

    instance attributeHook :: Attribute (Hook eff a)

    instance attributeStyle :: Attribute Style

    instance attributeToggle :: Attribute Toggle


### Values

    attr :: AttributeName -> String -> Attr

    handler :: forall eff a. AttributeName -> (Foreign -> Eff eff a) -> Handler eff a

    hook :: forall eff a. AttributeName -> (VTree -> String -> Eff eff a) -> Hook eff a

    key :: String -> Key

    node :: forall a. (Attribute a) => TagName -> [a] -> [VTree] -> Maybe Key -> Maybe Namespace -> VTree

    prop :: String -> String -> CssProperty

    style :: [CssProperty] -> Style

    toggle :: AttributeName -> Boolean -> Toggle



