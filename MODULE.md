# Module Documentation

## Module VirtualDOM.VTree.Typed

### Types

    data Attribute

    type AttributeName = String

    data CssProperty

    type Handler eff = Foreign -> Eff eff Unit

    type Key = String

    type Namespace = String

    type TagName = String

    type VTree = VT.VTree


### Values

    attr :: AttributeName -> String -> Attribute

    css :: String -> String -> CssProperty

    handler :: forall eff. AttributeName -> Handler eff -> Attribute

    toggle :: AttributeName -> Boolean -> Attribute

    vnode :: TagName -> [Attribute] -> [VTree] -> Maybe Key -> Maybe Namespace -> VTree



