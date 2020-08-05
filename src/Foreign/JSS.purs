module Foreign.JSS
  ( JSSConfig
  , JSSInstance
  , JSSStyleSheet
  , JSS
  , jss
  , createInstance
  , preset
  , createStyleSheet
  , globalAttachStyleSheet
  , toStringStyleSheet
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)

foreign import data JSSConfig :: Type

foreign import data JSSInstance :: Type

foreign import data JSSStyleSheet :: Type

foreign import data JSS :: Type

jss :: forall styles. { | styles } -> JSS
jss = unsafeCoerce

createInstance :: Effect JSSConfig -> Effect JSSInstance
createInstance = runEffectFn1 _createInstance

foreign import _createInstance :: EffectFn1 (Effect JSSConfig) JSSInstance

foreign import preset :: Effect JSSConfig

createStyleSheet :: JSSInstance -> JSS -> Effect JSSStyleSheet
createStyleSheet = runEffectFn2 _createStyleSheet

foreign import _createStyleSheet :: EffectFn2 JSSInstance JSS JSSStyleSheet

globalAttachStyleSheet :: JSSStyleSheet -> Effect Unit
globalAttachStyleSheet = runEffectFn1 _globalAttachStyleSheet

foreign import _globalAttachStyleSheet :: EffectFn1 JSSStyleSheet Unit

foreign import toStringStyleSheet :: JSSStyleSheet -> String
