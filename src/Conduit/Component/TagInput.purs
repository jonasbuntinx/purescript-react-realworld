module Conduit.Component.TagInput where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (key, preventDefault, stopPropagation, targetValue)
import React.Basic.Events (handler, handler_, merge, syntheticEvent)
import React.Basic.Hooks as React

type Props =
  { tags :: Set String
  , onChange :: Set String -> Effect Unit
  }

tagInput :: Props -> React.JSX
tagInput = unsafePerformEffect mkTagInput

mkTagInput :: React.Component Props
mkTagInput = do
  React.component "TagInput" \props -> React.do
    state /\ setState <- React.useState { text: "" }
    pure
      $ R.fieldset
          { className: "form-group"
          , children:
              [ R.input
                  { className: "form-control"
                  , type: "text"
                  , placeholder: "Enter tags"
                  , value: state.text
                  , onChange: handler targetValue $ traverse_ \str -> setState _ { text = str }
                  , onKeyDown:
                      handler (merge { key, syntheticEvent }) \{ key, syntheticEvent } -> case key of
                        Just "Enter" -> do
                          runEffectFn1
                            ( handler (stopPropagation <<< preventDefault) \_ -> do
                                when (state.text /= "") do
                                  props.onChange $ Set.insert state.text props.tags
                                  setState _ { text = "" }
                            )
                            syntheticEvent
                        _ -> pure unit
                  }
              , R.div
                  { className: "tag-list"
                  , children:
                      Set.toUnfoldable props.tags
                        <#> \tag ->
                          R.span
                            { className: "tag-default tag-pill"
                            , children:
                                [ R.i
                                    { className: "ion-close-round"
                                    , onClick: handler_ $ props.onChange $ Set.delete tag props.tags
                                    , children: []
                                    }
                                , R.text tag
                                ]
                            }
                  }
              ]
          }
