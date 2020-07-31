module Conduit.Component.Footer where

import React.Basic.DOM as R
import React.Basic.Hooks as React

footer :: React.JSX
footer =
  R.footer_
    [ R.div
        { className: "container"
        , children:
            [ R.a
                { className: "logo-font"
                , href: "/"
                , children: [ R.text "conduit" ]
                }
            , R.span
                { className: "attribution"
                , children:
                    [ R.text "An interactive learning project from "
                    , R.a
                        { href: "https://thinkster.io"
                        , children: [ R.text "Thinkster" ]
                        }
                    , R.text ". Code & design licensed under MIT. Implemented by "
                    , R.a
                        { href: "https://github.com/jonasbuntinx"
                        , children: [ R.text "Jonas Buntinx" ]
                        }
                    , R.text "."
                    ]
                }
            ]
        }
    ]
