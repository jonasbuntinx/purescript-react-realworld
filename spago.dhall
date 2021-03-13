{ name = "purescript-react-realword"
, dependencies =
    [ "affjax"
    , "argonaut-codecs"
    , "argonaut-core"
    , "console"
    , "effect"
    , "halogen-subscriptions"
    , "heterogeneous"
    , "js-timers"
    , "profunctor-lenses"
    , "react-basic-dom"
    , "react-basic-hooks"
    , "react-halo"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "unicode"
    , "web-uievents"
    , "wire-react-router"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
