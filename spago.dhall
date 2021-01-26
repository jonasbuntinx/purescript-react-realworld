{ name = "purescript-react-realword"
, dependencies =
    [ "apiary"
    , "console"
    , "effect"
    , "event"
    , "foreign-generic"
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
    , "wire"
    , "wire-react"
    , "wire-react-router"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
