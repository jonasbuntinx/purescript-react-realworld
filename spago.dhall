{ name = "purescript-react-realword"
, dependencies =
    [ "apiary"
    , "console"
    , "effect"
    , "foreign-generic"
    , "heterogeneous"
    , "js-timers"
    , "profunctor-lenses"
    , "react-basic-dom"
    , "react-basic-hooks"
    , "react-basic-hooks-store"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "unicode"
    , "web-uievents"
    , "wire"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
