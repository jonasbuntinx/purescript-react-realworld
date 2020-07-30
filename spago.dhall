{ name = "purescript-react-realword"
, dependencies =
    [ "apiary"
    , "console"
    , "effect"
    , "foreign-generic"
    , "heterogeneous"
    , "js-timers"
    , "profunctor-lenses"
    , "react-basic"
    , "react-basic-hooks"
    , "react-basic-hooks-store"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "web-uievents"
    , "wire"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
