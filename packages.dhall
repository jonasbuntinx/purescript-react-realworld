{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201021/packages.dhall sha256:55ebdbda1bd6ede4d5307fbc1ef19988c80271b4225d833c8d6fb9b6fb1aa6d8

let overrides =
      { simple-json =
              upstream.simple-json
          //  { repo = "https://github.com/robertdp/purescript-simple-json.git"
              , version = "v7.0.1"
              }
      }

let additions =
      { apiary =
        { dependencies = [ "affjax", "media-types", "simple-json" ]
        , repo = "https://github.com/robertdp/purescript-apiary"
        , version = "v0.2.0"
        }
      , react-halo =
        { dependencies =
          [ "aff", "event", "free", "freeap", "react-basic-hooks", "refs" ]
        , repo = "https://github.com/robertdp/purescript-react-halo"
        , version = "v1.2.0"
        }
      , wire =
        { dependencies = [ "aff", "filterable", "refs", "unsafe-reference" ]
        , repo = "https://github.com/robertdp/purescript-wire"
        , version = "v0.4.2"
        }
      , wire-react =
        { dependencies = [ "wire", "free", "freet", "react-basic-hooks" ]
        , repo = "https://github.com/robertdp/purescript-wire-react"
        , version = "v0.0.1"
        }
      , wire-react-router =
        { dependencies = [ "aff", "indexed-monad", "freet", "profunctor-lenses", "react-basic-hooks", "routing", "wire" ]
        , repo = "https://github.com/robertdp/purescript-wire-react-router"
        , version = "v0.2.1"
        }
      }

in  upstream // overrides // additions
