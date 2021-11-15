module Conduit.Data.Route (Route(..), routeCodec) where

import Prelude hiding ((/))
import Conduit.Data.Slug (Slug)
import Conduit.Data.Slug as Slug
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, default, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Login
  | Register
  | Settings
  | CreateArticle
  | UpdateArticle Slug
  | ViewArticle Slug
  | Profile Username
  | Favorites Username
  | Error

derive instance Generic Route _

derive instance Eq Route

routeCodec :: RouteDuplex' Route
routeCodec =
  default Error
    $ root
    $ sum
        { "Home": noArgs
        , "Login": "login" / noArgs
        , "Register": "register" / noArgs
        , "Settings": "settings" / noArgs
        , "CreateArticle": "editor" / noArgs
        , "UpdateArticle": "editor" / slug segment
        , "ViewArticle": "article" / slug segment
        , "Profile": "profile" / username segment
        , "Favorites": "profile" / username segment / "favorites"
        , "Error": "error" / noArgs
        }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.fromString >>> note "Bad slug")

username :: RouteDuplex' String -> RouteDuplex' Username
username = as Username.toString (Username.fromString >>> note "Bad username")
