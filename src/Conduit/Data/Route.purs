module Conduit.Data.Route where

import Conduit.Data.Slug (Slug)
import Conduit.Data.Slug as Slug
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Effects.Routing (class HasRoute)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Prelude (class Eq, ($), (>>>))
import Routing.Duplex (RouteDuplex', as, default, print, root, segment)
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
  | Error

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

instance hasRouteRoute :: HasRoute Route where
  toRouteString x = print routeCodec x

routeCodec :: RouteDuplex' Route
routeCodec =
  default (Error)
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
        , "Error": "error" / noArgs
        }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")

username :: RouteDuplex' String -> RouteDuplex' Username
username = as Username.toString (Username.parse >>> note "Bad username")
