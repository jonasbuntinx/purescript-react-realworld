module Conduit.Data.Route where

import Conduit.Data.Slug (Slug)
import Conduit.Data.Slug as Slug
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe)
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
  | Favorites Username
  | Error

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

toRouteString :: Route -> String
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
        , "Favorites": "profile" / username segment / "favorites"
        , "Error": "error" / noArgs
        }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.fromString >>> note "Bad slug")

username :: RouteDuplex' String -> RouteDuplex' Username
username = as Username.toString (Username.fromString >>> note "Bad username")

-- | Transition
data Transition
  = Loading (Maybe Route) Route
  | Loaded (Maybe Route) Route

derive instance eqTransition :: Eq Transition

_Transition :: Lens' Transition Route
_Transition = lens getter setter
  where
  getter = case _ of
    Loading _ route -> route
    Loaded _ route -> route

  setter = case _ of
    Loading previous _ -> Loading previous
    Loaded previous _ -> Loaded previous
