module Conduit.Data.Route where

import Conduit.Effects.Routing (class HasRoute)
import Data.Generic.Rep (class Generic)
import Prelude (class Eq, ($))
import Routing.Duplex (RouteDuplex', default, print, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Login
  | Register
  | Settings
  | Editor
  | Profile String
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
        , "Editor": "editor" / noArgs
        , "Profile": "profile" / string segment
        , "Error": "error" / noArgs
        }
