module Conduit.Api.Endpoint where

import Prelude hiding ((/))
import Conduit.Data.Article (ArticlesQuery)
import Conduit.Data.Comment (CommentId)
import Conduit.Data.Slug (Slug)
import Conduit.Data.Slug as Slug
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', as, int, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

data Endpoint
  = Login
  | User
  | Users
  | Follow Username
  | Article Slug
  | Comment Slug CommentId
  | Comments Slug
  | Favorite Slug
  | Articles ArticlesQuery
  | Profiles Username
  | Feed ArticlesQuery
  | Tags

derive instance Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root $ prefix "api"
    $ sum
        { "Login": "users" / "login" / noArgs
        , "User": "user" / noArgs
        , "Users": "users" / noArgs
        , "Follow": "profiles" / username segment / "follow"
        , "Article": "articles" / slug segment
        , "Comments": "articles" / slug segment / "comments"
        , "Comment": "articles" / slug segment / "comments" / commentId
        , "Favorite": "articles" / slug segment / "favorite"
        , "Articles":
            "articles"
              ?
                { tag: optional <<< string
                , author: optional <<< username
                , favorited: optional <<< username
                , offset: optional <<< int
                , limit: optional <<< int
                }
        , "Profiles": "profiles" / username segment
        , "Feed":
            "articles" / "feed"
              ?
                { tag: optional <<< string
                , author: optional <<< username
                , favorited: optional <<< username
                , offset: optional <<< int
                , limit: optional <<< int
                }
        , "Tags": "tags" / noArgs
        }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.fromString >>> note "Bad slug")

username :: RouteDuplex' String -> RouteDuplex' Username
username = as Username.toString (Username.fromString >>> note "Bad username")

commentId :: RouteDuplex' CommentId
commentId = _Newtype (int segment)
