module Conduit.Capability.Resource.Comment where

import Prelude
import Conduit.Api.Client (Error)
import Conduit.Data.Comment (Comment, CommentId)
import Conduit.Data.Slug (Slug)
import Data.Either (Either)
import React.Halo (HaloM, lift)

type CommentInstance m
  = { listComments :: Slug -> m (Either Error (Array Comment))
    , createComment :: Slug -> { body :: String } -> m (Either Error Comment)
    , deleteComment :: Slug -> CommentId -> m (Either Error Unit)
    }

class
  Monad m <= CommentRepository m where
  listComments :: Slug -> m (Either Error (Array Comment))
  createComment :: Slug -> { body :: String } -> m (Either Error Comment)
  deleteComment :: Slug -> CommentId -> m (Either Error Unit)

instance commentRepositoryHaloM :: CommentRepository m => CommentRepository (HaloM props state action m) where
  listComments = lift <<< listComments
  createComment = \a b -> lift $ createComment a b
  deleteComment = \a b -> lift $ deleteComment a b
