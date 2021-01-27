module Conduit.Capability.Resource.Comment where

import Prelude
import Conduit.Data.Comment (Comment, CommentId)
import Conduit.Data.Error (Error)
import Conduit.Data.Slug (Slug)
import Data.Either (Either)
import React.Halo (HaloM, lift)

type CommentInst m
  = { listComments :: Slug -> m (Either Error (Array Comment))
    , createComment :: Slug -> { body :: String } -> m (Either Error Comment)
    , deleteComment :: Slug -> CommentId -> m (Either Error Unit)
    }

class
  Monad m <= MonadComment m where
  listComments :: Slug -> m (Either Error (Array Comment))
  createComment :: Slug -> { body :: String } -> m (Either Error Comment)
  deleteComment :: Slug -> CommentId -> m (Either Error Unit)

instance monadCommentHaloM :: MonadComment m => MonadComment (HaloM props state action m) where
  listComments = lift <<< listComments
  createComment = \a b -> lift $ createComment a b
  deleteComment = \a b -> lift $ deleteComment a b
