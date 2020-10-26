module Conduit.Capability.Api where

import Prelude
import Conduit.Data.Article (Article, ArticlesQuery, ArticleRep)
import Conduit.Data.Comment (Comment, CommentId)
import Conduit.Data.Error (Error)
import Conduit.Data.Profile (Profile, ProfileRep, User)
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username (Username)
import Data.Either (Either)
import Data.Maybe (Maybe)
import React.Halo (HaloM, lift)

class
  Monad m <= UserApi m where
  loginUser :: { email :: String, password :: String } -> m (Either Error User)
  registerUser :: { username :: Username, email :: String, password :: String } -> m (Either Error User)
  updateUser :: { | ProfileRep ( email :: String, password :: String ) } -> m (Either Error User)

instance userApiHaloM :: UserApi m => UserApi (HaloM props state action m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  updateUser = lift <<< updateUser

class
  Monad m <= ArticleApi m where
  listArticles :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  listFeed :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  getArticle :: Slug -> m (Either Error Article)
  submitArticle :: Maybe Slug -> { | ArticleRep () } -> m (Either Error Article)
  deleteArticle :: Slug -> m (Either Error Unit)

instance articleApiHaloM :: ArticleApi m => ArticleApi (HaloM props state action m) where
  listArticles = lift <<< listArticles
  listFeed = lift <<< listFeed
  getArticle = lift <<< getArticle
  submitArticle = \a b -> lift $ submitArticle a b
  deleteArticle = lift <<< deleteArticle

class
  Monad m <= FavoriteApi m where
  toggleFavorite :: Article -> m (Either Error Article)

instance favoriteApiHaloM :: FavoriteApi m => FavoriteApi (HaloM props state action m) where
  toggleFavorite = lift <<< toggleFavorite

class
  Monad m <= CommentApi m where
  listComments :: Slug -> m (Either Error (Array Comment))
  createComment :: Slug -> { body :: String } -> m (Either Error Comment)
  deleteComment :: Slug -> CommentId -> m (Either Error Unit)

instance commentApiHaloM :: CommentApi m => CommentApi (HaloM props state action m) where
  listComments = lift <<< listComments
  createComment = \a b -> lift $ createComment a b
  deleteComment = \a b -> lift $ deleteComment a b

class
  Monad m <= ProfileApi m where
  getProfile :: Username -> m (Either Error Profile)

instance profileApiHaloM :: ProfileApi m => ProfileApi (HaloM props state action m) where
  getProfile = lift <<< getProfile

class
  Monad m <= FollowApi m where
  toggleFollow :: Profile -> m (Either Error Profile)

instance followApiHaloM :: FollowApi m => FollowApi (HaloM props state action m) where
  toggleFollow = lift <<< toggleFollow

class
  Monad m <= TagApi m where
  listTags :: m (Either Error (Array String))

instance tagApiHaloM :: TagApi m => TagApi (HaloM props state action m) where
  listTags = lift listTags
