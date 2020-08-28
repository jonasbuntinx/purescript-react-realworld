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

class MonadUserApi m where
  loginUser :: { email :: String, password :: String } -> m (Either Error User)
  registerUser :: { username :: Username, email :: String, password :: String } -> m (Either Error User)
  updateUser :: { | ProfileRep ( email :: String, password :: String ) } -> m (Either Error User)

class MonadArticleApi m where
  listArticles :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  listFeed :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  getArticle :: Slug -> m (Either Error Article)
  submitArticle :: Maybe Slug -> { | ArticleRep () } -> m (Either Error Article)
  deleteArticle :: Slug -> m (Either Error Unit)

class MonadFavoriteApi m where
  toggleFavorite :: Article -> m (Either Error Article)

class MonadCommentApi m where
  listComments :: Slug -> m (Either Error (Array Comment))
  createComment :: Slug -> { body :: String } -> m (Either Error Comment)
  deleteComment :: Slug -> CommentId -> m (Either Error Unit)

class MonadProfileApi m where
  getProfile :: Username -> m (Either Error Profile)

class MonadFollowApi m where
  toggleFollow :: Profile -> m (Either Error Profile)

class MonadTagApi m where
  listTags :: m (Either Error (Array String))
