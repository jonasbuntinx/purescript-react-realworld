module Conduit.Capability.Resource.Article where

import Prelude
import Conduit.Data.Article (Article, ArticlesQuery, ArticleRep)
import Conduit.Data.Error (Error)
import Conduit.Data.Slug (Slug)
import Data.Either (Either)
import Data.Maybe (Maybe)
import React.Halo (HaloM, lift)

type ArticleInstance m
  = { listArticles :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
    , listFeed :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
    , getArticle :: Slug -> m (Either Error Article)
    , submitArticle :: Maybe Slug -> { | ArticleRep () } -> m (Either Error Article)
    , deleteArticle :: Slug -> m (Either Error Unit)
    , toggleFavorite :: Article -> m (Either Error Article)
    }

class
  Monad m <= ArticleRepository m where
  listArticles :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  listFeed :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  getArticle :: Slug -> m (Either Error Article)
  submitArticle :: Maybe Slug -> { | ArticleRep () } -> m (Either Error Article)
  deleteArticle :: Slug -> m (Either Error Unit)
  toggleFavorite :: Article -> m (Either Error Article)

instance articleRepositoryHaloM :: ArticleRepository m => ArticleRepository (HaloM props state action m) where
  listArticles = lift <<< listArticles
  listFeed = lift <<< listFeed
  getArticle = lift <<< getArticle
  submitArticle = \a b -> lift $ submitArticle a b
  deleteArticle = lift <<< deleteArticle
  toggleFavorite = lift <<< toggleFavorite
