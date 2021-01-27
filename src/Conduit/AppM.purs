module Conduit.AppM where

import Prelude
import Conduit.Data.Article (Article, ArticlesQuery, ArticleRep)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Comment (Comment, CommentId)
import Conduit.Data.Error (Error)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route)
import Conduit.Data.Slug (Slug)
import Conduit.Data.User (CurrentUser, User)
import Conduit.Data.Username (Username)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception as Exception
import FRP.Event (Event)
import React.Halo (HaloM, lift)

newtype AppM a
  = AppM (ReaderT (AppImpl AppM) Aff a)

type AppImpl m
  = { auth :: AuthImpl m
    , routing :: RoutingImpl m
    , userApi :: UserApiImpl m
    , articleApi :: ArticleApiImpl m
    , commentApi :: CommentApiImpl m
    , profileApi :: ProfileApiImpl m
    , tagApi :: TagApiImpl m
    }

type AuthImpl m
  = { readAuth :: m (Maybe Auth)
    , readAuthEvent :: m (Event (Maybe Auth))
    , modifyAuth :: (Maybe Auth -> Maybe Auth) -> m (Maybe Auth)
    }

type RoutingImpl m
  = { readRoute :: m Route
    , readRoutingEvent :: m (Event Route)
    , navigate :: Route -> m Unit
    , redirect :: Route -> m Unit
    }

type UserApiImpl m
  = { loginUser :: { email :: String, password :: String } -> m (Either Error CurrentUser)
    , registerUser :: { username :: Username, email :: String, password :: String } -> m (Either Error CurrentUser)
    , updateUser :: { | User ( password :: String ) } -> m (Either Error CurrentUser)
    , logoutUser :: m Unit
    }

type ArticleApiImpl m
  = { listArticles :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
    , listFeed :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
    , getArticle :: Slug -> m (Either Error Article)
    , submitArticle :: Maybe Slug -> { | ArticleRep () } -> m (Either Error Article)
    , deleteArticle :: Slug -> m (Either Error Unit)
    , toggleFavorite :: Article -> m (Either Error Article)
    }

type CommentApiImpl m
  = { listComments :: Slug -> m (Either Error (Array Comment))
    , createComment :: Slug -> { body :: String } -> m (Either Error Comment)
    , deleteComment :: Slug -> CommentId -> m (Either Error Unit)
    }

type ProfileApiImpl m
  = { getProfile :: Username -> m (Either Error Profile)
    , toggleFollow :: Profile -> m (Either Error Profile)
    }

type TagApiImpl m
  = { listTags :: m (Either Error (Array String))
    }

runAppM :: AppImpl AppM -> AppM ~> Aff
runAppM impl (AppM go) = runReaderT go impl

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadThrowAppM :: MonadThrow Exception.Error AppM

derive newtype instance monadErrorAppM :: MonadError Exception.Error AppM

-- | Auth
class
  Monad m <= MonadAuth m where
  readAuth :: m (Maybe Auth)
  readAuthEvent :: m (Event (Maybe Auth))
  modifyAuth :: (Maybe Auth -> Maybe Auth) -> m (Maybe Auth)

instance monadAuthAppM :: MonadAuth AppM where
  readAuth = join $ AppM $ asks _.auth.readAuth
  readAuthEvent = join $ AppM $ asks _.auth.readAuthEvent
  modifyAuth f = (AppM $ asks _.auth.modifyAuth) >>= (#) f

instance authHaloM :: MonadAuth m => MonadAuth (HaloM props state action m) where
  readAuth = lift readAuth
  readAuthEvent = lift readAuthEvent
  modifyAuth = lift <<< modifyAuth

-- | Routing
class
  Monad m <= MonadRouting m where
  readRoute :: m Route
  readRoutingEvent :: m (Event Route)
  navigate :: Route -> m Unit
  redirect :: Route -> m Unit

instance monadRoutingAppM :: MonadRouting AppM where
  readRoute = join $ AppM $ asks _.routing.readRoute
  readRoutingEvent = join $ AppM $ asks _.routing.readRoutingEvent
  navigate route = (AppM $ asks _.routing.navigate) >>= (#) route
  redirect route = (AppM $ asks _.routing.redirect) >>= (#) route

instance routingHaloM :: MonadRouting m => MonadRouting (HaloM props state action m) where
  readRoute = lift readRoute
  readRoutingEvent = lift readRoutingEvent
  navigate = lift <<< navigate
  redirect = lift <<< redirect

-- | User
class
  Monad m <= MonadUserApi m where
  loginUser :: { email :: String, password :: String } -> m (Either Error CurrentUser)
  registerUser :: { username :: Username, email :: String, password :: String } -> m (Either Error CurrentUser)
  updateUser :: { | User ( password :: String ) } -> m (Either Error CurrentUser)
  logoutUser :: m Unit

instance monadUserApiM :: MonadUserApi AppM where
  loginUser creds = (AppM $ asks _.userApi.loginUser) >>= (#) creds
  registerUser user = (AppM $ asks _.userApi.registerUser) >>= (#) user
  updateUser user = (AppM $ asks _.userApi.updateUser) >>= (#) user
  logoutUser = join $ AppM $ asks _.userApi.logoutUser

instance userApiHaloM :: MonadUserApi m => MonadUserApi (HaloM props state action m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  updateUser = lift <<< updateUser
  logoutUser = lift logoutUser

-- | Article
class
  Monad m <= MonadArticleApi m where
  listArticles :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  listFeed :: ArticlesQuery -> m (Either Error { articles :: Array Article, articlesCount :: Int })
  getArticle :: Slug -> m (Either Error Article)
  submitArticle :: Maybe Slug -> { | ArticleRep () } -> m (Either Error Article)
  deleteArticle :: Slug -> m (Either Error Unit)
  toggleFavorite :: Article -> m (Either Error Article)

instance monadArticleApiM :: MonadArticleApi AppM where
  listArticles query = (AppM $ asks _.articleApi.listArticles) >>= (#) query
  listFeed query = (AppM $ asks _.articleApi.listFeed) >>= (#) query
  getArticle slug = (AppM $ asks _.articleApi.getArticle) >>= (#) slug
  submitArticle slug article = (AppM $ asks _.articleApi.submitArticle) >>= \f -> f slug article
  deleteArticle slug = (AppM $ asks _.articleApi.deleteArticle) >>= (#) slug
  toggleFavorite article = (AppM $ asks _.articleApi.toggleFavorite) >>= (#) article

instance articleApiHaloM :: MonadArticleApi m => MonadArticleApi (HaloM props state action m) where
  listArticles = lift <<< listArticles
  listFeed = lift <<< listFeed
  getArticle = lift <<< getArticle
  submitArticle = \a b -> lift $ submitArticle a b
  deleteArticle = lift <<< deleteArticle
  toggleFavorite = lift <<< toggleFavorite

-- | Comment
class
  Monad m <= MonadCommentApi m where
  listComments :: Slug -> m (Either Error (Array Comment))
  createComment :: Slug -> { body :: String } -> m (Either Error Comment)
  deleteComment :: Slug -> CommentId -> m (Either Error Unit)

instance monadCommentApiM :: MonadCommentApi AppM where
  listComments slug = (AppM $ asks _.commentApi.listComments) >>= (#) slug
  createComment slug comment = (AppM $ asks _.commentApi.createComment) >>= \f -> f slug comment
  deleteComment slug id = (AppM $ asks _.commentApi.deleteComment) >>= \f -> f slug id

instance commentApiHaloM :: MonadCommentApi m => MonadCommentApi (HaloM props state action m) where
  listComments = lift <<< listComments
  createComment = \a b -> lift $ createComment a b
  deleteComment = \a b -> lift $ deleteComment a b

-- | Profile
class
  Monad m <= MonadProfileApi m where
  getProfile :: Username -> m (Either Error Profile)
  toggleFollow :: Profile -> m (Either Error Profile)

instance monadProfileApiM :: MonadProfileApi AppM where
  getProfile username = (AppM $ asks _.profileApi.getProfile) >>= (#) username
  toggleFollow profile = (AppM $ asks _.profileApi.toggleFollow) >>= (#) profile

instance profileApiHaloM :: MonadProfileApi m => MonadProfileApi (HaloM props state action m) where
  getProfile = lift <<< getProfile
  toggleFollow = lift <<< toggleFollow

-- | Tag
class
  Monad m <= MonadTagApi m where
  listTags :: m (Either Error (Array String))

instance monadTagApiM :: MonadTagApi AppM where
  listTags = join $ AppM $ asks _.tagApi.listTags

instance tagApiHaloM :: MonadTagApi m => MonadTagApi (HaloM props state action m) where
  listTags = lift listTags
