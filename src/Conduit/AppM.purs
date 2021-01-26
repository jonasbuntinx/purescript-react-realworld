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
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
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
  = { read :: m (Maybe Auth)
    , subscribe :: (Maybe Auth -> Effect Unit) -> m (m Unit)
    }

type RoutingImpl m
  = { read :: m Route
    , navigate :: Route -> m Unit
    , redirect :: Route -> m Unit
    , logout :: m Unit
    }

type UserApiImpl m
  = { loginUser :: { email :: String, password :: String } -> m (Either Error CurrentUser)
    , registerUser :: { username :: Username, email :: String, password :: String } -> m (Either Error CurrentUser)
    , updateUser :: { | User ( password :: String ) } -> m (Either Error CurrentUser)
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

-- | Auth
class
  Monad m <= MonadAuth m where
  readAuth :: m (Maybe Auth)
  subscribeToAuth :: (Maybe Auth -> Effect Unit) -> m (m Unit)

instance monadAuthAppM :: MonadAuth AppM where
  readAuth = do
    f <- AppM (asks _.auth.read)
    f
  subscribeToAuth k = do
    f <- AppM (asks _.auth.subscribe)
    f k

instance authHaloM :: MonadAuth m => MonadAuth (HaloM props state action m) where
  readAuth = lift readAuth
  subscribeToAuth = map lift <<< lift <<< subscribeToAuth

-- | Routing
class
  Monad m <= MonadRouting m where
  readRoute :: m Route
  navigate :: Route -> m Unit
  redirect :: Route -> m Unit
  logout :: m Unit

instance monadRoutingAppM :: MonadRouting AppM where
  readRoute = do
    f <- AppM (asks _.routing.read)
    f
  navigate route = do
    f <- AppM (asks _.routing.navigate)
    f route
  redirect route = do
    f <- AppM (asks _.routing.redirect)
    f route
  logout = do
    f <- AppM (asks _.routing.logout)
    f

instance routingHaloM :: MonadRouting m => MonadRouting (HaloM props state action m) where
  readRoute = lift readRoute
  navigate = lift <<< navigate
  redirect = lift <<< redirect
  logout = lift logout

-- | User
class
  Monad m <= MonadUserApi m where
  loginUser :: { email :: String, password :: String } -> m (Either Error CurrentUser)
  registerUser :: { username :: Username, email :: String, password :: String } -> m (Either Error CurrentUser)
  updateUser :: { | User ( password :: String ) } -> m (Either Error CurrentUser)

instance monadUserApiM :: MonadUserApi AppM where
  loginUser creds = do
    f <- AppM (asks _.userApi.loginUser)
    f creds
  registerUser user = do
    f <- AppM (asks _.userApi.registerUser)
    f user
  updateUser user = do
    f <- AppM (asks _.userApi.updateUser)
    f user

instance userApiHaloM :: MonadUserApi m => MonadUserApi (HaloM props state action m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  updateUser = lift <<< updateUser

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
  listArticles query = do
    f <- AppM (asks _.articleApi.listArticles)
    f query
  listFeed query = do
    f <- AppM (asks _.articleApi.listFeed)
    f query
  getArticle slug = do
    f <- AppM (asks _.articleApi.getArticle)
    f slug
  submitArticle slug article = do
    f <- AppM (asks _.articleApi.submitArticle)
    f slug article
  deleteArticle slug = do
    f <- AppM (asks _.articleApi.deleteArticle)
    f slug
  toggleFavorite article = do
    f <- AppM (asks _.articleApi.toggleFavorite)
    f article

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
  listComments slug = do
    f <- AppM (asks _.commentApi.listComments)
    f slug
  createComment slug comment = do
    f <- AppM (asks _.commentApi.createComment)
    f slug comment
  deleteComment slug id = do
    f <- AppM (asks _.commentApi.deleteComment)
    f slug id

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
  getProfile username = do
    f <- AppM (asks _.profileApi.getProfile)
    f username
  toggleFollow profile = do
    f <- AppM (asks _.profileApi.toggleFollow)
    f profile

instance profileApiHaloM :: MonadProfileApi m => MonadProfileApi (HaloM props state action m) where
  getProfile = lift <<< getProfile
  toggleFollow = lift <<< toggleFollow

-- | Tag
class
  Monad m <= MonadTagApi m where
  listTags :: m (Either Error (Array String))

instance monadTagApiM :: MonadTagApi AppM where
  listTags = do
    f <- AppM (asks _.tagApi.listTags)
    f

instance tagApiHaloM :: MonadTagApi m => MonadTagApi (HaloM props state action m) where
  listTags = lift listTags
