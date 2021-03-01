module Conduit.AppM where

import Prelude
import Conduit.Capability.Auth (class MonadAuth, AuthInstance)
import Conduit.Capability.Resource.Article (class ArticleRepository, ArticleInstance)
import Conduit.Capability.Resource.Comment (class CommentRepository, CommentInstance)
import Conduit.Capability.Resource.Profile (class ProfileRepository, ProfileInstance)
import Conduit.Capability.Resource.Tag (class TagRepository, TagInstance)
import Conduit.Capability.Resource.User (class UserRepository, UserInstance)
import Conduit.Capability.Routing (class MonadRouting, RoutingInstance)
import Conduit.Capability.Serverless (class MonadServerless, ServerlessInstance, runStateBuilder)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception as Exception

type AppInstance m
  = { auth :: AuthInstance m
    , routing :: RoutingInstance m
    , serverless :: ServerlessInstance m
    , user :: UserInstance m
    , article :: ArticleInstance m
    , comment :: CommentInstance m
    , profile :: ProfileInstance m
    , tag :: TagInstance m
    }

newtype AppM a
  = AppM (ReaderT (AppInstance AppM) Aff a)

runAppM :: AppInstance AppM -> AppM ~> Aff
runAppM inst (AppM go) = runReaderT go inst

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
instance monadAuthAppM :: MonadAuth AppM where
  readAuth = join $ AppM $ asks _.auth.readAuth
  readAuthEvent = join $ AppM $ asks _.auth.readAuthEvent
  modifyAuth k = do
    f <- AppM $ asks _.auth.modifyAuth
    f k

-- | Routing
instance monadRoutingAppM :: MonadRouting AppM where
  readRoute = join $ AppM $ asks _.routing.readRoute
  readRoutingEvent = join $ AppM $ asks _.routing.readRoutingEvent
  navigate route = do
    f <- AppM $ asks _.routing.navigate
    f route
  redirect route = do
    f <- AppM $ asks _.routing.redirect
    f route

-- | Serverless
instance serverlessAppM :: MonadServerless AppM where
  buildInitialState ms fms = do
    sb <- AppM $ asks _.serverless.getStateBuilder
    runStateBuilder sb ms fms

-- | User
instance userRepositoryAppM :: UserRepository AppM where
  loginUser creds = do
    f <- AppM $ asks _.user.loginUser
    f creds
  registerUser user = do
    f <- AppM $ asks _.user.registerUser
    f user
  updateUser user = do
    f <- AppM $ asks _.user.updateUser
    f user
  logoutUser = join $ AppM $ asks _.user.logoutUser

-- | Article
instance articleRepositoryAppM :: ArticleRepository AppM where
  listArticles query = do
    f <- AppM $ asks _.article.listArticles
    f query
  listFeed query = do
    f <- AppM $ asks _.article.listFeed
    f query
  getArticle slug = do
    f <- AppM $ asks _.article.getArticle
    f slug
  submitArticle slug article = do
    f <- AppM $ asks _.article.submitArticle
    f slug article
  deleteArticle slug = do
    f <- AppM $ asks _.article.deleteArticle
    f slug
  toggleFavorite article = do
    f <- AppM $ asks _.article.toggleFavorite
    f article

-- | Comment
instance commentRepositoryAppM :: CommentRepository AppM where
  listComments slug = do
    f <- AppM $ asks _.comment.listComments
    f slug
  createComment slug comment = do
    f <- AppM $ asks _.comment.createComment
    f slug comment
  deleteComment slug id = do
    f <- AppM $ asks _.comment.deleteComment
    f slug id

-- | Profile
instance profileRepositoryAppM :: ProfileRepository AppM where
  getProfile username = do
    f <- AppM $ asks _.profile.getProfile
    f username
  toggleFollow profile = do
    f <- AppM $ asks _.profile.toggleFollow
    f profile

-- | Tag
instance tagRepositoryAppM :: TagRepository AppM where
  listTags = join $ AppM $ asks _.tag.listTags
