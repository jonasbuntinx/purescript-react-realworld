module Conduit.AppM where

import Prelude
import Conduit.Capability.Auth (class MonadAuth, AuthImpl)
import Conduit.Capability.Resource.Article (class MonadArticle, ArticleImpl)
import Conduit.Capability.Resource.Comment (class MonadComment, CommentImpl)
import Conduit.Capability.Resource.Profile (class MonadProfile, ProfileImpl)
import Conduit.Capability.Resource.Tag (class MonadTag, TagImpl)
import Conduit.Capability.Resource.User (class MonadUser, UserImpl)
import Conduit.Capability.Routing (class MonadRouting, RoutingImpl)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception as Exception

type AppImpl m
  = { auth :: AuthImpl m
    , routing :: RoutingImpl m
    , userApi :: UserImpl m
    , articleApi :: ArticleImpl m
    , commentApi :: CommentImpl m
    , profileApi :: ProfileImpl m
    , tagApi :: TagImpl m
    }

newtype AppM a
  = AppM (ReaderT (AppImpl AppM) Aff a)

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

-- | User
instance monadUserAppM :: MonadUser AppM where
  loginUser creds = do
    f <- AppM $ asks _.userApi.loginUser
    f creds
  registerUser user = do
    f <- AppM $ asks _.userApi.registerUser
    f user
  updateUser user = do
    f <- AppM $ asks _.userApi.updateUser
    f user
  logoutUser = join $ AppM $ asks _.userApi.logoutUser

-- | Article
instance monadArticleAppM :: MonadArticle AppM where
  listArticles query = do
    f <- AppM $ asks _.articleApi.listArticles
    f query
  listFeed query = do
    f <- AppM $ asks _.articleApi.listFeed
    f query
  getArticle slug = do
    f <- AppM $ asks _.articleApi.getArticle
    f slug
  submitArticle slug article = do
    f <- AppM $ asks _.articleApi.submitArticle
    f slug article
  deleteArticle slug = do
    f <- AppM $ asks _.articleApi.deleteArticle
    f slug
  toggleFavorite article = do
    f <- AppM $ asks _.articleApi.toggleFavorite
    f article

-- | Comment
instance monadCommentAppM :: MonadComment AppM where
  listComments slug = do
    f <- AppM $ asks _.commentApi.listComments
    f slug
  createComment slug comment = do
    f <- AppM $ asks _.commentApi.createComment
    f slug comment
  deleteComment slug id = do
    f <- AppM $ asks _.commentApi.deleteComment
    f slug id

-- | Profile
instance monadProfileAppM :: MonadProfile AppM where
  getProfile username = do
    f <- AppM $ asks _.profileApi.getProfile
    f username
  toggleFollow profile = do
    f <- AppM $ asks _.profileApi.toggleFollow
    f profile

-- | Tag
instance monadTagAppM :: MonadTag AppM where
  listTags = join $ AppM $ asks _.tagApi.listTags
