module Conduit.AppM where

import Prelude
import Conduit.Capability.Auth (class MonadAuth, AuthImpl)
import Conduit.Capability.Resource.Article (class MonadArticle, ArticleApiImpl)
import Conduit.Capability.Resource.Comment (class MonadComment, CommentApiImpl)
import Conduit.Capability.Resource.Profile (class MonadProfile, ProfileApiImpl)
import Conduit.Capability.Resource.Tag (class MonadTag, TagApiImpl)
import Conduit.Capability.Resource.User (class MonadUser, UserApiImpl)
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
    , userApi :: UserApiImpl m
    , articleApi :: ArticleApiImpl m
    , commentApi :: CommentApiImpl m
    , profileApi :: ProfileApiImpl m
    , tagApi :: TagApiImpl m
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
  modifyAuth f = (AppM $ asks _.auth.modifyAuth) >>= (#) f

-- | Routing
instance monadRoutingAppM :: MonadRouting AppM where
  readRoute = join $ AppM $ asks _.routing.readRoute
  readRoutingEvent = join $ AppM $ asks _.routing.readRoutingEvent
  navigate route = (AppM $ asks _.routing.navigate) >>= (#) route
  redirect route = (AppM $ asks _.routing.redirect) >>= (#) route

-- | User
instance monadUserAppM :: MonadUser AppM where
  loginUser creds = (AppM $ asks _.userApi.loginUser) >>= (#) creds
  registerUser user = (AppM $ asks _.userApi.registerUser) >>= (#) user
  updateUser user = (AppM $ asks _.userApi.updateUser) >>= (#) user
  logoutUser = join $ AppM $ asks _.userApi.logoutUser

-- | Article
instance monadArticleAppM :: MonadArticle AppM where
  listArticles query = (AppM $ asks _.articleApi.listArticles) >>= (#) query
  listFeed query = (AppM $ asks _.articleApi.listFeed) >>= (#) query
  getArticle slug = (AppM $ asks _.articleApi.getArticle) >>= (#) slug
  submitArticle slug article = (AppM $ asks _.articleApi.submitArticle) >>= \f -> f slug article
  deleteArticle slug = (AppM $ asks _.articleApi.deleteArticle) >>= (#) slug
  toggleFavorite article = (AppM $ asks _.articleApi.toggleFavorite) >>= (#) article

-- | Comment
instance monadCommentAppM :: MonadComment AppM where
  listComments slug = (AppM $ asks _.commentApi.listComments) >>= (#) slug
  createComment slug comment = (AppM $ asks _.commentApi.createComment) >>= \f -> f slug comment
  deleteComment slug id = (AppM $ asks _.commentApi.deleteComment) >>= \f -> f slug id

-- | Profile
instance monadProfileAppM :: MonadProfile AppM where
  getProfile username = (AppM $ asks _.profileApi.getProfile) >>= (#) username
  toggleFollow profile = (AppM $ asks _.profileApi.toggleFollow) >>= (#) profile

-- | Tag
instance monadTagAppM :: MonadTag AppM where
  listTags = join $ AppM $ asks _.tagApi.listTags
