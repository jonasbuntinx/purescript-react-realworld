module Conduit.AppM where

import Prelude
import Conduit.Capability.Auth (class MonadAuth, AuthInst)
import Conduit.Capability.Resource.Article (class MonadArticle, ArticleInst)
import Conduit.Capability.Resource.Comment (class MonadComment, CommentInst)
import Conduit.Capability.Resource.Profile (class MonadProfile, ProfileInst)
import Conduit.Capability.Resource.Tag (class MonadTag, TagInst)
import Conduit.Capability.Resource.User (class MonadUser, UserInst)
import Conduit.Capability.Routing (class MonadRouting, RoutingInst)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception as Exception

type AppInst m
  = { auth :: AuthInst m
    , routing :: RoutingInst m
    , user :: UserInst m
    , article :: ArticleInst m
    , comment :: CommentInst m
    , profile :: ProfileInst m
    , tag :: TagInst m
    }

newtype AppM a
  = AppM (ReaderT (AppInst AppM) Aff a)

runAppM :: AppInst AppM -> AppM ~> Aff
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

-- | User
instance monadUserAppM :: MonadUser AppM where
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
instance monadArticleAppM :: MonadArticle AppM where
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
instance monadCommentAppM :: MonadComment AppM where
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
instance monadProfileAppM :: MonadProfile AppM where
  getProfile username = do
    f <- AppM $ asks _.profile.getProfile
    f username
  toggleFollow profile = do
    f <- AppM $ asks _.profile.toggleFollow
    f profile

-- | Tag
instance monadTagAppM :: MonadTag AppM where
  listTags = join $ AppM $ asks _.tag.listTags
