module Conduit.AppM where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Conduit.Api.Client (Error, makeRequest, makeSecureRequest)
import Conduit.Api.Endpoint as Endpoint
import Conduit.Capability.Auth (class MonadAuth)
import Conduit.Capability.Auth as Auth
import Conduit.Capability.Halo (class MonadHalo)
import Conduit.Capability.Resource.Article (class ArticleRepository)
import Conduit.Capability.Resource.Comment (class CommentRepository)
import Conduit.Capability.Resource.Profile (class ProfileRepository)
import Conduit.Capability.Resource.Tag (class TagRepository)
import Conduit.Capability.Resource.User (class UserRepository)
import Conduit.Capability.Routing (class MonadRouting)
import Conduit.Capability.Routing as Routing
import Conduit.Component.Auth (AuthIO)
import Conduit.Component.Routing (RoutingIO)
import Conduit.Data.Article (Article, ArticleRep, articleCodec, defaultArticlesQuery, mkArticleRepCodec)
import Conduit.Data.Auth (toAuth)
import Conduit.Data.Comment (Comment, commentCodec)
import Conduit.Data.Profile (Profile, profileCodec)
import Conduit.Data.Route (Route(..))
import Conduit.Data.User (CurrentUser, UserRep, currentUserCodec, mkUserRepCodec)
import Conduit.Data.Username (Username, usernameCodec)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Foldable (for_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exception
import React.Halo as Halo
import Record as Record

type Env =
  { auth :: AuthIO
  , routing :: RoutingIO
  }

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance Functor AppM

derive newtype instance Apply AppM

derive newtype instance Applicative AppM

derive newtype instance Bind AppM

derive newtype instance Monad AppM

derive newtype instance MonadEffect AppM

derive newtype instance MonadAff AppM

derive newtype instance MonadThrow Exception.Error AppM

derive newtype instance MonadError Exception.Error AppM

-- | Halo
instance MonadHalo AppM where
  component name spec =
    AppM do
      env <- ask
      liftEffect
        $ Halo.component name
            spec { eval = Halo.hoist (runAppM env) <<< spec.eval }

-- | Auth
instance MonadAuth AppM where
  read = liftEffect =<< (AppM $ asks _.auth.read)
  getEmitter = AppM $ asks _.auth.emitter
  modify k = do
    f <- AppM $ asks _.auth.modify
    liftEffect $ f k

-- | Routing
instance MonadRouting AppM where
  read = liftEffect =<< (AppM $ asks _.routing.read)
  getEmitter = AppM $ asks _.routing.emitter
  navigate route = do
    f <- AppM $ asks _.routing.navigate
    liftEffect $ f route
  redirect route = do
    f <- AppM $ asks _.routing.redirect
    liftEffect $ f route

-- | User
instance UserRepository AppM where
  loginUser credentials = do
    (res :: Either Error { user :: CurrentUser }) <- makeRequest POST (StatusCode 200) Endpoint.Login loginBodyCodec userResponseCodec { user: credentials }
    for_ res \{ user: currentUser } -> do
      Auth.modify $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
    pure $ res <#> _.user
  registerUser user = do
    (res :: Either Error { user :: CurrentUser }) <- makeRequest POST (StatusCode 200) Endpoint.Users registerBodyCodec userResponseCodec { user }
    for_ res \{ user: currentUser } -> do
      Auth.modify $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
    pure $ res <#> _.user
  updateUser user = do
    (res :: Either Error { user :: CurrentUser }) <- makeSecureRequest PUT (StatusCode 200) Endpoint.User updateUserBodyCodec userResponseCodec { user }
    for_ res \{ user: currentUser } -> do
      Auth.modify $ map $ _ { user = Just $ Record.delete (SProxy :: _ "token") currentUser }
    pure $ res <#> _.user
  logoutUser = do
    void $ Auth.modify $ const Nothing
    Routing.redirect Home

loginBodyCodec :: JsonCodec { user :: { email :: String, password :: String } }
loginBodyCodec = CAR.object "Body" { user: CAR.object "User" { email: CA.string, password: CA.string } }

registerBodyCodec :: JsonCodec { user :: { username :: Username, email :: String, password :: String } }
registerBodyCodec = CAR.object "Body" { user: CAR.object "User" { username: usernameCodec, email: CA.string, password: CA.string } }

updateUserBodyCodec :: JsonCodec { user :: { | UserRep (password :: String) } }
updateUserBodyCodec = CAR.object "Body" { user: CA.object "User" $ mkUserRepCodec $ CAR.record { password: CA.string } }

userResponseCodec :: JsonCodec { user :: CurrentUser }
userResponseCodec = CAR.object "Response" { user: currentUserCodec }

-- | Article
instance ArticleRepository AppM where
  listArticles query = makeRequest GET (StatusCode 200) (Endpoint.Articles query) CA.null articlesResponseCodec unit
  listFeed query = makeSecureRequest GET (StatusCode 200) (Endpoint.Feed query) CA.null articlesResponseCodec unit
  getArticle slug = do
    (res :: Either Error { article :: Article }) <- makeRequest GET (StatusCode 200) (Endpoint.Article slug) CA.null articleResponseCodec unit
    pure $ res <#> _.article
  submitArticle slug article = do
    (res :: Either Error { article :: Article }) <- case slug of
      Nothing -> makeSecureRequest POST (StatusCode 200) (Endpoint.Articles defaultArticlesQuery) articleBodyCodec articleResponseCodec { article }
      Just slug' -> makeSecureRequest PUT (StatusCode 200) (Endpoint.Article slug') articleBodyCodec articleResponseCodec { article }
    pure $ res <#> _.article
  deleteArticle slug = do
    (res :: Either Error {}) <- makeSecureRequest DELETE (StatusCode 200) (Endpoint.Article slug) CA.null (CAR.object "Response" {}) unit
    pure $ res <#> const unit
  toggleFavorite { slug, favorited } = do
    (res :: Either Error { article :: Article }) <-
      if favorited then
        makeSecureRequest DELETE (StatusCode 200) (Endpoint.Favorite slug) CA.null articleResponseCodec unit
      else
        makeSecureRequest POST (StatusCode 200) (Endpoint.Favorite slug) CA.null articleResponseCodec unit
    pure $ res <#> _.article

articleBodyCodec :: JsonCodec { article :: { | ArticleRep () } }
articleBodyCodec = CAR.object "Body" { article: CA.object "Article" $ mkArticleRepCodec CA.record }

articlesResponseCodec :: JsonCodec { articles :: Array Article, articlesCount :: Int }
articlesResponseCodec = CAR.object "Response" { articles: CA.array articleCodec, articlesCount: CA.int }

articleResponseCodec :: JsonCodec { article :: Article }
articleResponseCodec = CAR.object "Response" { article: articleCodec }

-- | Comment
instance CommentRepository AppM where
  listComments slug = do
    (res :: Either Error { comments :: Array Comment }) <- makeRequest GET (StatusCode 200) (Endpoint.Comments slug) CA.null commentsResponseCodec unit
    pure $ res <#> _.comments
  createComment slug comment = do
    (res :: Either Error { comment :: Comment }) <- makeSecureRequest POST (StatusCode 200) (Endpoint.Comments slug) commentBodyCodec commentResponseCodec { comment }
    pure $ res <#> _.comment
  deleteComment slug id = do
    (res :: Either Error {}) <- makeSecureRequest DELETE (StatusCode 200) (Endpoint.Comment slug id) CA.null (CAR.object "Response" {}) unit
    pure $ res <#> const unit

commentBodyCodec :: JsonCodec { comment :: { body :: String } }
commentBodyCodec = CAR.object "Body" { comment: CAR.object "Comment" { body: CA.string } }

commentsResponseCodec :: JsonCodec { comments :: Array Comment }
commentsResponseCodec = CAR.object "Response" { comments: CA.array commentCodec }

commentResponseCodec :: JsonCodec { comment :: Comment }
commentResponseCodec = CAR.object "Response" { comment: commentCodec }

-- | Profile
instance ProfileRepository AppM where
  getProfile username = do
    (res :: Either Error { profile :: Profile }) <- makeRequest GET (StatusCode 200) (Endpoint.Profiles username) CA.null profileResponseCodec unit
    pure $ res <#> _.profile
  toggleFollow { username, following } = do
    (res :: Either Error { profile :: Profile }) <-
      if following then
        makeSecureRequest DELETE (StatusCode 200) (Endpoint.Follow username) CA.null profileResponseCodec unit
      else
        makeSecureRequest POST (StatusCode 200) (Endpoint.Follow username) CA.null profileResponseCodec unit
    pure $ res <#> _.profile

profileResponseCodec :: JsonCodec { profile :: Profile }
profileResponseCodec = CAR.object "Response" { profile: profileCodec }

-- | Tag
instance TagRepository AppM where
  listTags = do
    (res :: Either Error { tags :: Array String }) <- makeRequest GET (StatusCode 200) Endpoint.Tags CA.null tagsResponseCodec unit
    pure $ res <#> _.tags

tagsResponseCodec :: JsonCodec { tags :: Array String }
tagsResponseCodec = CAR.object "Response" { tags: CA.array CA.string }
