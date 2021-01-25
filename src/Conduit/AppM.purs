module Conduit.AppM where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (authenticate, makeRequest, makeSecureRequest)
import Conduit.Capability.Api (class ArticleApi, class CommentApi, class ProfileApi, class TagApi, class UserApi)
import Conduit.Capability.Routing (class Routing)
import Conduit.Data.Env (Env)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (expand, match)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Record as Record
import Type.Equality (class TypeEquals, from)
import Wire.React.Atom.Class (modify)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

-- | Routing
instance routingAppM :: Routing AppM where
  navigate route = ask >>= \{ router } -> liftEffect $ router.navigate route
  redirect route = ask >>= \{ router } -> liftEffect $ router.redirect route
  logout =
    ask
      >>= \{ auth, router } ->
          liftEffect do
            modify auth.signal $ const Nothing
            router.redirect Home

-- | User
instance userApiAppM :: UserApi AppM where
  loginUser credentials = do
    authenticate (Apiary.Route :: Endpoints.LoginUser) Apiary.none Apiary.none { user: credentials }
  registerUser user = do
    authenticate (Apiary.Route :: Endpoints.RegisterUser) Apiary.none Apiary.none { user }
  updateUser user = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.UpdateUser) Apiary.none Apiary.none { user }
    res
      # either
          (pure <<< Left)
          ( match
              { ok:
                  \{ user: currentUser } -> do
                    ask >>= \{ auth } -> liftEffect $ modify auth.signal $ map $ _ { user = Just $ Record.delete (SProxy :: _ "token") currentUser }
                    pure $ Right currentUser
              , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
              }
          )

-- | Article
instance articleApiAppM :: ArticleApi AppM where
  listArticles query = do
    res <- makeRequest (Apiary.Route :: Endpoints.ListArticles) Apiary.none query Apiary.none
    pure $ res >>= match { ok: Right }
  listFeed query = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.ListFeed) Apiary.none query Apiary.none
    pure $ res >>= match { ok: Right }
  getArticle slug = do
    res <- makeRequest (Apiary.Route :: Endpoints.GetArticle) { slug } Apiary.none Apiary.none
    pure $ res >>= (match { ok: Right <<< _.article, notFound: Left <<< NotFound })
  submitArticle slug article = do
    res <- case slug of
      Nothing -> map expand <$> makeSecureRequest (Apiary.Route :: Endpoints.CreateArticle) Apiary.none Apiary.none { article }
      Just slug' -> map expand <$> makeSecureRequest (Apiary.Route :: Endpoints.UpdateArticle) { slug: slug' } Apiary.none { article }
    pure $ res >>= (match { ok: Right <<< _.article, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })
  deleteArticle slug = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.DeleteArticle) { slug } Apiary.none Apiary.none
    pure $ res >>= (match { ok: const $ Right unit })
  toggleFavorite { slug, favorited } = do
    res <-
      if favorited then
        makeSecureRequest (Apiary.Route :: Endpoints.UnfavoriteArticle) { slug } Apiary.none Apiary.none
      else
        makeSecureRequest (Apiary.Route :: Endpoints.FavoriteArticle) { slug } Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.article }

-- | Comment
instance commentApiAppM :: CommentApi AppM where
  listComments slug = do
    res <- makeRequest (Apiary.Route :: Endpoints.ListComments) { slug } Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.comments }
  createComment slug comment = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.CreateComment) { slug } Apiary.none { comment }
    pure $ res >>= (match { ok: Right <<< _.comment })
  deleteComment slug id = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.DeleteComment) { slug, id } Apiary.none Apiary.none
    pure $ res >>= (match { ok: const $ Right unit })

-- | Profile
instance profileApiAppM :: ProfileApi AppM where
  getProfile username = do
    res <- makeRequest (Apiary.Route :: Endpoints.GetProfile) { username } Apiary.none Apiary.none
    pure $ res >>= (match { ok: Right <<< _.profile, notFound: Left <<< NotFound })
  toggleFollow { username, following } = do
    res <-
      if following then
        makeSecureRequest (Apiary.Route :: Endpoints.UnfollowProfile) { username } Apiary.none Apiary.none
      else
        makeSecureRequest (Apiary.Route :: Endpoints.FollowProfile) { username } Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.profile }

-- | Tag
instance tagApiAppM :: TagApi AppM where
  listTags = do
    res <- makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.tags }
