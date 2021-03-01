module Main where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (makeRequest, makeSecureRequest)
import Conduit.AppM (AppM, AppInstance, runAppM)
import Conduit.Capability.Auth (modifyAuth)
import Conduit.Capability.Resource.Article (ArticleInstance)
import Conduit.Capability.Resource.Comment (CommentInstance)
import Conduit.Capability.Resource.Profile (ProfileInstance)
import Conduit.Capability.Resource.Tag (TagInstance)
import Conduit.Capability.Resource.User (UserInstance)
import Conduit.Capability.Routing (redirect)
import Conduit.Capability.Serverless (mkStateBuilder)
import Conduit.Component.Auth (mkAuthManager)
import Conduit.Component.Routing (mkRoutingManager)
import Conduit.Context.Hydrate (mkHydrateProvider)
import Conduit.Data.Auth (Auth, toAuth)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Conduit.Root as Root
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (expand, match)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import FRP.Event as Event
import React.Basic as React
import React.Basic.DOM (hydrate)
import Record as Record
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  container <- getElementById "conduit" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> Exception.throw "Conduit container element not found."
    Just c -> do
      auth <- mkAuthManager
      routing <- mkRoutingManager
      launchAff_
        $ runAppM (appInstance auth routing) do
            context /\ hydrateProvider <- mkHydrateProvider
            root <- Root.mkComponent context
            liftEffect $ hydrate (React.fragment [ routing.component, auth.component, hydrateProvider $ root unit ]) c

appInstance ::
  forall r s.
  { read :: Effect (Maybe Auth)
  , event :: Event.Event (Maybe Auth)
  , modify :: (Maybe Auth -> Maybe Auth) -> Effect (Maybe Auth)
  | r
  } ->
  { read :: Effect Route
  , event :: Event.Event Route
  , navigate :: Route -> Effect Unit
  , redirect :: Route -> Effect Unit
  | s
  } ->
  AppInstance AppM
appInstance auth routing =
  { auth:
      { readAuth: liftEffect auth.read
      , readAuthEvent: liftEffect $ pure auth.event
      , modifyAuth: liftEffect <<< auth.modify
      }
  , routing:
      { readRoute: liftEffect routing.read
      , readRoutingEvent: liftEffect $ pure routing.event
      , navigate: liftEffect <<< routing.navigate
      , redirect: liftEffect <<< routing.redirect
      }
  , serverless:
      { getStateBuilder: mkStateBuilder \s _ -> s
      }
  , user: userInstance
  , article: articleInstance
  , comment: commentInstance
  , profile: profileInstance
  , tag: tagInstance
  }

userInstance :: UserInstance AppM
userInstance =
  let
    handleAuthRes =
      either
        (pure <<< Left)
        ( match
            { ok:
                \{ user: currentUser } -> do
                  void $ modifyAuth $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
                  pure $ Right currentUser
            , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
            }
        )
  in
    { loginUser:
        \credentials -> do
          res <- makeRequest (Apiary.Route :: Endpoints.LoginUser) Apiary.none Apiary.none { user: credentials }
          res # handleAuthRes
    , registerUser:
        \user -> do
          res <- makeRequest (Apiary.Route :: Endpoints.RegisterUser) Apiary.none Apiary.none { user }
          res # handleAuthRes
    , updateUser:
        \user -> do
          res <- makeSecureRequest (Apiary.Route :: Endpoints.UpdateUser) Apiary.none Apiary.none { user }
          res
            # either
                (pure <<< Left)
                ( match
                    { ok:
                        \{ user: currentUser } -> do
                          void $ modifyAuth $ map $ _ { user = Just $ Record.delete (SProxy :: _ "token") currentUser }
                          pure $ Right currentUser
                    , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
                    }
                )
    , logoutUser:
        do
          void $ modifyAuth $ const Nothing
          redirect Home
    }

articleInstance :: ArticleInstance AppM
articleInstance =
  { listArticles:
      \query -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListArticles) Apiary.none query Apiary.none
        pure $ res >>= match { ok: Right }
  , listFeed:
      \query -> do
        res <- makeSecureRequest (Apiary.Route :: Endpoints.ListFeed) Apiary.none query Apiary.none
        pure $ res >>= match { ok: Right }
  , getArticle:
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetArticle) { slug } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.article, notFound: Left <<< NotFound })
  , submitArticle:
      \slug article -> do
        res <- case slug of
          Nothing -> map expand <$> makeSecureRequest (Apiary.Route :: Endpoints.CreateArticle) Apiary.none Apiary.none { article }
          Just slug' -> map expand <$> makeSecureRequest (Apiary.Route :: Endpoints.UpdateArticle) { slug: slug' } Apiary.none { article }
        pure $ res >>= (match { ok: Right <<< _.article, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })
  , deleteArticle:
      \slug -> do
        res <- makeSecureRequest (Apiary.Route :: Endpoints.DeleteArticle) { slug } Apiary.none Apiary.none
        pure $ res >>= (match { ok: const $ Right unit })
  , toggleFavorite:
      \{ slug, favorited } -> do
        res <-
          if favorited then
            makeSecureRequest (Apiary.Route :: Endpoints.UnfavoriteArticle) { slug } Apiary.none Apiary.none
          else
            makeSecureRequest (Apiary.Route :: Endpoints.FavoriteArticle) { slug } Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.article }
  }

commentInstance :: CommentInstance AppM
commentInstance =
  { listComments:
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListComments) { slug } Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.comments }
  , createComment:
      \slug comment -> do
        res <- makeSecureRequest (Apiary.Route :: Endpoints.CreateComment) { slug } Apiary.none { comment }
        pure $ res >>= (match { ok: Right <<< _.comment })
  , deleteComment:
      \slug id -> do
        res <- makeSecureRequest (Apiary.Route :: Endpoints.DeleteComment) { slug, id } Apiary.none Apiary.none
        pure $ res >>= (match { ok: const $ Right unit })
  }

profileInstance :: ProfileInstance AppM
profileInstance =
  { getProfile:
      \username -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetProfile) { username } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.profile, notFound: Left <<< NotFound })
  , toggleFollow:
      \{ username, following } -> do
        res <-
          if following then
            makeSecureRequest (Apiary.Route :: Endpoints.UnfollowProfile) { username } Apiary.none Apiary.none
          else
            makeSecureRequest (Apiary.Route :: Endpoints.FollowProfile) { username } Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.profile }
  }

tagInstance :: TagInstance AppM
tagInstance =
  { listTags:
      do
        res <- makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.tags }
  }
