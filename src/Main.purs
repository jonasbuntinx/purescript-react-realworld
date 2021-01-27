module Main where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (makeRequest, makeSecureRequest)
import Conduit.AppM (AppImpl, AppM, modifyAuth, runAppM)
import Conduit.Component.Auth as Auth
import Conduit.Component.Routing as Routing
import Conduit.Data.Auth (Auth, toAuth)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Conduit.Root as Root
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (expand, match)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import FRP.Event as Event
import React.Basic as React
import React.Basic.DOM (render)
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
      auth <- Auth.makeAuthManager
      routing <- Routing.makeRoutingManager
      launchAff_ do
        root <- runAppM (appImpl { auth, routing }) Root.makeRoot
        liftEffect $ render (React.fragment [ routing.component, auth.component, root unit ]) c

appImpl ::
  forall r s.
  { auth ::
      { read :: Effect (Maybe Auth)
      , event :: Event.Event (Maybe Auth)
      , modify :: (Maybe Auth -> Maybe Auth) -> Effect (Maybe Auth)
      | r
      }
  , routing ::
      { read :: Effect Route
      , event :: Event.Event Route
      , navigate :: Route -> Effect Unit
      , redirect :: Route -> Effect Unit
      | s
      }
  } ->
  AppImpl AppM
appImpl { auth, routing } =
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
  , userApi:
      { loginUser:
          \credentials -> do
            res <- makeRequest (Apiary.Route :: Endpoints.LoginUser) Apiary.none Apiary.none { user: credentials }
            res
              # either
                  (pure <<< Left)
                  ( match
                      { ok:
                          \{ user: currentUser } -> do
                            void $ modifyAuth $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
                            pure $ Right currentUser
                      , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
                      }
                  )
      , registerUser:
          \user -> do
            res <- makeRequest (Apiary.Route :: Endpoints.RegisterUser) Apiary.none Apiary.none { user }
            res
              # either
                  (pure <<< Left)
                  ( match
                      { ok:
                          \{ user: currentUser } -> do
                            void $ modifyAuth $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
                            pure $ Right currentUser
                      , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
                      }
                  )
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
            liftEffect $ routing.redirect Home
      }
  , articleApi:
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
  , commentApi:
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
  , profileApi:
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
  , tagApi:
      { listTags:
          do
            res <- makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
            pure $ res >>= match { ok: Right <<< _.tags }
      }
  }
