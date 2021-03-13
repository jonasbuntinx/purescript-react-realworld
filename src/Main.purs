module Main where

import Prelude
import Affjax.StatusCode (StatusCode(..))
import Conduit.Api.Client (Error, makeRequest, makeSecureRequest)
import Conduit.Api.Endpoint as Endpoint
import Conduit.AppM (AppM, runAppM)
import Conduit.Capability.Auth (modifyAuth)
import Conduit.Capability.Resource.Article (ArticleInstance)
import Conduit.Capability.Resource.Comment (CommentInstance)
import Conduit.Capability.Resource.Profile (ProfileInstance)
import Conduit.Capability.Resource.Tag (TagInstance)
import Conduit.Capability.Resource.User (UserInstance)
import Conduit.Capability.Routing (redirect)
import Conduit.Component.Auth as Auth
import Conduit.Component.Routing as Routing
import Conduit.Data.Article (Article, defaultArticlesQuery)
import Conduit.Data.Auth (toAuth)
import Conduit.Data.Comment (Comment)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Data.User (CurrentUser)
import Conduit.Root as Root
import Data.Either (Either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
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
      auth <- Auth.mkAuthManager
      routing <- Routing.mkRoutingManager
      launchAff_ do
        root <-
          runAppM
            { auth:
                { readAuth: liftEffect auth.read
                , readAuthEvent: pure auth.event
                , modifyAuth: liftEffect <<< auth.modify
                }
            , routing:
                { readRoute: liftEffect routing.read
                , readRoutingEvent: pure routing.event
                , navigate: liftEffect <<< routing.navigate
                , redirect: liftEffect <<< routing.redirect
                }
            , user: userInstance
            , article: articleInstance
            , comment: commentInstance
            , profile: profileInstance
            , tag: tagInstance
            }
            Root.mkRoot
        liftEffect $ render (React.fragment [ routing.component, auth.component, root unit ]) c

userInstance :: UserInstance AppM
userInstance =
  { loginUser:
      \credentials -> do
        (res :: Either Error { user :: CurrentUser }) <- makeRequest POST (StatusCode 200) Endpoint.Login { user: credentials }
        for_ res \{ user: currentUser } -> do
          modifyAuth $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
        pure $ res <#> _.user
  , registerUser:
      \user -> do
        (res :: Either Error { user :: CurrentUser }) <- makeRequest POST (StatusCode 200) Endpoint.Users { user }
        for_ res \{ user: currentUser } -> do
          modifyAuth $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
        pure $ res <#> _.user
  , updateUser:
      \user -> do
        (res :: Either Error { user :: CurrentUser }) <- makeSecureRequest PUT (StatusCode 200) Endpoint.User { user }
        for_ res \{ user: currentUser } -> do
          modifyAuth $ map $ _ { user = Just $ Record.delete (SProxy :: _ "token") currentUser }
        pure $ res <#> _.user
  , logoutUser:
      do
        void $ modifyAuth $ const Nothing
        redirect Home
  }

articleInstance :: ArticleInstance AppM
articleInstance =
  { listArticles:
      \query -> do
        makeRequest GET (StatusCode 200) (Endpoint.Articles query) unit
  , listFeed:
      \query -> do
        makeSecureRequest GET (StatusCode 200) (Endpoint.Feed query) unit
  , getArticle:
      \slug -> do
        (res :: Either Error { article :: Article }) <- makeRequest GET (StatusCode 200) (Endpoint.Article slug) unit
        pure $ res <#> _.article
  , submitArticle:
      \slug article -> do
        (res :: Either Error { article :: Article }) <- case slug of
          Nothing -> makeSecureRequest POST (StatusCode 200) (Endpoint.Articles defaultArticlesQuery) { article }
          Just slug' -> makeSecureRequest PUT (StatusCode 200) (Endpoint.Article slug') { article }
        pure $ res <#> _.article
  , deleteArticle:
      \slug -> do
        (res :: Either Error {}) <- makeSecureRequest DELETE (StatusCode 200) (Endpoint.Article slug) unit
        pure $ res <#> const unit
  , toggleFavorite:
      \{ slug, favorited } -> do
        (res :: Either Error { article :: Article }) <-
          if favorited then
            makeSecureRequest DELETE (StatusCode 200) (Endpoint.Favorite slug) unit
          else
            makeSecureRequest POST (StatusCode 200) (Endpoint.Favorite slug) unit
        pure $ res <#> _.article
  }

commentInstance :: CommentInstance AppM
commentInstance =
  { listComments:
      \slug -> do
        (res :: Either Error { comments :: Array Comment }) <- makeRequest GET (StatusCode 200) (Endpoint.Comments slug) unit
        pure $ res <#> _.comments
  , createComment:
      \slug comment -> do
        (res :: Either Error { comment :: Comment }) <- makeSecureRequest POST (StatusCode 200) (Endpoint.Comments slug) { comment }
        pure $ res <#> _.comment
  , deleteComment:
      \slug id -> do
        (res :: Either Error {}) <- makeSecureRequest DELETE (StatusCode 200) (Endpoint.Comment slug id) unit
        pure $ res <#> const unit
  }

profileInstance :: ProfileInstance AppM
profileInstance =
  { getProfile:
      \username -> do
        (res :: Either Error { profile :: Profile }) <- makeRequest GET (StatusCode 200) (Endpoint.Profiles username) unit
        pure $ res <#> _.profile
  , toggleFollow:
      \{ username, following } -> do
        (res :: Either Error { profile :: Profile }) <-
          if following then
            makeSecureRequest DELETE (StatusCode 200) (Endpoint.Follow username) unit
          else
            makeSecureRequest POST (StatusCode 200) (Endpoint.Follow username) unit
        pure $ res <#> _.profile
  }

tagInstance :: TagInstance AppM
tagInstance =
  { listTags:
      do
        (res :: Either Error { tags :: Array String }) <- makeRequest GET (StatusCode 200) Endpoint.Tags unit
        pure $ res <#> _.tags
  }
