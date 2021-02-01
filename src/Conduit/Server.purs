module Conduit.Server where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (makeRequest, makeSecureRequest)
import Conduit.AppM (AppM, runAppM)
import Conduit.Capability.Auth (modifyAuth)
import Conduit.Capability.Resource.Article (ArticleInstance)
import Conduit.Capability.Resource.Comment (CommentInstance)
import Conduit.Capability.Resource.Profile (ProfileInstance)
import Conduit.Capability.Resource.Tag (TagInstance)
import Conduit.Capability.Resource.User (UserInstance)
import Conduit.Capability.Routing (redirect)
import Conduit.Data.Auth (toAuth)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Root as Root
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Symbol (SProxy(..))
import Data.Variant (expand, match)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event as Event
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import React.Basic.DOM.Server (renderToString)
import Record as Record
import Routing.Duplex (parse)

server :: String -> (String -> Effect Unit) -> Effect Unit
server path fn = do
  document <- readTextFile UTF8 "index.html"
  launchAff_ do
    root <-
      runAppM
        { auth:
            { readAuth: liftEffect $ pure Nothing
            , readAuthEvent: liftEffect $ map _.event Event.create
            , modifyAuth: liftEffect <<< (const $ pure Nothing)
            }
        , routing:
            { readRoute: liftEffect $ pure $ either (const Error) identity $ parse routeCodec path
            , readRoutingEvent: liftEffect $ map _.event Event.create
            , navigate: liftEffect <<< (const $ pure unit)
            , redirect: liftEffect <<< (const $ pure unit)
            }
        , user: userInstance
        , article: articleInstance
        , comment: commentInstance
        , profile: profileInstance
        , tag: tagInstance
        }
        Root.mkRoot
    liftEffect
      $ fn
      $ replace
          (Pattern "<div id=\"conduit\"></div>")
          (Replacement $ "<div id=\"conduit\">" <> (renderToString $ root unit) <> "</div>")
          document

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
