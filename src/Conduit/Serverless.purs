module Conduit.Serverless where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (makeRequest)
import Conduit.AppM (AppM, runAppM)
import Conduit.Capability.Resource.Article (ArticleInstance)
import Conduit.Capability.Resource.Comment (CommentInstance)
import Conduit.Capability.Resource.Profile (ProfileInstance)
import Conduit.Capability.Resource.Tag (TagInstance)
import Conduit.Capability.Resource.User (UserInstance)
import Conduit.Capability.Serverless (mkStateBuilder)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Serverless (Context, Event, Response)
import Conduit.Root as Root
import Data.Either (Either(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Variant (match)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import React.Basic.DOM.Server (renderToString)

serverless :: Event -> Context -> Aff Response
serverless event context = do
  document <- liftEffect $ readTextFile UTF8 "index.html"
  root <-
    runAppM
      { auth:
          { readAuth: notImplemented "readAuth"
          , readAuthEvent: notImplemented "readAuthEvent"
          , modifyAuth: \_ -> notImplemented "modifyAuth"
          }
      , routing:
          { readRouting: notImplemented "readRouting"
          , readRoutingEvent: notImplemented "readRoutingEvent"
          , navigate: \_ -> notImplemented "navigate"
          , redirect: \_ -> notImplemented "redirect"
          }
      , serverless:
          { getStateBuilder: mkStateBuilder \_ f -> f event context
          }
      , user: userInstance
      , article: articleInstance
      , comment: commentInstance
      , profile: profileInstance
      , tag: tagInstance
      }
      Root.mkRoot
  pure
    { statusCode: 200
    , body:
        replace
          (Pattern "<div id=\"conduit\"></div>")
          (Replacement $ "<div id=\"conduit\">" <> (renderToString $ root unit) <> "</div>")
          document
    }

notImplemented :: forall a. String -> AppM a
notImplemented label = liftEffect $ Exception.throw ("Fixture `" <> label <> "` is not implemented.")

userInstance :: UserInstance AppM
userInstance =
  { loginUser: \_ -> notImplemented "loginUser"
  , registerUser: \_ -> notImplemented "registerUser"
  , updateUser: \_ -> notImplemented "updateUser"
  , logoutUser: notImplemented "logoutUser"
  }

articleInstance :: ArticleInstance AppM
articleInstance =
  { listArticles:
      \query -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListArticles) Apiary.none query Apiary.none
        pure $ res >>= match { ok: Right }
  , listFeed: \_ -> notImplemented "listFeed"
  , getArticle:
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetArticle) { slug } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.article, notFound: Left <<< NotFound })
  , submitArticle: \_ _ -> notImplemented "submitArticle"
  , deleteArticle: \_ -> notImplemented "deleteArticle"
  , toggleFavorite: \_ -> notImplemented "toggleFavorite"
  }

commentInstance :: CommentInstance AppM
commentInstance =
  { listComments:
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListComments) { slug } Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.comments }
  , createComment: \_ _ -> notImplemented "createComment"
  , deleteComment: \_ _ -> notImplemented "deleteComment"
  }

profileInstance :: ProfileInstance AppM
profileInstance =
  { getProfile:
      \username -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetProfile) { username } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.profile, notFound: Left <<< NotFound })
  , toggleFollow: \_ -> notImplemented "toggleFollow"
  }

tagInstance :: TagInstance AppM
tagInstance =
  { listTags:
      do
        res <- makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.tags }
  }
