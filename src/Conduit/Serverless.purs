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
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Root as Root
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Variant (match)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FRP.Event as Event
import Foreign (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import React.Basic.DOM.Server (renderToString)
import Routing.Duplex (parse)

type Event
  = { path :: String
    }

type Context
  = Foreign

type Response
  = { body :: String
    , statusCode :: Int
    }

serverless :: Event -> Context -> Aff Response
serverless { path } _ = do
  document <- liftEffect $ readTextFile UTF8 "index.html"
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
  pure
    { statusCode: 200
    , body:
        replace
          (Pattern "<div id=\"conduit\"></div>")
          (Replacement $ "<div id=\"conduit\">" <> (renderToString $ root unit) <> "</div>")
          document
    }

userInstance :: UserInstance AppM
userInstance =
  { loginUser: \_ -> pure $ Left Unavailable
  , registerUser: \_ -> pure $ Left Unavailable
  , updateUser: \_ -> pure $ Left Unavailable
  , logoutUser: pure unit
  }

articleInstance :: ArticleInstance AppM
articleInstance =
  { listArticles:
      \query -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListArticles) Apiary.none query Apiary.none
        pure $ res >>= match { ok: Right }
  , listFeed: \_ -> pure $ Left Unavailable
  , getArticle:
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetArticle) { slug } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.article, notFound: Left <<< NotFound })
  , submitArticle: \_ _ -> pure $ Left Unavailable
  , deleteArticle: \_ -> pure $ Left Unavailable
  , toggleFavorite: \_ -> pure $ Left Unavailable
  }

commentInstance :: CommentInstance AppM
commentInstance =
  { listComments:
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListComments) { slug } Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.comments }
  , createComment: \_ _ -> pure $ Left Unavailable
  , deleteComment: \_ _ -> pure $ Left Unavailable
  }

profileInstance :: ProfileInstance AppM
profileInstance =
  { getProfile:
      \username -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetProfile) { username } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.profile, notFound: Left <<< NotFound })
  , toggleFollow: \_ -> pure $ Left Unavailable
  }

tagInstance :: TagInstance AppM
tagInstance =
  { listTags:
      do
        res <- makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.tags }
  }
