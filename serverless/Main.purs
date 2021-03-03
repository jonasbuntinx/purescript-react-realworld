module Serverless.Main where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (makeRequest)
import Conduit.AppM (AppM, AppInstance, runAppM)
import Conduit.Capability.Resource.Article (ArticleInstance)
import Conduit.Capability.Resource.Comment (CommentInstance)
import Conduit.Capability.Resource.Profile (ProfileInstance)
import Conduit.Capability.Resource.Tag (TagInstance)
import Conduit.Capability.Routing (RoutingInstance, readRoute)
import Conduit.Capability.Serverless (mkStateBuilder)
import Conduit.Context.Hydrate (mkHydrateProvider)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Page.Article as Article
import Conduit.Root (mkRoot)
import Control.Promise (Promise, fromAff)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Tuple.Nested ((/\))
import Data.Variant (match)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Foreign (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Server (renderToString)
import Routing.Duplex (parse)
import Serverless.Fixture (fixture)
import Simple.JSON (write, writeJSON)

handler :: EffectFn2 { path :: String } Foreign (Promise { body :: String, statusCode :: Int })
handler =
  mkEffectFn2 \event context ->
    fromAff do
      document <- liftEffect $ readTextFile UTF8 "index.html"
      runAppM (appInstance event context) do
        dehydrated <- loadDehydrated
        hydrateContext /\ hydrateProvider <- liftEffect $ mkHydrateProvider dehydrated
        root <- mkRoot hydrateContext
        pure
          { statusCode: 200
          , body:
              replace
                (Pattern "<div id=\"conduit\"></div>")
                ( Replacement
                    $ renderToString
                    $ React.fragment
                        [ R.div { id: "conduit", children: [ hydrateProvider $ root unit ] }
                        , case dehydrated of
                            Just value -> R.script { dangerouslySetInnerHTML: { __html: "var dehydrated = " <> (writeJSON value) <> ";" } }
                            Nothing -> React.empty
                        ]
                )
                document
          }
  where
  loadDehydrated = do
    route <- readRoute
    case route of
      ViewArticle slug -> do
        initialState <- Article.mkInitialPartialState slug
        pure $ Just $ write initialState
      _ -> pure Nothing

appInstance :: { path :: String } -> Foreign -> AppInstance AppM
appInstance event context =
  (fixture :: AppInstance AppM)
    { routing =
      (fixture :: RoutingInstance AppM)
        { readRoute = pure $ either (const Error) identity $ parse routeCodec event.path
        }
    , serverless =
      { getStateBuilder: mkStateBuilder \_ f -> f event context
      }
    , article = articleInstance
    , comment = commentInstance
    , profile = profileInstance
    , tag = tagInstance
    }

articleInstance :: ArticleInstance AppM
articleInstance =
  (fixture :: ArticleInstance AppM)
    { listArticles =
      \query -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListArticles) Apiary.none query Apiary.none
        pure $ res >>= match { ok: Right }
    , getArticle =
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetArticle) { slug } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.article, notFound: Left <<< NotFound })
    }

commentInstance :: CommentInstance AppM
commentInstance =
  (fixture :: CommentInstance AppM)
    { listComments =
      \slug -> do
        res <- makeRequest (Apiary.Route :: Endpoints.ListComments) { slug } Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.comments }
    }

profileInstance :: ProfileInstance AppM
profileInstance =
  (fixture :: ProfileInstance AppM)
    { getProfile =
      \username -> do
        res <- makeRequest (Apiary.Route :: Endpoints.GetProfile) { username } Apiary.none Apiary.none
        pure $ res >>= (match { ok: Right <<< _.profile, notFound: Left <<< NotFound })
    }

tagInstance :: TagInstance AppM
tagInstance =
  { listTags:
      do
        res <- makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
        pure $ res >>= match { ok: Right <<< _.tags }
  }
