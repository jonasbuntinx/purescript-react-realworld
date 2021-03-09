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
import Conduit.Context.HydratedState as HydratedState
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Page.Article as Article
import Conduit.Root as Root
import Control.Promise (Promise, fromAff)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Tuple.Nested ((/\))
import Data.Variant (match)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Foreign (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
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
        dehydratedState <- mkDehydratedState
        hydratedStateContext /\ hydratedStateProvider <- liftEffect $ HydratedState.mkHydratedStateProvider dehydratedState
        root <- Root.mkRoot
        pure
          { statusCode: 200
          , body:
              replace (Pattern "</body>")
                ( Replacement
                    $ "<script>window.renderWithState("
                    <> (fromMaybe "" (writeJSON <$> dehydratedState))
                    <> ")</script></body>"
                )
                ( replace
                    (Pattern "<div id=\"conduit\"></div>")
                    ( Replacement
                        $ renderToString
                        $ R.div { id: "conduit", children: [ hydratedStateProvider $ root unit ] }
                    )
                    document
                )
          }
  where
  mkDehydratedState = do
    route <- readRoute
    case route of
      ViewArticle slug -> do
        partial <- Article.mkPartialState slug
        pure $ Just $ write partial
      _ -> pure Nothing

appInstance :: { path :: String } -> Foreign -> AppInstance AppM
appInstance event context =
  (fixture :: AppInstance AppM)
    { routing =
      (fixture :: RoutingInstance AppM)
        { readRoute = pure $ either (const Error) identity $ parse routeCodec event.path
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
