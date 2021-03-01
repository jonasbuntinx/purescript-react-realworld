module Conduit.Context.Hydrate where

import Prelude
import Conduit.AppM (AppM)
import Conduit.Capability.Resource.Article (getArticle, listArticles)
import Conduit.Capability.Resource.Comment (listComments)
import Conduit.Capability.Resource.Profile (getProfile)
import Conduit.Capability.Routing (readRouting)
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Data.Route (Route(..))
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign (Foreign)
import React.Basic.Hooks as React
import Simple.JSON (write)

type Context
  = React.ReactContext { value :: Maybe Foreign, delete :: Effect Unit }

mkHydrateProvider :: AppM (Context /\ (React.JSX -> React.JSX))
mkHydrateProvider = do
  { value, delete } <- create
  context <- liftEffect $ React.createContext { value, delete }
  component <-
    liftEffect
      $ React.component "HydrateProvider " \content -> React.do
          pure $ React.provider context { value, delete } $ pure content
  pure (context /\ component)
  where
  create = do
    dehydrated <- mkMockup
    value <- liftEffect $ Ref.new dehydrated
    pure
      { value: dehydrated
      , delete: Ref.write Nothing value
      }

-- TODO: replace with using JSON provided by the server
mkMockup :: AppM (Maybe Foreign)
mkMockup = do
  { route } <- readRouting
  case route of
    ViewArticle slug -> do
      initialState <- do
        article <- getArticle slug
        comments <- listComments slug
        pure { article: hush article, comments: hush comments }
      pure $ Just $ write initialState
    Profile username -> do
      initialState <- do
        profile <- getProfile username
        articles <- listArticles $ defaultArticlesQuery { author = Just username }
        pure { profile: hush profile, articles: hush articles }
      pure $ Just $ write initialState
    Favorites username -> do
      initialState <- do
        profile <- getProfile username
        articles <- listArticles $ defaultArticlesQuery { favorited = Just username }
        pure { profile: hush profile, articles: hush articles }
      pure $ Just $ write initialState
    _ -> pure Nothing
