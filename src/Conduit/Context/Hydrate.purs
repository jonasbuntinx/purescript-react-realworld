module Conduit.Context.Hydrate where

import Prelude
import Conduit.AppM (AppM)
import Conduit.Capability.Resource.Article (getArticle)
import Conduit.Capability.Resource.Comment (listComments)
import Conduit.Capability.Routing (readRoute)
import Conduit.Data.Route (Route(..))
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import React.Basic.Hooks as React
import Simple.JSON (write)

type Context
  = React.ReactContext { value :: Maybe Foreign, delete :: Effect Unit }

mkHydrateProvider :: AppM (Context /\ (React.JSX -> React.JSX))
mkHydrateProvider = do
  dehydrated <- mkMockup
  context <- liftEffect $ React.createContext { value: dehydrated, delete: pure unit }
  component <-
    liftEffect
      $ React.component "HydrateProvider " \content -> React.do
          state /\ setState <- React.useState' dehydrated
          pure $ React.provider context { value: state, delete: setState Nothing } $ pure content
  pure (context /\ component)

-- TODO: replace with using JSON provided by the server
mkMockup :: AppM (Maybe Foreign)
mkMockup = do
  route <- readRoute
  case route of
    ViewArticle slug -> do
      initialState <- do
        article <- getArticle slug
        comments <- listComments slug
        pure { article: hush article, comments: hush comments }
      pure $ Just $ write initialState
    _ -> pure Nothing
