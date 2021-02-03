module Conduit.Root where

import Prelude
import Conduit.Capability.Auth (readAuth, readAuthEvent)
import Conduit.Capability.Routing (navigate, readRouting, readRoutingEvent, redirect)
import Conduit.Capability.Serverless (buildInitialState)
import Conduit.Component.App as App
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Page.Article (mkArticlePage)
import Conduit.Page.Editor (mkEditorPage)
import Conduit.Page.Home (mkHomePage)
import Conduit.Page.Login (mkLoginPage)
import Conduit.Page.Profile (Tab(..), mkProfilePage)
import Conduit.Page.Register (mkRegisterPage)
import Conduit.Page.Settings (mkSettingsPage)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import React.Basic.Hooks as React
import React.Halo as Halo
import Routing.Duplex (parse)

data Action
  = Initialize
  | UpdateAuth (Maybe Auth)
  | UpdateRoute Route
  | Navigate Route

mkRoot :: App.Component Unit
mkRoot = do
  render <- mkRender
  initialState' <-
    buildInitialState
      ( do
          route <- _.route <$> readRouting
          pure { auth: Nothing, route: route }
      )
      ( \{ path } _ -> do
          pure { auth: Nothing, route: either (const Error) identity $ parse routeCodec path }
      )
  App.component "Root" { initialState: initialState', eval, render }
  where
  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      -- auth
      auth <- readAuth
      handleAction $ UpdateAuth auth
      authEvent <- readAuthEvent
      void $ Halo.subscribe $ map UpdateAuth authEvent
      -- routing
      route <- _.route <$> readRouting
      handleAction $ UpdateRoute route
      routingEvent <- readRoutingEvent
      void $ Halo.subscribe $ map (UpdateRoute <<< _.route) routingEvent
    UpdateAuth auth -> Halo.modify_ _ { auth = auth }
    UpdateRoute route -> do
      Halo.modify_ _ { route = route }
      auth <- readAuth
      case route, auth of
        Login, Just _ -> redirect Home
        Register, Just _ -> redirect Home
        Settings, Nothing -> redirect Home
        CreateArticle, Nothing -> redirect Home
        UpdateArticle _, Nothing -> redirect Home
        Error, _ -> redirect Home
        _, _ -> pure unit
    Navigate route -> navigate route

  mkRender = do
    homePage <- mkHomePage
    loginPage <- mkLoginPage
    registerPage <- mkRegisterPage
    settingsPage <- mkSettingsPage
    editorPage <- mkEditorPage
    articlePage <- mkArticlePage
    profilePage <- mkProfilePage
    pure
      $ \{ state, send } ->
          React.fragment
            [ Header.header { auth: state.auth, currentRoute: state.route, onNavigate: send <<< Navigate }
            , case state.route of
                Home -> homePage unit
                Login -> loginPage unit
                Register -> registerPage unit
                Settings -> settingsPage unit
                CreateArticle -> editorPage { slug: Nothing }
                UpdateArticle slug -> editorPage { slug: Just slug }
                ViewArticle slug -> articlePage { slug }
                Profile username -> profilePage { username, tab: Published }
                Favorites username -> profilePage { username, tab: Favorited }
                Error -> React.empty
            , Footer.footer
            ]
