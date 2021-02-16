module Conduit.Root where

import Prelude
import Conduit.Capability.Access (readAccess, readAccessEvent)
import Conduit.Capability.Routing (navigate, readRouting, readRoutingEvent, redirect)
import Conduit.Component.App as App
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Data.Access (Access(..))
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route(..))
import Conduit.Page.Article as Article
import Conduit.Page.Editor as Editor
import Conduit.Page.Home as Home
import Conduit.Page.Login as Login
import Conduit.Page.Profile as Profile
import Conduit.Page.Register as Register
import Conduit.Page.Settings as Settings
import Data.Maybe (Maybe(..))
import React.Basic.Hooks as React
import React.Halo as Halo

data Action
  = Initialize
  | UpdateAccess (Access Auth)
  | UpdateRouting { route :: Route, prevRoute :: Maybe Route }
  | Navigate Route

mkComponent :: App.Component Unit
mkComponent = do
  routing <- readRouting
  render <- mkRender
  App.component "Root" { initialState: { access: Public, routing }, eval, render }
  where
  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      -- access
      access <- readAccess
      handleAction $ UpdateAccess access
      accessEvent <- readAccessEvent
      void $ Halo.subscribe $ map UpdateAccess accessEvent
      -- routing
      routing <- readRouting
      handleAction $ UpdateRouting routing
      routingEvent <- readRoutingEvent
      void $ Halo.subscribe $ map UpdateRouting routingEvent
    UpdateAccess access -> Halo.modify_ _ { access = access }
    UpdateRouting routing -> do
      Halo.modify_ _ { routing = routing }
      access <- readAccess
      case routing.route, access of
        Login, Authorized _ -> redirect Home
        Register, Authorized _ -> redirect Home
        Settings, Public -> redirect Home
        CreateArticle, Public -> redirect Home
        UpdateArticle _, Public -> redirect Home
        Error, _ -> redirect Home
        _, _ -> pure unit
    Navigate route -> navigate route

  mkInitialPage = do
    { route } <- readRouting
    case route of
      Login -> do
        component <- Login.mkComponent
        pure $ Just $ component unit
      Register -> do
        component <- Register.mkComponent
        pure $ Just $ component unit
      ViewArticle slug -> do
        let
          props = { slug }
        component <- Article.mkComponent <<< Just =<< Article.mkInitialState props
        pure $ Just $ component props
      Profile username -> do
        let
          props = { username, tab: Profile.Published }
        component <- Profile.mkComponent <<< Just =<< Profile.mkInitialState props
        pure $ Just $ component props
      Favorites username -> do
        let
          props = { username, tab: Profile.Favorited }
        component <- Profile.mkComponent <<< Just =<< Profile.mkInitialState props
        pure $ Just $ component props
      _ -> pure Nothing

  mkRender = do
    initialPage <- mkInitialPage
    homePage <- Home.mkComponent
    loginPage <- Login.mkComponent
    registerPage <- Register.mkComponent
    settingsPage <- Settings.mkComponent
    editorPage <- Editor.mkComponent
    articlePage <- Article.mkComponent Nothing
    profilePage <- Profile.mkComponent Nothing
    pure
      $ \{ state, send } ->
          React.fragment
            [ Header.header
                { access: state.access
                , currentRoute: state.routing.route
                , onNavigate: send <<< Navigate
                }
            , case initialPage, state.routing.prevRoute of
                Just page, Nothing -> page
                _, _ -> case state.routing.route of
                  Home -> homePage unit
                  Login -> loginPage unit
                  Register -> registerPage unit
                  Settings -> settingsPage unit
                  CreateArticle -> editorPage { slug: Nothing }
                  UpdateArticle slug -> editorPage { slug: Just slug }
                  ViewArticle slug -> articlePage { slug }
                  Profile username -> profilePage { username, tab: Profile.Published }
                  Favorites username -> profilePage { username, tab: Profile.Favorited }
                  Error -> React.empty
            , Footer.footer
            ]
