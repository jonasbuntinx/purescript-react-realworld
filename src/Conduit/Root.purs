module Conduit.Root where

import Prelude
import Conduit.Capability.Auth (class MonadAuth, readAuth, readAuthEvent)
import Conduit.Capability.Halo (class MonadHalo, JSX, component)
import Conduit.Capability.Resource.Article (class ArticleRepository)
import Conduit.Capability.Resource.Comment (class CommentRepository)
import Conduit.Capability.Resource.Profile (class ProfileRepository)
import Conduit.Capability.Resource.Tag (class TagRepository)
import Conduit.Capability.Resource.User (class UserRepository)
import Conduit.Capability.Routing (class MonadRouting, navigate, readRoute, readRoutingEvent, redirect)
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route(..))
import Conduit.Page.Article (mkArticlePage)
import Conduit.Page.Editor (mkEditorPage)
import Conduit.Page.Home (mkHomePage)
import Conduit.Page.Login (mkLoginPage)
import Conduit.Page.Profile (Tab(..), mkProfilePage)
import Conduit.Page.Register (mkRegisterPage)
import Conduit.Page.Settings (mkSettingsPage)
import Data.Maybe (Maybe(..))
import React.Basic.Hooks as React
import React.Halo as Halo

data Action
  = Initialize
  | UpdateAuth (Maybe Auth)
  | UpdateRoute Route
  | Navigate Route

mkRoot ::
  forall m.
  MonadAuth m =>
  MonadRouting m =>
  TagRepository m =>
  ArticleRepository m =>
  MonadHalo m =>
  UserRepository m =>
  CommentRepository m =>
  ProfileRepository m =>
  m (Unit -> JSX)
mkRoot = do
  route <- readRoute
  render <- mkRender
  component "Root" { initialState: { auth: Nothing, route }, eval, render }
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
      route <- readRoute
      handleAction $ UpdateRoute route
      routingEvent <- readRoutingEvent
      void $ Halo.subscribe $ map UpdateRoute routingEvent
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
