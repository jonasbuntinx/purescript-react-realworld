module Conduit.Root where

import Prelude
import Conduit.Capability.Auth (class MonadAuth)
import Conduit.Capability.Auth as Auth
import Conduit.Capability.Halo (class MonadHalo, component)
import Conduit.Capability.Resource.Article (class ArticleRepository)
import Conduit.Capability.Resource.Comment (class CommentRepository)
import Conduit.Capability.Resource.Profile (class ProfileRepository)
import Conduit.Capability.Resource.Tag (class TagRepository)
import Conduit.Capability.Resource.User (class UserRepository)
import Conduit.Capability.Routing (class MonadRouting)
import Conduit.Capability.Routing as Routing
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
import Control.Monad.State (modify_)
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
  m (Unit -> React.JSX)
mkRoot = do
  render <- mkRender
  component "Root" { initialState, eval, render }
  where
  initialState _ =
    { auth: Nothing
    , route: Error
    }

  eval =
    Halo.mkEval
      Halo.defaultEval
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      -- auth
      handleAction <<< UpdateAuth =<< Auth.read
      Auth.subscribe UpdateAuth
      -- routing
      handleAction <<< UpdateRoute =<< Routing.read
      Routing.subscribe UpdateRoute
    UpdateAuth auth -> modify_ _ { auth = auth }
    UpdateRoute route -> do
      modify_ _ { route = route }
      auth <- Auth.read
      case route, auth of
        Login, Just _ -> Routing.redirect Home
        Register, Just _ -> Routing.redirect Home
        Settings, Nothing -> Routing.redirect Home
        CreateArticle, Nothing -> Routing.redirect Home
        UpdateArticle _, Nothing -> Routing.redirect Home
        Error, _ -> Routing.redirect Home
        _, _ -> pure unit
    Navigate route -> Routing.navigate route

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
