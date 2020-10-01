module Conduit.Root where

import Prelude
import Conduit.AppM (runAppM)
import Conduit.Capability.Routing (redirect)
import Conduit.Component.App as App
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Data.Route (Route(..))
import Conduit.Hook.Auth (useAuth)
import Conduit.Hook.Router (useRoute)
import Conduit.Page.Article (mkArticlePage)
import Conduit.Page.Editor (mkEditorPage)
import Conduit.Page.Home (mkHomePage)
import Conduit.Page.Login (mkLoginPage)
import Conduit.Page.Profile (Tab(..), mkProfilePage)
import Conduit.Page.Register (mkRegisterPage)
import Conduit.Page.Settings (mkSettingsPage)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import React.Basic.Hooks as React

mkRoot :: App.Component Unit
mkRoot = do
  homePage <- mkHomePage
  loginPage <- mkLoginPage
  registerPage <- mkRegisterPage
  settingsPage <- mkSettingsPage
  editorPage <- mkEditorPage
  articlePage <- mkArticlePage
  profilePage <- mkProfilePage
  App.component' "Root" \env props -> React.do
    auth <- useAuth env
    route <- useRoute env
    React.useEffect (route /\ (isJust auth)) do
      runAppM env case route, auth of
        Login, Just _ -> redirect Home
        Register, Just _ -> redirect Home
        Settings, Nothing -> redirect Home
        CreateArticle, Nothing -> redirect Home
        UpdateArticle _, Nothing -> redirect Home
        Error, _ -> redirect Home
        _, _ -> pure unit
      mempty
    pure
      $ React.fragment
          [ Header.header { auth, currentRoute: route, onNavigate: env.router.navigate }
          , case route of
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
