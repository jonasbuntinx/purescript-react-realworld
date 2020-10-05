module Conduit.Root where

import Prelude
import Conduit.Component.Env as Env
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Data.Route (Route(..))
import Conduit.Hook.Auth (useAuth)
import Conduit.Hook.Router (useRoute)
import Conduit.Page.Article (makeArticlePage)
import Conduit.Page.Editor (makeEditorPage)
import Conduit.Page.Home (makeHomePage)
import Conduit.Page.Login (makeLoginPage)
import Conduit.Page.Profile (Tab(..), makeProfilePage)
import Conduit.Page.Register (makeRegisterPage)
import Conduit.Page.Settings (makeSettingsPage)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import React.Basic.Hooks as React

makeRoot :: Env.Component Unit
makeRoot = do
  homePage <- makeHomePage
  loginPage <- makeLoginPage
  registerPage <- makeRegisterPage
  settingsPage <- makeSettingsPage
  editorPage <- makeEditorPage
  articlePage <- makeArticlePage
  profilePage <- makeProfilePage
  Env.component "Root" \env props -> React.do
    auth <- useAuth env
    route <- useRoute env
    React.useEffect (route /\ (isJust auth)) do
      case route, auth of
        Login, Just _ -> env.router.redirect Home
        Register, Just _ -> env.router.redirect Home
        Settings, Nothing -> env.router.redirect Home
        CreateArticle, Nothing -> env.router.redirect Home
        UpdateArticle _, Nothing -> env.router.redirect Home
        Error, _ -> env.router.redirect Home
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
