module Conduit.Root where

import Prelude
import Conduit.Component.App as App
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Component.Routing (redirect)
import Conduit.Data.Route (Route(..))
import Conduit.Env (Env)
import Conduit.Hook.Auth (useAuth)
import Conduit.Hook.Routing (useRoute)
import Conduit.Page.Article (mkArticlePage)
import Conduit.Page.Editor (mkEditorPage)
import Conduit.Page.Home (mkHomePage)
import Conduit.Page.Login (mkLoginPage)
import Conduit.Page.Profile (Tab(..), mkProfilePage)
import Conduit.Page.Register (mkRegisterPage)
import Conduit.Page.Settings (mkSettingsPage)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import React.Basic.Hooks as React

mkRoot :: App.Component Env Unit
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
    React.useEffect (route /\ auth) do
      case route, auth of
        Login, Just _ -> do
          redirect Home
        Register, Just _ -> do
          redirect Home
        Settings, Nothing -> do
          redirect Home
        CreateArticle, Nothing -> do
          redirect Home
        UpdateArticle _, Nothing -> do
          redirect Home
        Error, _ -> do
          redirect Home
        _, _ -> do
          pure unit
      mempty
    pure
      $ React.fragment
          [ Header.header auth route
          , case route of
              Home -> do
                homePage unit
              Login -> do
                loginPage unit
              Register -> do
                registerPage unit
              Settings -> do
                settingsPage unit
              CreateArticle -> do
                editorPage { slug: Nothing }
              UpdateArticle slug -> do
                editorPage { slug: Just slug }
              ViewArticle slug -> do
                articlePage { slug }
              Profile username -> do
                profilePage { username, tab: Published }
              Favorites username -> do
                profilePage { username, tab: Favorited }
              Error -> do
                React.empty
          , Footer.footer
          ]
