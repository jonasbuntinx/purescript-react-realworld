module Conduit.Root where

import Prelude
import Conduit.Component.App as App
import Conduit.Component.Header (header)
import Conduit.Control.Routing (Completed, Pending, Routing, continue, redirect)
import Conduit.Data.Route (Route(..))
import Conduit.Env (Env)
import Conduit.Hook.Auth (useAuth)
import Conduit.Hook.Routing (useRoute)
import Conduit.Page.Home (mkHomePage)
import Conduit.Page.Login (mkLoginPage)
import Conduit.Page.Settings (mkSettingsPage)
import Conduit.State.Auth (AuthState)
import Control.Monad.Indexed.Qualified as Ix
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import Wire.React.Class (read) as Wire

mkRoot :: App.Component Env Unit
mkRoot = do
  homePage <- mkHomePage
  loginPage <- mkLoginPage
  settingsPage <- mkSettingsPage
  App.component' "Root" \env props -> React.do
    auth <- useAuth env
    route <- useRoute env
    pure
      $ React.fragment
          [ header auth route
          , case route of
              Home -> do
                homePage unit
              Login -> do
                loginPage unit
              Settings -> do
                settingsPage unit
              Error -> do
                R.text "Error"
              _ -> do
                React.empty
          ]

onNavigate :: AuthState -> Route -> Routing Pending Completed Unit
onNavigate authState route = Ix.do
  auth <- (liftEffect :: _ -> _ Pending Pending _) $ Wire.read authState
  case route, auth of
    Login, Just _ -> do
      redirect Home
    Settings, Nothing -> do
      redirect Home
    _, _ -> do
      continue
