module Conduit.Root where

import Prelude
import Conduit.Component.App as App
import Conduit.Component.Auth (AuthAtom)
import Conduit.Component.Header (header)
import Conduit.Component.Routing (RoutingAtom, _Transition)
import Conduit.Control.Routing (Completed, Pending, Routing, continue, redirect)
import Conduit.Data.Route (Route(..))
import Conduit.Page.Home (mkHomePage)
import Conduit.Page.Login (mkLoginPage)
import Conduit.Page.Settings (mkSettingsPage)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Reader as Reader
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import Wire.React (useAtomValue) as Wire
import Wire.React.Class (read) as Wire

mkRoot :: forall env. App.Component { authAtom :: AuthAtom, routingAtom :: RoutingAtom | env } Unit
mkRoot = do
  homePage <- mkHomePage
  loginPage <- mkLoginPage
  settingsPage <- mkSettingsPage
  { authAtom, routingAtom } <- Reader.ask
  Reader.lift
    $ React.component "Root" \props -> React.do
        auth <- Wire.useAtomValue authAtom
        route <- view _Transition <$> Wire.useAtomValue routingAtom
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

onNavigate :: AuthAtom -> Route -> Routing Pending Completed Unit
onNavigate authAtom route = Ix.do
  auth <- (liftEffect :: _ -> _ Pending Pending _) $ Wire.read authAtom
  case route, auth of
    Login, Just _ -> do
      redirect Home
    Settings, Nothing -> do
      redirect Home
    _, _ -> do
      continue
