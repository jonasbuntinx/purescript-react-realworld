module Conduit.Root where

import Prelude
import Conduit.AppM (navigate, readAuth, readAuthEvent, redirect)
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Component.Page as Page
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route(..))
import Conduit.Page.Article (makeArticlePage)
import Conduit.Page.Editor (makeEditorPage)
import Conduit.Page.Home (makeHomePage)
import Conduit.Page.Login (makeLoginPage)
import Conduit.Page.Profile (Tab(..), makeProfilePage)
import Conduit.Page.Register (makeRegisterPage)
import Conduit.Page.Settings (makeSettingsPage)
import Control.Parallel (parTraverse_)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect.Console (log)
import React.Basic.Hooks as React
import React.Halo (liftEffect)
import React.Halo as Halo

data Action
  = Initialize (Array Action)
  | SubscribeToAuth
  | UpdateAuth (Maybe Auth)
  | SubscribeToRouting
  | UpdateRoute Route
  | Redirect Route
  | Navigate Route

makeRoot :: Page.Component Unit
makeRoot = do
  render <- mkRender
  Page.component "Root" { initialState, eval } \self@{ state, send } -> React.do
    React.useEffect (state.route /\ (isJust state.auth)) do
      case state.route, state.auth of
        Login, Just _ -> send $ Redirect Home
        Register, Just _ -> send $ Redirect Home
        Settings, Nothing -> send $ Redirect Home
        CreateArticle, Nothing -> send $ Redirect Home
        UpdateArticle _, Nothing -> send $ Redirect Home
        Error, _ -> send $ Redirect Home
        _, _ -> pure unit
      mempty
    pure $ render self
  where
  initialState =
    { auth: Nothing
    , route: Error
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just $ Initialize [ SubscribeToAuth, SubscribeToRouting ]
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize actions -> parTraverse_ handleAction actions
    SubscribeToAuth -> do
      auth <- readAuth
      Halo.modify_ _ { auth = auth }
      authEvent <- readAuthEvent
      void $ Halo.subscribe $ map UpdateAuth authEvent
    SubscribeToRouting -> do
      liftEffect $ log "FOOBAR"
    UpdateAuth auth -> Halo.modify_ _ { auth = auth }
    UpdateRoute route -> Halo.modify_ _ { route = route }
    Redirect route -> redirect route
    Navigate route -> navigate route

  mkRender = do
    homePage <- makeHomePage
    loginPage <- makeLoginPage
    registerPage <- makeRegisterPage
    settingsPage <- makeSettingsPage
    editorPage <- makeEditorPage
    articlePage <- makeArticlePage
    profilePage <- makeProfilePage
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
