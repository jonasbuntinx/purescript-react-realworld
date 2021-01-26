module Conduit.Root where

import Prelude
import Conduit.AppM (AppM(..), navigate, readAuth, readRoute, redirect, subscribeToAuth)
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Component.Page as Page
import Conduit.Data.Auth (Auth)
import Conduit.Data.Env (Env)
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
import Control.Monad.Reader (ReaderT, ask)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React
import React.Halo as Halo
import Wire.Event as Event

data Action
  = Initialize
  | Navigate Route
  | UpdateAuth (Maybe Auth)

makeRoot :: Page.Component Unit
makeRoot = do
  auth <- liftEffect Event.create
  _ <- subscribeToAuth $ liftEffect <<< auth.push <<< UpdateAuth
  render <- mkRender
  Page.component "Root" { initialState, eval: eval auth } \self -> React.do
    -- React.useEffect (route /\ (isJust auth)) do
    --   case route, auth of
    --     Login, Just _ -> env.router.redirect Home
    --     Register, Just _ -> env.router.redirect Home
    --     Settings, Nothing -> env.router.redirect Home
    --     CreateArticle, Nothing -> env.router.redirect Home
    --     UpdateArticle _, Nothing -> env.router.redirect Home
    --     Error, _ -> env.router.redirect Home
    --     _, _ -> pure unit
    --   mempty
    pure $ render self
  where
  initialState =
    { auth: Nothing
    , route: Error
    }

  eval auth =
    Halo.makeEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction auth
        }

  handleAction auth = case _ of
    Initialize -> do
      currentAuth <- readAuth
      Halo.modify_ _ { auth = currentAuth }
      void $ Halo.subscribe auth.event
    Navigate route -> navigate route
    UpdateAuth newAuth -> Halo.modify_ _ { auth = newAuth }

  mkRender =
    -- homePage <- makeHomePage
    -- loginPage <- makeLoginPage
    -- registerPage <- makeRegisterPage
    -- settingsPage <- makeSettingsPage
    -- editorPage <- makeEditorPage
    -- articlePage <- makeArticlePage
    -- profilePage <- makeProfilePage
    pure
      $ \{ state, send } ->
          React.fragment
            [ Header.header { auth: state.auth, currentRoute: state.route, onNavigate: send <<< Navigate }
            , case state.route of
                -- Home -> homePage unit
                -- Login -> loginPage unit
                -- Register -> registerPage unit
                -- Settings -> settingsPage unit
                -- CreateArticle -> editorPage { slug: Nothing }
                -- UpdateArticle slug -> editorPage { slug: Just slug }
                -- ViewArticle slug -> articlePage { slug }
                -- Profile username -> profilePage { username, tab: Published }
                -- Favorites username -> profilePage { username, tab: Favorited }
                -- Error -> React.empty
                _ -> React.empty
            , Footer.footer
            ]
