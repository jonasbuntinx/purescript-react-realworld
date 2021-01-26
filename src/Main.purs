module Main where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (makeRequest, makeSecureRequest)
import Conduit.AppM (AppImpl, AppM, UserApiImpl, modifyAuth, runAppM)
import Conduit.Component.Auth as Auth
import Conduit.Data.Auth (toAuth)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Root as Root
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import React.Basic as React
import React.Basic.DOM (render)
import Record as Record
import Routing.Duplex (parse, print)
import Routing.PushState as PushState
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Wire.React.Router as Router
import Wire.Signal as Signal

main :: Effect Unit
main = do
  container <- getElementById "conduit" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> Exception.throw "Conduit container element not found."
    Just c -> do
      auth <- Auth.makeAuthManager
      interface <- PushState.makeInterface
      routing <- Signal.create Error
      router <-
        Router.makeRouter interface
          { parse: parse routeCodec
          , print: print routeCodec
          , onRoute: const $ Router.continue
          , onTransition:
              case _ of
                Router.Resolved _ route -> routing.modify $ const route
                _ -> pure unit
          }
      launchAff_ do
        root <- runAppM appImpl Root.makeRoot
        liftEffect $ render (React.fragment [ router.component, auth.component, root unit ]) c

appImpl :: AppImpl AppM
appImpl =
  { auth:
      { readAuth: liftEffect $ Exception.throw "readAuth not implemented"
      , readAuthEvent: liftEffect $ Exception.throw "readAuthEvent not implemented"
      , modifyAuth: \_ -> liftEffect $ Exception.throw "modifyAuth not implemented"
      }
  , routing:
      { navigate: \_ -> liftEffect $ Exception.throw "navigate not implemented"
      , redirect: \_ -> liftEffect $ Exception.throw "redirect not implemented"
      , logout: liftEffect $ Exception.throw " logout not implemented"
      }
  , userApi: userApiImpl
  , articleApi:
      { listArticles: \_ -> liftEffect $ Exception.throw "listArticles not implemented"
      , listFeed: \_ -> liftEffect $ Exception.throw "listFeed not implemented"
      , getArticle: \_ -> liftEffect $ Exception.throw "getArticle not implemented"
      , submitArticle: \_ _ -> liftEffect $ Exception.throw "submitArticle not implemented"
      , deleteArticle: \_ -> liftEffect $ Exception.throw "deleteArticle not implemented"
      , toggleFavorite: \_ -> liftEffect $ Exception.throw "toggleFavorite not implemented"
      }
  , commentApi:
      { listComments: \_ -> liftEffect $ Exception.throw "listComments not implemented"
      , createComment: \_ _ -> liftEffect $ Exception.throw "createComment not implemented"
      , deleteComment: \_ _ -> liftEffect $ Exception.throw "deleteComment not implemented"
      }
  , profileApi:
      { getProfile: \_ -> liftEffect $ Exception.throw "getProfile not implemented"
      , toggleFollow: \_ -> liftEffect $ Exception.throw "toggleFollow not implemented"
      }
  , tagApi:
      { listTags: liftEffect $ Exception.throw "listTags not implemented"
      }
  }

userApiImpl :: UserApiImpl AppM
userApiImpl =
  { loginUser:
      \credentials -> do
        res <- makeRequest (Apiary.Route :: Endpoints.LoginUser) Apiary.none Apiary.none { user: credentials }
        res
          # either
              (pure <<< Left)
              ( match
                  { ok:
                      \{ user: currentUser } -> do
                        void $ modifyAuth $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
                        pure $ Right currentUser
                  , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
                  }
              )
  , registerUser:
      \user -> do
        res <- makeRequest (Apiary.Route :: Endpoints.RegisterUser) Apiary.none Apiary.none { user }
        res
          # either
              (pure <<< Left)
              ( match
                  { ok:
                      \{ user: currentUser } -> do
                        void $ modifyAuth $ const $ toAuth currentUser.token (Just $ Record.delete (SProxy :: _ "token") currentUser)
                        pure $ Right currentUser
                  , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
                  }
              )
  , updateUser:
      \user -> do
        res <- makeSecureRequest (Apiary.Route :: Endpoints.UpdateUser) Apiary.none Apiary.none { user }
        res
          # either
              (pure <<< Left)
              ( match
                  { ok:
                      \{ user: currentUser } -> do
                        void $ modifyAuth $ map $ _ { user = Just $ Record.delete (SProxy :: _ "token") currentUser }
                        pure $ Right currentUser
                  , unprocessableEntity: pure <<< Left <<< UnprocessableEntity <<< _.errors
                  }
              )
  }
