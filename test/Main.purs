module Test.Main where

import Prelude
import Conduit.AppM (AppInstance, AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception as Exception

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

appInstance :: AppInstance AppM
appInstance =
  let
    throw :: forall m a. MonadEffect m => String -> m a
    throw = liftEffect <<< Exception.throw
  in
    { auth:
        { readAuth: throw "readAuth not implemented"
        , readAuthEvent: throw "readAuthEvent not implemented"
        , modifyAuth: \_ -> throw "modifyAuth not implemented"
        }
    , routing:
        { readRoute: throw "readRoute not implemented"
        , readRoutingEvent: throw "readRoutingEvent not implemented"
        , navigate: \_ -> throw "navigate not implemented"
        , redirect: \_ -> throw "redirect not implemented"
        }
    , user:
        { loginUser: \_ -> throw "loginUser not implemented"
        , registerUser: \_ -> throw "registerUser not implemented"
        , updateUser: \_ -> throw "updateUser not implemented"
        , logoutUser: throw " logoutUser not implemented"
        }
    , article:
        { listArticles: \_ -> throw "listArticles not implemented"
        , listFeed: \_ -> throw "listFeed not implemented"
        , getArticle: \_ -> throw "getArticle not implemented"
        , submitArticle: \_ _ -> throw "submitArticle not implemented"
        , deleteArticle: \_ -> throw "deleteArticle not implemented"
        , toggleFavorite: \_ -> throw "toggleFavorite not implemented"
        }
    , comment:
        { listComments: \_ -> throw "listComments not implemented"
        , createComment: \_ _ -> throw "createComment not implemented"
        , deleteComment: \_ _ -> throw "deleteComment not implemented"
        }
    , profile:
        { getProfile: \_ -> throw "getProfile not implemented"
        , toggleFollow: \_ -> throw "toggleFollow not implemented"
        }
    , tag:
        { listTags: throw "listTags not implemented"
        }
    }
