module Test.Main where

import Prelude
import Conduit.AppM (AppInst, AppM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception as Exception

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

appInst :: AppInst AppM
appInst =
  { auth:
      { readAuth: liftEffect $ Exception.throw "readAuth not implemented"
      , readAuthEvent: liftEffect $ Exception.throw "readAuthEvent not implemented"
      , modifyAuth: \_ -> liftEffect $ Exception.throw "modifyAuth not implemented"
      }
  , routing:
      { readRoute: liftEffect $ Exception.throw "readRoute not implemented"
      , readRoutingEvent: liftEffect $ Exception.throw "readRoutingEvent not implemented"
      , navigate: \_ -> liftEffect $ Exception.throw "navigate not implemented"
      , redirect: \_ -> liftEffect $ Exception.throw "redirect not implemented"
      }
  , userApi:
      { loginUser: \_ -> liftEffect $ Exception.throw "loginUser not implemented"
      , registerUser: \_ -> liftEffect $ Exception.throw "registerUser not implemented"
      , updateUser: \_ -> liftEffect $ Exception.throw "updateUser not implemented"
      , logoutUser: liftEffect $ Exception.throw " logoutUser not implemented"
      }
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
