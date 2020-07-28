module Conduit.Api.User where

import Apiary.Media (JSON)
import Apiary.Route (POST)
import Data.Maybe (Maybe)

type Login
  = POST "/api/users/login"
      { body ::
          JSON
            { user ::
                { email :: String
                , password :: String
                }
            }
      , response ::
          { ok ::
              JSON
                { user :: User ()
                }
          }
      }

-- | Types
type User r
  = Profile
      ( token :: Token
      | r
      )

type Profile r
  = { username :: String
    , bio :: Maybe String
    , image :: Maybe String
    | r
    }

type Token
  = String
