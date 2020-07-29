module Conduit.Api.User where

import Apiary.Media (JSON)
import Apiary.Route (POST, GET)
import Conduit.Data.Profile (ProfileRep)

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
                { user :: { | ProfileRep ( token :: String ) }
                }
          }
      }

type GetUser
  = GET "/api/user"
      { response ::
          { ok ::
              JSON
                { user :: { | ProfileRep ( token :: String ) }
                }
          }
      }
