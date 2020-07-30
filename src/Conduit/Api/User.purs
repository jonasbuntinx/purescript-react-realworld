module Conduit.Api.User where

import Apiary.Media (JSON)
import Apiary.Route (GET, POST, PUT)
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
                { user :: { | ProfileRep ( email :: String, token :: String ) }
                }
          }
      }

type GetUser
  = GET "/api/user"
      { response ::
          { ok ::
              JSON
                { user :: { | ProfileRep ( email :: String, token :: String ) }
                }
          }
      }

type PutUser
  = PUT "/api/user"
      { body ::
          JSON
            { user :: { | ProfileRep ( email :: String, password :: String ) }
            }
      , response ::
          { ok ::
              JSON
                { user :: { | ProfileRep ( email :: String, token :: String ) }
                }
          }
      }
