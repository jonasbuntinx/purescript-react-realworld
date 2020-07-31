module Conduit.Api.User where

import Apiary.Media (JSON)
import Apiary.Route (GET, POST, PUT)
import Conduit.Data.Profile (ProfileRep, User)
import Conduit.Data.Username (Username)
import Foreign.Object (Object)

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
              JSON { user :: User }
          , unprocessableEntity ::
              JSON { errors :: Object (Array String) }
          }
      }

type Register
  = POST "/api/users"
      { body ::
          JSON
            { user ::
                { username :: Username
                , email :: String
                , password :: String
                }
            }
      , response ::
          { ok ::
              JSON { user :: User }
          , unprocessableEntity ::
              JSON { errors :: Object (Array String) }
          }
      }

type GetUser
  = GET "/api/user"
      { response ::
          { ok ::
              JSON { user :: User }
          }
      }

type UpdateUser
  = PUT "/api/user"
      { body ::
          JSON
            { user :: { | ProfileRep ( email :: String, password :: String ) }
            }
      , response ::
          { ok ::
              JSON { user :: User }
          , unprocessableEntity ::
              JSON { errors :: Object (Array String) }
          }
      }
