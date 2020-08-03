module Conduit.Api.Profile where

import Apiary.Media (JSON)
import Apiary.Route (GET)
import Conduit.Data.Profile (Author)
import Conduit.Data.Username (Username)

type GetProfile
  = GET "/api/profiles/:username"
      { path ::
          { username :: Username
          }
      , response ::
          { ok ::
              JSON { profile :: Author }
          }
      }
