module Conduit.Api.Profile where

import Apiary.Media (JSON)
import Apiary.Route (GET, POST, DELETE)
import Conduit.Data.Profile (Author)
import Conduit.Data.Username (Username)

type GetProfile
  = GET "/api/profiles/:username"
      { path ::
          { username :: Username
          }
      , response ::
          { ok ::
              JSON
                { profile :: Author
                }
          , notFound ::
              JSON
                { status :: String
                , error :: String
                }
          }
      }

-- | Follow
type FollowProfile
  = POST "/api/profiles/:username/follow"
      { path ::
          { username :: Username
          }
      , response ::
          { ok ::
              JSON
                { profile :: Author
                }
          }
      }

type UnfollowProfile
  = DELETE "/api/profiles/:username/follow"
      { path ::
          { username :: Username
          }
      , response ::
          { ok ::
              JSON
                { profile :: Author
                }
          }
      }
