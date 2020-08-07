module Conduit.Api.Tag where

import Apiary.Media (JSON)
import Apiary.Route (GET)

type ListTags
  = GET "/api/tags"
      { response ::
          { ok ::
              JSON
                { tags :: Array String
                }
          }
      }
