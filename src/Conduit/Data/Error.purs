module Conduit.Data.Error where

import Apiary.Types (Error) as Apiary
import Foreign.Object (Object)

data Error
  = NotAuthorized
  | NotFound { status :: String, error :: String }
  | UnprocessableEntity (Object (Array String))
  | ApiaryError Apiary.Error
