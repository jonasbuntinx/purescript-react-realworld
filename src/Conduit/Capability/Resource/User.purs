module Conduit.Capability.Resource.User where

import Prelude
import Conduit.Api.Client (Error)
import Conduit.Data.User (CurrentUser, UserRep)
import Conduit.Data.Username (Username)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import React.Halo (HaloM)

class
  Monad m <=
  UserRepository m where
  loginUser :: { email :: String, password :: String } -> m (Either Error CurrentUser)
  registerUser :: { username :: Username, email :: String, password :: String } -> m (Either Error CurrentUser)
  updateUser :: { | UserRep (password :: String) } -> m (Either Error CurrentUser)
  logoutUser :: m Unit

instance UserRepository m => UserRepository (HaloM props ctx state action m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  updateUser = lift <<< updateUser
  logoutUser = lift logoutUser
