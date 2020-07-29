module Conduit.Data.Transition where

import Prelude
import Conduit.Data.Route (Route)
import Data.Lens (Lens', Prism', is, lens, prism')
import Data.Maybe (Maybe(..))

data Transition
  = Loading (Maybe Route) Route
  | Loaded (Maybe Route) Route

derive instance eqTransition :: Eq Transition

-- | Lenses
_Transition :: Lens' Transition Route
_Transition = lens getter setter
  where
  getter = case _ of
    Loading _ route -> route
    Loaded _ route -> route

  setter = case _ of
    Loading previous _ -> Loading previous
    Loaded previous _ -> Loaded previous

_Loading :: Prism' Transition Route
_Loading =
  prism' (Loading Nothing) case _ of
    Loading _ route -> Just route
    _ -> Nothing

_Loaded :: Prism' Transition Route
_Loaded =
  prism' (Loaded Nothing) case _ of
    Loaded _ route -> Just route
    _ -> Nothing

-- | Helpers
isLoading :: Transition -> Boolean
isLoading = is _Loading

isLoaded :: Transition -> Boolean
isLoaded = is _Loaded
