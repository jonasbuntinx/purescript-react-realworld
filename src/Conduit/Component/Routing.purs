module Conduit.Component.Routing where

import Prelude
import Conduit.Control.Routing (Command(..), Completed, Pending, Routing) as RoutingControl
import Conduit.Data.Route (Route(..))
import Conduit.Effects.Routing (onPushState, redirect) as RoutingEffect
import Control.Monad.Free.Trans (runFreeT)
import Data.Foldable (traverse_)
import Data.Lens (Lens', Prism', is, lens, prism')
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Basic.Hooks as React
import Routing.Duplex (RouteDuplex')
import Wire.Event as Event
import Wire.React.Class (modify)
import Wire.React.Pure (Pure, create) as Pure

data Transition
  = Loading (Maybe Route) Route
  | Loaded (Maybe Route) Route

derive instance eqTransition :: Eq Transition

type RoutingAtom
  = Pure.Pure Transition

-- | Manager
mkRoutingManager ::
  RouteDuplex' Route ->
  (Route -> RoutingControl.Routing RoutingControl.Pending RoutingControl.Completed Unit) ->
  Effect (Tuple RoutingAtom (React.JSX -> React.JSX))
mkRoutingManager routes onNavigate = do
  routingAtom <- Pure.create $ Loading Nothing Home
  fiberRef <- Ref.new Nothing
  previousRouteRef <- Ref.new Nothing
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (RoutingEffect.onPushState routes) \(Tuple _ route) -> do
          Ref.read fiberRef >>= traverse_ \fiber -> launchAff_ do killFiber (error "Transition cancelled") fiber
          previousRoute <- Ref.read previousRouteRef
          modify routingAtom $ const $ Loading previousRoute route
          let
            finalise r =
              liftEffect do
                Ref.write (Just r) previousRouteRef
                modify routingAtom $ const $ Loaded previousRoute r
          fiber <-
            onNavigate route
              # unwrap
              # runFreeT
                  ( \cmd -> do
                      liftEffect $ Ref.write Nothing fiberRef
                      case cmd of
                        RoutingControl.Redirect route' -> RoutingEffect.redirect route'
                        RoutingControl.Override route' -> finalise route'
                        RoutingControl.Continue -> finalise route
                      mempty
                  )
              # launchAff
          Ref.write (Just fiber) fiberRef
      pure content
  pure $ Tuple routingAtom component

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
