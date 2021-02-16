module Conduit.Data.Access where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Lens (Prism', is, prism)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault)

data Access a
  = Public
  | Authorized a

instance functorAccess :: Functor Access where
  map _ Public = Public
  map fn (Authorized x) = Authorized (fn x)

instance applyAccess :: Apply Access where
  apply Public _ = Public
  apply (Authorized fn) x = fn <$> x

instance applicativeAccess :: Applicative Access where
  pure = Authorized

instance foldableAccess :: Foldable Access where
  foldMap _ Public = mempty
  foldMap f (Authorized a) = f a
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableAccess :: Traversable Access where
  traverse _ Public = pure Public
  traverse f (Authorized a) = Authorized <$> f a
  sequence = sequenceDefault

-- | Lenses
_Authorized :: forall a. Prism' (Access a) a
_Authorized = prism Authorized unwrap
  where
  unwrap (Authorized x) = Right x

  unwrap y = Left y

_Public :: forall a. Prism' (Access a) Unit
_Public = prism (const Public) unwrap
  where
  unwrap Public = Right unit

  unwrap y = Left y

isAuthorized :: forall a. Access a -> Boolean
isAuthorized = is _Authorized

isPublic :: forall a. Access a -> Boolean
isPublic = is _Public

-- | Helpers
toMaybe :: forall a. Access a -> Maybe a
toMaybe (Authorized value) = Just value

toMaybe _ = Nothing

fromMaybe :: forall a. Maybe a -> Access a
fromMaybe Nothing = Public

fromMaybe (Just value) = Authorized value

maybe :: forall a b. b -> (a -> b) -> Access a -> b
maybe _ f (Authorized a) = f a

maybe b _ _ = b

withDefault :: forall a. a -> Access a -> a
withDefault b = maybe b identity
