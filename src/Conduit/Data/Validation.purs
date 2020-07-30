module Conduit.Data.Validation where

import Prelude
import Control.Comonad (class Comonad, class Extend)
import Data.Bifunctor (lmap)
import Data.Either (fromRight)
import Data.Lens (Lens, Lens', Prism', lens, over, prism', view)
import Data.Lens as L
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.Validation.Semigroup (V(..))
import Data.Validation.Semigroup as V
import Heterogeneous.Mapping (class HMap, class MapRecordWithIndex, class Mapping, ConstMapping, hmap, mapping)
import Partial.Unsafe (unsafePartial)
import Prim.RowList as RL

data Validated a
  = Fresh a
  | Modified a

derive instance functorValidated :: Functor Validated

instance applyValidated :: Apply Validated where
  apply (Fresh f) a = map f a
  apply (Modified f) (Fresh a) = Modified (f a)
  apply (Modified f) (Modified a) = Modified (f a)

instance applicativeValidated :: Applicative Validated where
  pure = Fresh

instance extendValidated :: Extend Validated where
  extend f a = f a <$ a

instance comonadValidated :: Comonad Validated where
  extract (Fresh a) = a
  extract (Modified a) = a

-- | Lenses
_Validated :: forall a b. Lens (Validated a) (Validated b) a b
_Validated =
  flip lens ($>) case _ of
    Fresh a -> a
    Modified a -> a

_Fresh :: forall a. Prism' (Validated a) a
_Fresh =
  prism' Fresh case _ of
    Fresh a -> Just a
    _ -> Nothing

_Modified :: forall a. Prism' (Validated a) a
_Modified =
  prism' Fresh case _ of
    Modified a -> Just a
    _ -> Nothing

-- | Transformation
newtype ModifyValidated
  = ModifyValidated (Validated ~> Validated)

instance modifyValidated :: Mapping ModifyValidated a a => Mapping ModifyValidated (Validated a) (Validated a) where
  mapping m@(ModifyValidated f) = over _Validated (mapping m) <<< f
else instance modifyValidatedRecord :: (RL.RowToList r xs, MapRecordWithIndex xs (ConstMapping ModifyValidated) r r) => Mapping ModifyValidated { | r } { | r } where
  mapping d = hmap d
else instance modifyValidatedArray :: Mapping ModifyValidated a a => Mapping ModifyValidated (Array a) (Array a) where
  mapping d = map (mapping d)
else instance modifyValidatedIdentity :: Mapping ModifyValidated a a where
  mapping _ = identity

setFresh :: forall i o. HMap ModifyValidated i o => i -> o
setFresh = hmap (ModifyValidated (Fresh <<< view _Validated))

setModified :: forall i o. HMap ModifyValidated i o => i -> o
setModified = hmap (ModifyValidated (Modified <<< view _Validated))

-- | Helpers
validate ::
  forall value err err' result.
  Monoid err' =>
  (err -> err') ->
  (value -> V err result) ->
  Validated value ->
  V err' result
validate invalidF validateF input =
  if L.is _Modified input then
    lmap invalidF (validateF value)
  else
    lmap (const mempty) (validateF value)
  where
  value = L.view _Validated input

toRecord ::
  forall err errs.
  Monoid { | errs } =>
  Lens' { | errs } err ->
  err ->
  { | errs }
toRecord lens err = L.set lens err (mempty :: { | errs })

-- | String validators
validateNonEmpty :: String -> V (Array String) String
validateNonEmpty input
  | String.null input = V.invalid $ pure "is required"
  | otherwise = V $ pure input

validateEmailFormat :: String -> V (Array String) String
validateEmailFormat input
  | not $ isValidEmail input = V.invalid $ pure "is invalid"
  | otherwise = V $ pure input

isValidEmail :: String -> Boolean
isValidEmail = Regex.test emailRegex
  where
  emailRegex = unsafePartial $ fromRight $ Regex.regex """^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$""" Flags.global

validateMinimumLength :: Int -> String -> V (Array String) String
validateMinimumLength validLength input
  | String.length input <= validLength = V.invalid $ pure "is too short"
  | otherwise = V $ pure input

validateMaximunLength :: Int -> String -> V (Array String) String
validateMaximunLength validLength input
  | String.length input > validLength = V.invalid $ pure "is too long"
  | otherwise = V $ pure input
