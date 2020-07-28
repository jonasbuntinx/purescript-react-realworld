module Conduit.Effects.LocalStorage where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, decodeJSON, encodeJSON)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

newtype StorageKey a
  = StorageKey String

instance eqStorageKey :: Eq (StorageKey a) where
  eq (StorageKey a) (StorageKey b) = a == b

read :: forall a. Decode a => StorageKey a -> Effect (Maybe a)
read (StorageKey key) = do
  localStorage <- Window.localStorage =<< window
  item <- Storage.getItem key localStorage
  pure $ (hush <<< runExcept <<< decodeJSON) =<< item

write :: forall a. Encode a => StorageKey a -> a -> Effect Unit
write (StorageKey key) value = do
  localStorage <- Window.localStorage =<< window
  Storage.setItem key (encodeJSON value) localStorage

delete :: forall a. StorageKey a -> Effect Unit
delete (StorageKey key) = do
  localStorage <- Window.localStorage =<< window
  Storage.removeItem key localStorage

deleteAll :: Effect Unit
deleteAll = do
  localStorage <- Window.localStorage =<< window
  Storage.clear localStorage
