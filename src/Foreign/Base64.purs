module Foreign.Base64
  ( atob
  , btoa
  ) where

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect.Exception (Error)

atob :: String -> Either Error String
atob str = runFn3 _atob Left Right str

foreign import _atob :: Fn3 (forall x y. x -> Either x y) (forall x y. y -> Either x y) String (Either Error String)

btoa :: String -> Either Error String
btoa str = runFn3 _btoa Left Right str

foreign import _btoa :: Fn3 (forall x y. x -> Either x y) (forall x y. y -> Either x y) String (Either Error String)
