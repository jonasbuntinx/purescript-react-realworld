module Conduit.Data.Serverless where

import Foreign (Foreign)

type Event
  = { path :: String
    }

type Context
  = Foreign

type Response
  = { body :: String
    , statusCode :: Int
    }
