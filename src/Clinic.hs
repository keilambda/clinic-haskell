{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Clinic (main) where

import Clinic.API (app)
import Clinic.Database (migrate')

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))

import Data.Kind (Type)

import Database.Persist.Postgresql (createPostgresqlPool)

import Network.Wai.Handler.Warp (run)

import Options.Generic

import Data.ByteString (ByteString)

type Opts :: Type -> Type
data Opts w = MkOpts
  { port :: !(w ::: Int <?> "Port to listen on" <!> "8080" <#> "p")
  , dbUrl :: !(w ::: ByteString <?> "Database connection string" <#> "u")
  , dbPoolSize :: !(w ::: Int <?> "Database pool size" <!> "10" <#> "s")
  }
  deriving stock (Generic)

deriving anyclass instance ParseRecord (Opts Wrapped)
deriving stock instance Show (Opts Unwrapped)

main :: IO ()
main = do
  (opts :: Opts Unwrapped) <- unwrapRecord "Clinic API"
  pool <- runNoLoggingT $ createPostgresqlPool opts.dbUrl opts.dbPoolSize
  let ?pool = pool
  migrate'
  run opts.port app
