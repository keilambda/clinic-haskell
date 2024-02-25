{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Clinic (module Clinic) where

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (ReaderT (runReaderT))

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Pool (Pool, withResource)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.TH

import Network.Wai.Handler.Warp (run)

import Servant (Application)
import Servant.API
import Servant.API.Generic (Generic)
import Servant.Server.Generic (AsServerT, genericServeT)

import UnliftIO (MonadIO (..))

type Sex :: Type
data Sex = Male | Female
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

derivePersistField "Sex"

share
  [mkPersist sqlSettings{mpsPrefixFields = False}, mkMigrate "migrateAll"]
  [persistLowerCase|
Patient
  firstName Text
  middleName Text
  lastName Text
  sex Sex
  birthDate UTCTime
  address Text
  insurance Text
|]

type Patient :: Type
type PatientId :: Type

deriving stock instance Show Patient
deriving stock instance Generic Patient
deriving anyclass instance ToJSON Patient
deriving anyclass instance FromJSON Patient

withPool :: (?pool :: Pool s) => ReaderT s IO r -> IO r
withPool = withResource ?pool . runReaderT

dbPatientGetAll :: (?pool :: Pool SqlBackend) => IO [Entity Patient]
dbPatientGetAll = withPool $ select $ from table

dbPatientGetOne :: (?pool :: Pool SqlBackend) => PatientId -> IO (Maybe (Entity Patient))
dbPatientGetOne _id = withPool do
  selectOne do
    p <- from table
    where_ (p ^. PatientId ==. val _id)
    pure p

dbPatientAdd :: (?pool :: Pool SqlBackend) => Patient -> IO PatientId
dbPatientAdd = withPool . insert

dbPatientUpdate :: (?pool :: Pool SqlBackend) => PatientId -> Patient -> IO ()
dbPatientUpdate _id pt = withPool do
  update \p -> do
    set
      p
      [ FirstName =. val pt.firstName
      , MiddleName =. val pt.middleName
      , LastName =. val pt.lastName
      , Sex =. val pt.sex
      , BirthDate =. val pt.birthDate
      , Address =. val pt.address
      , Insurance =. val pt.insurance
      ]
    where_ (p ^. PatientId ==. val _id)

dbPatientDelete :: (?pool :: Pool SqlBackend) => PatientId -> IO ()
dbPatientDelete _id = withPool do
  delete do
    p <- from table
    where_ (p ^. PatientId ==. val _id)

migrate' :: (?pool :: Pool SqlBackend) => IO ()
migrate' = withPool $ runMigration migrateAll

type PatientRoutes :: Type -> Type
data PatientRoutes route = MkPatientRoutes
  { _getAll :: route :- Get '[JSON] [Patient]
  , _getOne :: route :- Capture "id" PatientId :> Get '[JSON] (Maybe Patient)
  , _addOne :: route :- ReqBody '[JSON] Patient :> Post '[JSON] PatientId
  , _update :: route :- Capture "id" PatientId :> ReqBody '[JSON] Patient :> Put '[JSON] ()
  , _delete :: route :- Capture "id" PatientId :> Delete '[JSON] ()
  }
  deriving stock (Generic)

api :: Proxy (ToServantApi PatientRoutes)
api = genericApi (Proxy :: Proxy PatientRoutes)

handlers :: (?pool :: Pool SqlBackend) => PatientRoutes (AsServerT IO)
handlers =
  MkPatientRoutes
    { _getAll = map entityVal <$> dbPatientGetAll
    , _getOne = fmap (fmap entityVal) . dbPatientGetOne
    , _addOne = dbPatientAdd
    , _update = dbPatientUpdate
    , _delete = dbPatientDelete
    }

app :: (?pool :: Pool SqlBackend) => Application
app = genericServeT liftIO handlers

main :: IO ()
main = do
  pool <- runNoLoggingT $ createPostgresqlPool "" 10
  let ?pool = pool
  migrate'
  dbPatientGetAll >>= print
  run 3030 app
