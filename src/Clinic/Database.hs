{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Clinic.Database (
  Sex (..),
  Patient (..),
  PatientId,
  PoolSql,
  migrate',
  dbPatientGetAll,
  dbPatientGetOne,
  dbPatientAdd,
  dbPatientUpdate,
  dbPatientDelete,
)
where

import Control.Monad.Reader (ReaderT, runReaderT)

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Constraint, Type)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Esqueleto.Experimental
import Database.Persist.TH

import GHC.Generics (Generic)

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

type PoolSql :: Constraint
type PoolSql = (?pool :: Pool SqlBackend)

withPool :: (?pool :: Pool s) => ReaderT s IO r -> IO r
withPool = withResource ?pool . runReaderT

dbPatientGetAll :: PoolSql => IO [Patient]
dbPatientGetAll = map entityVal <$> withPool (select $ from table)

dbPatientGetOne :: PoolSql => PatientId -> IO (Maybe Patient)
dbPatientGetOne _id = fmap entityVal <$> withPool do
  selectOne do
    p <- from table
    where_ (p ^. PatientId ==. val _id)
    pure p

dbPatientAdd :: PoolSql => Patient -> IO PatientId
dbPatientAdd = withPool . insert

dbPatientUpdate :: PoolSql => PatientId -> Patient -> IO ()
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

dbPatientDelete :: PoolSql => PatientId -> IO ()
dbPatientDelete _id = withPool do
  delete do
    p <- from table
    where_ (p ^. PatientId ==. val _id)

migrate' :: PoolSql => IO ()
migrate' = withPool $ runMigration migrateAll
