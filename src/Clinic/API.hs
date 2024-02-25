{-# LANGUAGE DataKinds #-}

module Clinic.API (
  PatientRoutes,
  api,
  app,
  handlers,
) where

import Clinic.Database (Patient, PatientId, PoolSql)
import Clinic.Database qualified as DB

import Data.Kind (Type)
import Data.Proxy (Proxy (..))

import Servant (Application)
import Servant.API
import Servant.API.Generic (Generic)
import Servant.Server.Generic (AsServerT, genericServeT)

import UnliftIO (MonadIO (..))

type PatientRoutes :: Type -> Type
data PatientRoutes route = MkPatientRoutes
  { _getAll :: route :- Get '[JSON] [Patient]
  , _getOne :: route :- Capture "id" PatientId :> Get '[JSON] (Maybe Patient)
  , _create :: route :- ReqBody '[JSON] Patient :> Post '[JSON] PatientId
  , _update :: route :- Capture "id" PatientId :> ReqBody '[JSON] Patient :> Put '[JSON] ()
  , _delete :: route :- Capture "id" PatientId :> Delete '[JSON] ()
  }
  deriving stock (Generic)

api :: Proxy (ToServantApi PatientRoutes)
api = genericApi (Proxy :: Proxy PatientRoutes)

handlers :: (PoolSql) => PatientRoutes (AsServerT IO)
handlers =
  MkPatientRoutes
    { _getAll = DB.patientGetAll
    , _getOne = DB.patientGetOne
    , _create = DB.patientCreate
    , _update = DB.patientUpdate
    , _delete = DB.patientDelete
    }

app :: (PoolSql) => Application
app = genericServeT liftIO handlers
