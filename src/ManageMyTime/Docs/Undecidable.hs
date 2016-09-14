{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module ManageMyTime.Docs.Undecidable where

import Servant.Docs (ToSample, ToCapture(..), DocCapture(..), singleSample, toSamples)
import Servant.API (Capture)
import           Database.Persist.Sql     (ToBackendKey, SqlBackend, Key, toSqlKey)

instance (ToBackendKey SqlBackend a) => ToSample (Key a) where
    toSamples _ = singleSample $ toSqlKey 1

instance (ToBackendKey SqlBackend a) => ToCapture (Capture "id" (Key a)) where
    toCapture _ =
        DocCapture "id"
            "id (integer) of the resource to access"
