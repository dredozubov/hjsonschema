{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.JsonSchema.Core where

import           Data.Aeson
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as H
import           Data.JsonSchema.Reference
import           Data.Maybe
import           Data.Text                 (Text)
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           GHC.Generics (Generic)
import           Network.URI

--------------------------------------------------
-- * Primary API
--------------------------------------------------

compile :: Spec -> Graph -> RawSchema -> Schema
compile spec g (RawSchema t o) = Schema . catMaybes . H.elems $ H.intersectionWith f (_unSpec spec) o
  where
    f :: (ValidatorGen, a) -> Value -> Maybe Validator
    f (vGen,_) = vGen spec g $ RawSchema (newResolutionScope t o) o

validate :: Schema -> Value -> Either (Vector ValErr) Value
validate schema x =
  let errs = V.concatMap ($ x) $ V.fromList (_unSchema schema)
  in if V.null errs
    then Right x
    else Left errs

--------------------------------------------------
-- * Types
--------------------------------------------------

-- Missing instances
-- Orphan instances aren't really cool, but 'network-uri' is
-- used in GHC, so it's silly to hope that it will depend on 'hashable'
deriving instance Generic URIAuth

instance Hashable URIAuth

instance Hashable URI

newtype Spec = Spec { _unSpec :: HashMap URI (ValidatorGen, EmbeddedSchemas) }

-- | Set of potentially mutually recursive schemas.
type Graph = HashMap Text (HashMap Text Value)

type ValErr = Text

newtype Schema = Schema { _unSchema :: [Validator] }

type Validator = Value -> Vector ValErr

type ValidatorGen = Spec -> Graph -> RawSchema -> Value -> Maybe Validator

-- | Return a schema's immediate subschemas.
--
-- This is used by 'Data.JsonSchema.fetchRefs' to find all the
-- subschemas in a document. This allows it to process only
-- "$ref"s and "id"s that are actual schema keywords.
type EmbeddedSchemas = Text -> Value -> Vector RawSchema

data RawSchema = RawSchema
  { _rsURI    :: URI
  , _rsObject :: HashMap URI Value
  }
