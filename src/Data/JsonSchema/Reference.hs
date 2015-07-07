{-# LANGUAGE ViewPatterns #-}

module Data.JsonSchema.Reference
       ( Reference(..)
       , mkAbsoluteRef
       , mkRelativeRef
       , newResolutionScope
       , refAndPointer
       , fetchRef
       , safeGet
       ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Client
import           Network.URI
import           Prelude              hiding (foldr)

-- | Resolved reference from supplied ref.
-- Don't use it directly, smart constructor must be used
data Reference = Reference URI

instance Show Reference where
  show (Reference uri) = show uri

mkAbsoluteRef :: Text -> Maybe Reference
mkAbsoluteRef = fmap Reference . parseAbsoluteURI . T.unpack

mkRelativeRef :: Reference -> Text -> Maybe Reference
mkRelativeRef (Reference base) t@(T.unpack -> str)
  | isURIReference str = let parsed = parseRelativeReference str
                             merged = (`relativeTo` base) <$> parsed
                         in Reference <$> merged
  | isAbsoluteURI  str = mkAbsoluteRef t
  | otherwise          = Nothing

newResolutionScope :: Text -> HashMap Text Value -> Maybe Reference
newResolutionScope t (H.lookup "id" -> id') =
  case id' of
    Just (String idKeyword) -> flip mkRelativeRef idKeyword =<< parent
    _                       -> parent
  where parent = mkAbsoluteRef t

-- | Split 'URI' 'Reference' by the fragment separator.
refAndPointer :: Reference -> Maybe (Text, Text)
-- FIXME: still hellish enough.
refAndPointer (Reference uri) = getParts
                              . T.splitOn "#"
                              . T.pack
                              . show $ uri
  where getParts :: [Text] -> Maybe (Text, Text)
        getParts []    = Just ("","")
        getParts [x]   = Just (x,"")
        getParts [x,y] = Just (x,y)
        getParts _     = Nothing

fetchRef :: Text -> IO (Either Text (HashMap Text Value))
fetchRef t = do
  eResp <- safeGet t
  case eResp of
    Left e  -> return $ Left e
    Right b -> return . left T.pack $ eitherDecode b

safeGet :: Text -> IO (Either Text ByteString)
safeGet url = catch (Right <$> simpleHttp') handler
  where
    handler :: SomeException -> IO (Either Text ByteString)
    handler e = return . Left . T.pack . show $ e

    -- We don't want to depend on http-conduit, but Network.Http.Conduit.simpleHttp
    -- is the model for this function. simpleHttp also sets "Connection: close".
    simpleHttp' :: IO ByteString
    simpleHttp' = fmap responseBody $ withManager defaultManagerSettings $ \man -> do
      req <- parseUrl (T.unpack url)
      httpLbs req { requestHeaders = ("Connection", "close") : requestHeaders req } man
