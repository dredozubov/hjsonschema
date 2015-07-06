-- | TODO: The code handling resolution scope updates
-- is hacky and needs improvement.

module Data.JsonSchema.Reference where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Client
import           Network.URI
import           Prelude              hiding (foldr)

combineIdAndRef :: Text -> Text -> Text
combineIdAndRef a b
  | "://" `T.isInfixOf` b              = b
  | T.length a < 1 || T.length b < 1   = a <> b
  | T.last a == '#' && T.head b == '#' = a <> T.tail b
  | otherwise                          = a <> b

newResolutionScope :: URI -> HashMap URI Value -> URI
newResolutionScope t o = undefined
  -- case H.lookup "id" o of
  --   Just (String idKeyword) -> t `combineIds` idKeyword
  --   _                       -> t

refAndPointer :: Text -> Maybe (Text, Text)
refAndPointer val = getParts $ T.splitOn "#" val
  where
    getParts :: [Text] -> Maybe (Text, Text)
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
