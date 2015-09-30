module Data.Storage.DataBase
   ( DataBase (..)
   , StoreResult (..)
   , RetrieveResult (..)
   , SetTagResult (..)
   , GetTagResult (..)
   , SetMarkerResult (..)
   , GetMarkerResult (..)
   , DateTime
   , encodeDate
   , decodeDate
   )
where

import Data.Storage.Blob

import Data.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode, decode)

type DateTime = Int64

encodeDate :: DateTime -> BS.ByteString
encodeDate = LBS.toStrict . encode

decodeDate :: BS.ByteString -> DateTime
decodeDate = decode . LBS.fromStrict

class DataBase a where
   -- | Store a blob in the database
   storeBlob    :: a -> Blob -> IO (StoreResult Hash)

   -- | Retrieve a blob from the database
   retrieveBlob :: a -> Hash -> IO (RetrieveResult Blob)

   -- | Set an immutable tag
   setTag       :: a -> String -> Hash -> IO SetTagResult

   -- | Get an immutable tag
   getTag       :: a -> String -> IO GetTagResult

   -- | Set a marker
   setMarker    :: a -> String -> DateTime -> Hash -> IO SetMarkerResult

   -- | Remove a marker
   removeMarker :: a -> String -> DateTime -> IO SetMarkerResult

   -- | Get a marker
   getMarker    :: a -> String -> DateTime -> IO GetMarkerResult


data StoreResult a
   = StoreSuccess a
   | StoreError String
   deriving (Show,Eq)

data RetrieveResult a
   = RetrieveSuccess a
   | RetrieveError String
   deriving (Show,Eq)

data SetTagResult
   = SetTagSuccess
   | SetTagError String
   deriving (Show,Eq)

data GetTagResult
   = GetTagSuccess Hash
   | GetTagError String
   deriving (Show,Eq)

data SetMarkerResult
   = SetMarkerSuccess
   | SetMarkerError String
   deriving (Show,Eq)

data GetMarkerResult
   = GetMarkerSuccess DateTime Hash
   | GetMarkerError String
   deriving (Show,Eq)

