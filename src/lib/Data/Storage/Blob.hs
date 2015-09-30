module Data.Storage.Blob
   ( Blob
   , Hash
   , computeHash
   , hashSize
   , encodeHash
   , decodeHash
   )
where

import Data.SafeCopy
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Digest.Pure.SHA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode, decode)

-- | Binary blob
type Blob = BS.ByteString

-- | Hash
newtype Hash = Hash (Digest SHA256State) deriving (Eq)

instance Show Hash where
   show (Hash hash) = showDigest hash

-- | Compute the hash of a binary blob
computeHash :: Blob -> Hash
computeHash = Hash . sha256 . LBS.fromStrict

-- | Size of a Hash in bytes
hashSize :: Int
hashSize = fromIntegral $ LBS.length (encode hash)
   where
      Hash hash = computeHash BS.empty

encodeHash :: Hash -> BS.ByteString
encodeHash (Hash hash) = LBS.toStrict $ encode hash

decodeHash :: BS.ByteString -> Hash
decodeHash = Hash . decode . LBS.fromStrict

instance SafeCopy Hash where
   putCopy hash = contain $ putByteString (encodeHash hash)
   getCopy      = contain $ (decodeHash <$> getByteString hashSize)
   version = 1
   kind = base

