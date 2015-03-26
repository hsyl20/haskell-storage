{-# LANGUAGE TemplateHaskell #-}
module Data.Storage.Storage
   ( ObjectHash
   , Ref
   , Storage
   , initStorage
   , readObject
   , writeObject
   , modifyObject
   )
where

import Data.Digest.Pure.SHA
import Data.SafeCopy
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Binary (encode,decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.FilePath
import System.Directory
import Control.Monad (when)
import Control.Applicative ((<$>))
import Data.Int

-- | Object identifier
newtype ObjectHash = ObjectHash (Digest SHA256State) deriving (Eq)

makeHash :: BS.ByteString -> ObjectHash
makeHash = ObjectHash . sha256 . LBS.fromStrict

sizeOfHash :: Int64
sizeOfHash = LBS.length (encode hash)
   where
      ObjectHash hash = makeHash BS.empty

instance SafeCopy ObjectHash where
   putCopy (ObjectHash hash) = contain $ putLazyByteString (encode hash)
   getCopy                   = contain $ (ObjectHash . decode <$> getLazyByteString sizeOfHash)
   version = 1
   kind = base

-- | Reference to another object
newtype Ref a = Ref ObjectHash deriving (Eq)

deriveSafeCopy 1 'base ''Ref

instance Show (Ref a) where
   show (Ref (ObjectHash hash)) = "ref-" ++ showDigest hash

-- | A storage
data Storage = Storage
   { storagePath :: FilePath
   }

-- | Initialize a storage
initStorage :: FilePath -> IO Storage
initStorage path = do
   createDirectoryIfMissing True path
   createDirectoryIfMissing True (path </> "objects")
   return (Storage path)


-- | Compute the object path
computePath :: ObjectHash -> FilePath
computePath (ObjectHash hash) = "objects" </> showDigest hash


-- | Read an object from the storage
readObject :: SafeCopy a => Storage -> Ref a -> IO a
readObject storage (Ref hash) = do

   bs <- BS.readFile (storagePath storage </> computePath hash)

   when (makeHash bs /= hash) $
      error "readObject: file has been altered (invalid hash)"

   case runGet safeGet bs of
      Left err -> error (show err)
      Right v  -> return v


-- | Write an object into the storage
writeObject :: SafeCopy a => Storage -> a -> IO (Ref a)
writeObject storage obj = do

   let 
      bs   = runPut (safePut obj)
      hash = makeHash bs

   BS.writeFile (storagePath storage </> computePath hash) bs

   return (Ref hash)

-- | Modify an object, store the new one and return its reference
modifyObject :: SafeCopy a => Storage -> (a -> a) -> Ref a -> IO (Ref a)
modifyObject storage f ref = do
   obj <- readObject storage ref
   writeObject storage (f obj)
