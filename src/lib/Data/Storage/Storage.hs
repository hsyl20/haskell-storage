module Data.Storage.Storage
   ( ObjectHash
   , Storage
   , initStorage
   , readObject
   )
where

import Data.Digest.Pure.SHA
import Data.SafeCopy
import Data.Serialize.Get
import qualified Data.ByteString as BS
import System.FilePath

-- | Object identifier
newtype ObjectHash = ObjectHash (Digest SHA256State)

-- | Reference to another object
newtype Ref a = Ref ObjectHash


data Storage = Storage
   { storagePath :: FilePath
   }


initStorage :: FilePath -> IO Storage
initStorage path = return (Storage path)

computePath :: ObjectHash -> FilePath
computePath (ObjectHash hash) = "objects" </> showDigest hash

readObject :: SafeCopy a => Storage -> Ref a -> IO a
readObject storage (Ref hash) = do

   bs <- BS.readFile (storagePath storage </> computePath hash)

   case runGet safeGet bs of
      Left err -> error (show err)
      Right v  -> return v
