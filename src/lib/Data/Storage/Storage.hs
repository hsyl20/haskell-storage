{-# LANGUAGE TemplateHaskell #-}
module Data.Storage.Storage
   ( ObjectHash
   , Ref
   , Storage
   , initStorage
   , readObject
   , writeObject
   , modifyObject
   , withStorage
   , (-->)
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
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Int
import qualified Control.Category as C

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

type S a = StateT Storage IO a

withStorage :: Storage -> S a -> IO a
withStorage s st = evalStateT st s


(-->) :: SafeCopy a => S b -> (b -> Ref a) -> S a
(-->) b f = do
   s <- get
   b' <- b
   lift $ readObject s (f b')


data Store m s a = Store (s -> m a) (m s)
data MonadicLens m s a = MonadicLens (s -> Store m a s)

refLens :: SafeCopy a => MonadicLens (StateT Storage IO) (Ref a) a
refLens = MonadicLens (\ref -> Store (putObj ref) (getObj ref))
   where
      putObj ref obj = do
         s <- get
         lift $ writeObject s obj
      getObj ref = do
         s <- get
         lift $ readObject s ref


instance Monad m => C.Category (MonadicLens m) where
   id = MonadicLens (\s -> Store (const (return s)) (return s))

   (.) (MonadicLens f) (MonadicLens g) = ret
         where
            ret = MonadicLens (\a -> Store (pt a) (gt a)) -- :: MonadicLens m a c

            --gt :: a -> m c
            gt a = do
               let (Store _ gb) = g a
               b <- gb
               let (Store _ gc) = f b
               gc

            --pt :: a -> c -> m a
            pt a c = do
               let (Store pb gb) = g a
               b <- gb
               let (Store pc gc) = f b
               b' <- pc c
               pb b'
