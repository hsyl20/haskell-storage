{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Storage.FileDB
   ( FileDB
   , initFileDB
   )
where

import Control.Exception (catch, IOException)
import Foreign.Storable (sizeOf)
import System.IO
import System.FilePath
import System.Directory
import Control.Exception (bracket)
import qualified Data.ByteString as BS

import Data.Storage.Blob
import Data.Storage.DataBase

data FileDB = FileDB
   { dbPath :: FilePath
   }

instance DataBase FileDB where
   storeBlob      = fdbStoreBlob
   retrieveBlob   = fdbRetrieveBlob
   setTag         = fdbSetTag
   getTag         = fdbGetTag
   setMarker      = fdbSetMarker
   removeMarker   = fdbRemoveMarker
   getMarker      = fdbGetMarker

-- | Initialize a file database
initFileDB :: FilePath -> IO FileDB
initFileDB path = do
   createDirectoryIfMissing True path
   createDirectoryIfMissing True (path </> "objects")
   createDirectoryIfMissing True (path </> "tags")
   createDirectoryIfMissing True (path </> "markers")
   return (FileDB path)

-- | Compute the object path
computeObjectPath :: Hash -> FilePath
computeObjectPath hash = "objects" </> show hash

-- | Write a 'ByteString' to a file if the file does not exist
writeNewFile :: FilePath -> BS.ByteString -> IO Bool
writeNewFile f bs = bracket (openBinaryFile f WriteMode) hClose
    (\h -> hFileSize h >>= \case
         0 -> BS.hPut h bs >> return True
         _ -> return False)

fdbStoreBlob :: FileDB -> Blob -> IO (StoreResult Hash)
fdbStoreBlob db bs = do
   let hash = computeHash bs
   BS.writeFile (dbPath db </> computeObjectPath hash) bs
   return (StoreSuccess hash)

fdbRetrieveBlob :: FileDB -> Hash -> IO (RetrieveResult Blob)
fdbRetrieveBlob db hash = do
   bs <- BS.readFile (dbPath db </> computeObjectPath hash)
   return (RetrieveSuccess bs)

-- | Set an immutable tag
fdbSetTag :: FileDB -> String -> Hash -> IO SetTagResult
fdbSetTag db tag hash = do
   ret <- writeNewFile (dbPath db </> "tags" </> tag) (encodeHash hash)
   return $ if ret
      then SetTagSuccess
      else SetTagError "Tag already exists"

-- | Get an immutable tag
fdbGetTag :: FileDB -> String -> IO GetTagResult
fdbGetTag db tag = do
   catch (do
      hash <- decodeHash <$> BS.readFile (dbPath db </> "tags" </> tag)
      return $ GetTagSuccess hash)
      (\(_ :: IOException) ->  return $ GetTagError ("Tag \"" ++ tag ++ "\" not found"))


-- | Set a marker
fdbSetMarker :: FileDB -> String -> DateTime -> Hash -> IO SetMarkerResult
fdbSetMarker db marker date hash = do
   let bs = BS.append (encodeDate date) (encodeHash hash)
   BS.appendFile (dbPath db </> "markers" </> marker) bs
   return SetMarkerSuccess

emptyHash :: Blob
emptyHash = BS.pack (replicate (fromIntegral hashSize) 0)

-- | Remove a marker
fdbRemoveMarker :: FileDB -> String -> DateTime -> IO SetMarkerResult
fdbRemoveMarker db marker date = do
   let bs = BS.append (encodeDate date) emptyHash
   BS.appendFile (dbPath db </> "markers" </> marker) bs
   return SetMarkerSuccess

-- | Get a marker
fdbGetMarker :: FileDB -> String -> DateTime -> IO GetMarkerResult
fdbGetMarker db marker date =
   bracket (openBinaryFile f ReadMode) hClose (readLoop 0)

   where 
      f = (dbPath db </> "markers" </> marker)

      -- size of the date (timestamp)
      dateSize = sizeOf (undefined :: DateTime)

      -- size of the whole record (date + hash)
      recSize  = dateSize + hashSize

      -- try to read the marker, starting from the end of the file
      readLoop n h = do
         hSeek h SeekFromEnd n
         markerDate <- decodeDate <$> BS.hGet h (fromIntegral dateSize)
         if markerDate > date
            then readLoop (n + fromIntegral recSize + fromIntegral dateSize) h
            else do
               markerHashRaw <- BS.hGet h (fromIntegral hashSize)
               if markerHashRaw == emptyHash
                  then return $ GetMarkerError "Marker not found (deleted)"
                  else return $ GetMarkerSuccess markerDate (decodeHash markerHashRaw)

