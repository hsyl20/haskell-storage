{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Data.Storage.Storage
   ( Ref
   , retrieveObject
   , storeObject
   , modifyObject
   , tagObjectRef
   , retrieveTag
   , retrieveObjectFromTag
   , markObjectRef
   , retrieveCurrentMarker
   , retrieveMarker
   , retrieveCurrentMarkerObject
   , retrieveMarkerObject
   , deleteMarker
   )
where

import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.SafeCopy
import Data.Serialize.Get
import Data.Serialize.Put

import Data.Storage.Blob
import Data.Storage.DataBase

-- | Reference to another object
newtype Ref a = Ref Hash deriving (Eq)

deriveSafeCopy 1 'base ''Ref

instance Show (Ref a) where
   show (Ref hash) = "ref-" ++ show hash


-- | Read an object from the storage
retrieveObject :: (SafeCopy a, DataBase db) => db -> Ref a -> IO a
retrieveObject db (Ref hash) = do

   retrieveBlob db hash >>= \case
      RetrieveError err  -> error err
      RetrieveSuccess bs -> do
         case runGet safeGet bs of
            Left err -> error (show err)
            Right v  -> return v


-- | Write an object into the storage
storeObject :: (SafeCopy a, DataBase db) => db -> a -> IO (Ref a)
storeObject db obj = do
   let bs = runPut (safePut obj)

   storeBlob db bs >>= \case
      StoreError err    -> error err
      StoreSuccess hash -> return (Ref hash)

-- | Modify an object, store the new one and return its reference
modifyObject :: (DataBase db, SafeCopy a) => db -> (a -> a) -> Ref a -> IO (Ref a)
modifyObject db f ref = do
   obj <- retrieveObject db ref
   storeObject db (f obj)

-- | Tag an object reference
tagObjectRef :: DataBase db => db -> String -> Ref a -> IO ()
tagObjectRef db tag (Ref hash) = do
   res <- setTag db tag hash
   case res of
      SetTagError err -> error err
      SetTagSuccess   -> return ()

-- | Retrieve a tag
retrieveTag :: DataBase db => db -> String -> IO (Maybe (Ref a))
retrieveTag db tag = do
   res <- getTag db tag
   case res of
      GetTagSuccess hash -> return (Just (Ref hash))
      GetTagError _      -> return Nothing

-- | Retrieve the object associated to a tag
retrieveObjectFromTag :: (SafeCopy a, DataBase db) => db -> String -> IO (Maybe a)
retrieveObjectFromTag db tag = do
   ref <- retrieveTag db tag
   traverse (retrieveObject db) ref

-- | Add a marker to an object
markObjectRef :: DataBase db => db -> String -> Ref a -> IO ()
markObjectRef db marker (Ref hash) = do
   time <- round <$> getPOSIXTime
   res <- setMarker db marker time hash
   case res of
      SetMarkerError err -> error err
      SetMarkerSuccess   -> return ()

-- | Retrieve the current marker
retrieveCurrentMarker :: DataBase db => db -> String -> IO (Maybe (Ref a))
retrieveCurrentMarker db marker = do
   time <- getPOSIXTime
   retrieveMarker db marker time

-- | Retrieve the marker at the given time
retrieveMarker :: DataBase db => db -> String -> POSIXTime -> IO (Maybe (Ref a))
retrieveMarker db marker time = do
   res <- getMarker db marker (round time)
   case res of
      GetMarkerSuccess _ hash -> return (Just (Ref hash))
      GetMarkerError _      -> return Nothing

-- | Retrieve the object associated to a marker at the given time
retrieveMarkerObject :: (SafeCopy a, DataBase db) => db -> String -> POSIXTime -> IO (Maybe a)
retrieveMarkerObject db marker time = do
   ref <- retrieveMarker db marker time
   traverse (retrieveObject db) ref

-- | Retrieve the current object associated to a marker
retrieveCurrentMarkerObject :: (SafeCopy a, DataBase db) => db -> String -> IO (Maybe a)
retrieveCurrentMarkerObject db marker = do
   ref <- retrieveCurrentMarker db marker
   traverse (retrieveObject db) ref

-- | Delete a marker
deleteMarker :: DataBase db => db -> String -> IO ()
deleteMarker db marker = do
   time <- round <$> getPOSIXTime
   res <- removeMarker db marker time
   case res of
      SetMarkerError err -> error err
      SetMarkerSuccess   -> return ()
