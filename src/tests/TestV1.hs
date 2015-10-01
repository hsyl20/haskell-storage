{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Data.Storage.Storage
import Data.Storage.FileDB
import Data.SafeCopy
import Data.Word

data Gender = Male | Female deriving (Show,Eq)

deriveSafeCopy 1 'base ''Gender

data Person = Person
   { personAge    :: Word8
   , personName   :: String
   , personGender :: Gender
   } deriving (Show)

deriveSafeCopy 1 'base ''Person

data Office = Office
   { officeName   :: String
   , officeBoss   :: Ref Person
   , officeWorker :: Ref Person
   } deriving (Show)

deriveSafeCopy 1 'base ''Office


main :: IO ()
main = do
   db <- initFileDB "officestore"

   -- try to find the current office
   retrieveObjectFromCurrentMark db "current" >>= \case
      Just office -> do
         putStrLn $ "Found current office with name " ++ officeName office ++ "!"
         boss   <- retrieveObject db (officeBoss office)
         putStrLn $ "  - Office boss: " ++ show boss
         worker <- retrieveObject db (officeWorker office)
         putStrLn $ "  - Office worker: " ++ show worker

      Nothing     -> do
         putStrLn "We create a new office! Restart the app the see it"

         -- create a office with a boss and a single employee
         let boss = Person 40 "Michael" Male
         bossRef <- storeObject db boss

         let worker = Person 34 "Jim" Male
         workerRef <- storeObject db worker

         let office = Office "Scranton" bossRef workerRef
         officeRef <- storeObject db office

         -- put a marker on the office
         markObjectRef db "current" officeRef
