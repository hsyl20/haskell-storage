{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Storage.Storage
import Data.Storage.FileDB
import Data.SafeCopy
import Data.Word
import Data.Foldable (traverse_)

data Gender = Male | Female deriving (Show,Eq)

deriveSafeCopy 1 'base ''Gender

data Person = Person
   { personAge    :: Word8
   , personName   :: String
   , personGender :: Gender
   } deriving (Show)

deriveSafeCopy 1 'base ''Person

-------------------------------------------------------------
-- Our office is getting larger! We can have several workers now!
--
-- We keep the old structure to be able to update from the old database!
data Office_V1 = Office_V1
   { officeV1Name   :: String
   , officeV1Boss   :: Ref Person
   , officeV1Worker :: Ref Person
   } deriving (Show)

deriveSafeCopy 1 'base ''Office_V1

-- Here is our new data structure
data Office = Office
   { officeName    :: String
   , officeBoss    :: Ref Person
   , officeWorkers :: [Ref Person]
   } deriving (Show)

-- It is now an extension of the old one
deriveSafeCopy 2 'extension ''Office

-- And here is how we convert between the old one and the new one
instance Migrate Office where
   type MigrateFrom Office = Office_V1
   migrate old = Office (officeV1Name old) (officeV1Boss old) [officeV1Worker old]

-------------------------------------------------------------


main :: IO ()
main = do
   db <- initFileDB "officestore"

   -- try to find the current office
   retrieveObjectFromCurrentMark db "current" >>= \case
      Just office -> do
         putStrLn $ "Found current office with name " ++ officeName office ++ "!"
         boss   <- retrieveObject db (officeBoss office)
         putStrLn $ "  - Office boss: " ++ show boss
         -----------------------------------------------
         putStrLn $ "  - Office workers: "
         workers <- traverse (retrieveObject db) (officeWorkers office)
         traverse_ (\w -> putStrLn $ "      * " ++ show w) workers
         -----------------------------------------------

      Nothing     -> do
         putStrLn "We create a new office! Restart the app the see it"

         -- create a office with a boss and a several employee
         let boss = Person 40 "Michael" Male
         bossRef <- storeObject db boss

         -----------------------------------------------------------------
         let workers = [Person 34 "Jim" Male, Person 32 "Pam" Female, Person 36 "Dwight" Male]
         workerRefs <- traverse (storeObject db) workers

         let office = Office "Scranton" bossRef workerRefs
         -----------------------------------------------------------------
         officeRef <- storeObject db office

         -- put a marker on the office
         markObjectRef db "current" officeRef
