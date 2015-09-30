{-# LANGUAGE TemplateHaskell #-}

import Data.Storage.Storage
import Data.Storage.FileDB
import Data.Storage.DataBase
import Data.SafeCopy
import Data.Word

data MyObject = MyObject
   { field0 :: Word64
   , field1 :: Word64
   } deriving (Show)

deriveSafeCopy 1 'base ''MyObject

data MyRefObject = MyRefObject
   { field2 :: Int
   , field3 :: Ref MyObject
   } deriving (Show)

deriveSafeCopy 1 'base ''MyRefObject

data MyRefObject2 = MyRefObject2
   { field4 :: Int
   , field5 :: Ref MyRefObject
   } deriving (Show)

deriveSafeCopy 1 'base ''MyRefObject2



main :: IO ()
main = do
   db <- initFileDB "mystore"

   
   tag <- getTag db "root"
   case tag of
      GetTagSuccess hash -> putStrLn ("Tag root corresponds to: " ++ show hash)
      GetTagError err    -> putStrLn ("Error while getting tag: " ++ err)

   let obj = MyObject 14 18
   ref <- storeObject db obj
   putStrLn ("Reference: " ++ show ref)
   obj2 <- retrieveObject db ref
   putStrLn (show obj2)

   let obj3 = MyRefObject 14 ref
   putStrLn (show obj3)
   ref2 <- storeObject db obj3
   putStrLn ("Reference: " ++ show ref2)
   obj4 <- retrieveObject db ref2
   putStrLn (show obj4)

   tagObjectRef db "root" ref2

   let obj5 = MyRefObject2 5 ref2
   putStrLn (show obj5)
   ref3 <- storeObject db obj5
   putStrLn ("Reference: " ++ show ref3)
   obj6 <- retrieveObject db ref3
   putStrLn (show obj6)
