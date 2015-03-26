{-# LANGUAGE TemplateHaskell #-}

import Data.Storage.Storage
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

main :: IO ()
main = do
   s <- initStorage "mystore"

   let obj = MyObject 14 18
   ref <- writeObject s obj
   putStrLn ("Reference: " ++ show ref)
   obj2 <- readObject s ref
   putStrLn (show obj2)

   let obj3 = MyRefObject 14 ref
   putStrLn (show obj3)
   ref2 <- writeObject s obj3
   putStrLn ("Reference: " ++ show ref2)
   obj4 <- readObject s ref2
   putStrLn (show obj4)
