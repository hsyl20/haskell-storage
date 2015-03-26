{-# LANGUAGE TemplateHaskell #-}

import Data.Storage.Storage
import Data.SafeCopy
import Data.Word
import Control.Applicative (pure)

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

   let obj5 = MyRefObject2 5 ref2
   putStrLn (show obj5)
   ref3 <- writeObject s obj5
   putStrLn ("Reference: " ++ show ref3)
   obj6 <- readObject s ref3
   putStrLn (show obj6)

   r <- withStorage s $ do
      pure obj6 --> field5 --> field3

   putStrLn (show r)
