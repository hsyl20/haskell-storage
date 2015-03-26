import Data.Storage.Storage


main :: IO ()
main = do
   putStrLn "test"

data MyObject = MyObject
   { field0 :: Word64
   , field1 :: Word64
   }

data MyObject2 = MyObject2
   { field2 :: Int
   , field3 :: Ref MyObject
   }
