module Data.Storage.Storage
   ( Object(..)
   , ObjectHash(..)
   )
where

import Data.Digest.Pure.SHA

-- | Object identifier
newtype ObjectHash = ObjectHash SHA256State


-- | A stored object
data Object = Object
   { objectHash :: ObjectHash
   }

