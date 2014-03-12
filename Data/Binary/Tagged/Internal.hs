{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Data.Binary.Tagged
-- Copyright   : (c) Justin Le 2014,
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : stable
-- Portability : portable
--
-- Internals for the library, exported in case you should need it.
-- Usually, the parts you would need should be re-exported in
-- 'Data.Binary.Tagged'.
--

module Data.Binary.Tagged.Internal (
    -- * Data types
    Tagged          -- abstract, instances: Show, Eq, Binary, Typeable, Generic
  , TagFingerprint  -- abstract, instances: Show, Eq, Ord, Binary, Typeable, Generic, Default
    -- * Tagging and extracting data
  , tag             -- :: Typeable a => a -> Tagged a
  , getTagged       -- :: Typeable a => Tagged a -> Maybe a
  , tagMatched      -- :: Typeable a => Tagged a -> Bool
    -- * 'TagFingerprint' utilities
  , typeFingerprint -- :: Typeable a => a -> TagFingerprint
  , tagFingerprint  -- :: Tagged a -> TagFingerprint
  ) where

import Data.Binary
import Data.Default
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8 as LC
import Data.Maybe             (isJust)
import Data.Typeable.Internal
import GHC.Generics


-- | A data type tupling together data with a 'TagFingerprint',
-- representing data tagged with its type.  You really should never have to
-- use this type; it's best to interface directly with data using
-- 'encodeTagged', 'decodeTagged', etc.  Use 'tag' to tag data and
-- 'extractTagged' to extract data from valid tagged data.
data Tagged a = Tagged !TagFingerprint a
                deriving (Show, Eq, Generic, Typeable)

-- | A data type representing a fingerprint for a 'Typeable' type.
-- Ideally, this would be 'Data.Typeable.Internal''s own 'Fingerprint'
-- types; however, for some reason, the fingerprints for the same data type
-- from the same modules differ between different GHC backends.  So for
-- now, it is just a ByteString representation of the name of the type.
-- This is literally a bad idea...but works for now.
--
-- This type is mostly used for the ability to categorized Tagged items
-- by their type.
newtype TagFingerprint = TagFP ByteString
                         deriving (Show, Typeable, Generic, Eq, Ord)

instance Binary a => Binary (Tagged a)
instance Binary TagFingerprint

instance Default TagFingerprint where
  def = TagFP empty

-- | Wrap data inside a 'Tagged' tuple.
tag :: Typeable a => a -> Tagged a
tag x = Tagged (typeFingerprint x) x

-- | Compute the 'Fingerprint' representing a type.  It is non-strict on
-- its parameter, so passing in undefined should work if you want to just
-- get the 'Fingerprint' of a specific type without having data of that
-- type on hand:
--
-- > typeFingerprint (undefined :: Int)
--
typeFingerprint :: Typeable a => a -> TagFingerprint
typeFingerprint = TagFP . LC.pack . show . typeOf

-- | Extract data out of a 'Tagged', but only the type of the data matches
-- the type represented by the fingerprint.  It is polymorphic on its
-- output and meant to be used when decoding a 'Tagged' item with a desired
-- type.
getTagged :: Typeable a => Tagged a -> Maybe a
getTagged (Tagged tfp x) | tfp == xfp = Just x
                         | otherwise  = Nothing
  where
    xfp = typeFingerprint x

-- | Check if the type inside the 'Tagged' matches the fingerprint.
tagMatched :: Typeable a => Tagged a -> Bool
tagMatched = isJust . getTagged

-- | Extract the 'Fingerprint' out of a 'Tagged'.  Mostly used so that you
-- can categorize and associate Tagged items; to check if a 'Tagged' is
-- of a desired typed, 'getTagged' and 'tagMatched' might be more useful.
tagFingerprint :: Tagged a -> TagFingerprint
tagFingerprint (Tagged fp _) = fp

