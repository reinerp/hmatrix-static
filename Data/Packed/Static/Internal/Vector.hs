-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Internal.Vector
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Vector newtype definition.
--
-----------------------------------------------------------------------------


module Data.Packed.Static.Internal.Vector(
   Vector(..),
 ) where

import qualified Numeric.LinearAlgebra as H

-- | A vector with elements of type @t@ and length @n@.
-- The type @n@ encodes the vector's length, and will
-- usually either be 'Unknown' or will satisfy 'PositiveT'.
-- 
-- Operations which return vectors of length 'Unknown'
-- will return vectors whose lengths are determined
-- at runtime. All operations which mention 'Unknown'
-- lengths will have names ending in an uppercase U,
-- for example 'fromListU', 'subVectorU'.
-- 
-- The use of 'Unknown' facilitates manipulation
-- of dynamically-lengthed vectors without
-- using continuations for each operation, since
-- most operations work equally well for lengthed
-- as well as unlengthed vectors. When vectors
-- of 'Unknown' length are used, runtime length
-- mismatches may arise, and the system is as safe
-- as hmatrix.
-- 
-- When the length of every vector is known, if
-- the code typechecks, then there will be
-- no runtime vector length mismatches. Equivalently,
-- there will be no runtime vector length mismatches
-- if:
-- 
--   * no unsafe functions are used; and 
-- 
--   * no functions mentioning 'Unknown' are used, i.e. no functions with suffix U are used.
newtype Vector n t = Vector { unVector :: H.Vector t } 