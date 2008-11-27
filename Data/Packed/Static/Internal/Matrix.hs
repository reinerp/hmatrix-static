-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Internal.Matrix
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Matrix newtype definition.
--
-----------------------------------------------------------------------------


module Data.Packed.Static.Internal.Matrix(
   Matrix(..),
 ) where

import qualified Numeric.LinearAlgebra as H

-- | A matrix with @m@ rows, @n@ columns.
newtype Matrix mn t = Matrix { unMatrix :: H.Matrix t } deriving()
