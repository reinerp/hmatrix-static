-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Imports
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Useful imports from other packages. In particular:
-- imports from TFP, Foreign.Storable, and HMatrix.
--
-----------------------------------------------------------------------------

module Data.Packed.Static.Imports(
   H.Element,
   H.Field,
   H.Linear,
   F.Storable,
   module Types.Data.Num,
   True,
   (:<=:),
   Min,
  ) where

import qualified Numeric.LinearAlgebra as H
import qualified Foreign.Storable as F

import Types.Data.Num
import Types.Data.Bool(True)
import Types.Data.Ord((:<=:), Min)

