-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Shapes
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Shape-based functionality, common for matrices and vectors
--
-----------------------------------------------------------------------------

{-# LANGUAGE UndecidableInstances #-}

module Data.Packed.Static.Shapes (
     Unknown,
     ShapedContainer(..),
     atShape,
     shapeOf,
     forgetShapeU,
     unsafeWrap,
 ) where

import qualified Numeric.LinearAlgebra as H

-- | Uninhabited type. Represents unknown lengths.
-- Instances of 'ShapedContainer' use 'Unknown'
-- for the 'UnknownShape' type.
data Unknown

class ShapedContainer a where
    -- | Less-typed, hmatrix representation
    type Unwrapped a :: * -> *
    -- | Convert to hmatrix representation
    unWrap :: a s t -> Unwrapped a t
    -- | Convert from hmatrix representation
    wrapU :: Unwrapped a t -> a (UnknownShape a) t
    
    -- | standard \'unknown\' shape. For vectors, @Unknown@; for matrices, @(Unknown,Unknown)@.
    type UnknownShape a
    -- | Coerce the static shape. Unsafe; the user
    -- of this function has an obligation to prove that
    -- the object's dynamic shape is the same as that
    -- represented by s'.
    unsafeReshape :: a s t -> a s' t

-- | For type hints.
-- 
-- @\> constant (5::Double) `atShape` d4
-- [$vec| 5.0, 5.0, 5.0, 5.0 |] :: Vector D4 Double@
--
-- Implementation:
-- 
-- @atShape = const@.
atShape :: a s t -> s -> a s t
atShape = const

-- | For type hints.
--
-- @\> constant (5::Double) `atShape` shapeOf [$vec|1|]
-- [$vec| 5.0 |]@
--
-- Implementation:
--
-- @shapeOf _ = undefined@
shapeOf :: a s t -> s
shapeOf _ = undefined

-- | @unsafeWrap = unsafeReshape . wrapU@.
unsafeWrap :: ShapedContainer a => Unwrapped a t -> a s t
unsafeWrap = unsafeReshape . wrapU

-- | Changes the static shape to the UnknownShape.
-- Dynamic representation is unchanged.
forgetShapeU :: ShapedContainer a => a s t -> a (UnknownShape a) t
forgetShapeU = unsafeReshape

------- instances

liftH f = unsafeWrap . f . unWrap
liftH2 f a b = unsafeWrap $ f (unWrap a) (unWrap b)
liftH2' f a b = f (unWrap a) (unWrap b)

instance (ShapedContainer a, H.Container (Unwrapped a) e) => H.Container (a n) e where
    toComplex = uncurry $ liftH2 $ curry H.toComplex
    fromComplex m = let (a,b) = H.fromComplex $ unWrap m in (unsafeWrap a, unsafeWrap b)
    comp = liftH H.comp
    conj = liftH H.conj
    real = liftH H.real
    complex = liftH H.complex

instance (ShapedContainer a, H.Linear (Unwrapped a) e) => H.Linear (a n) e where
    scale e       = liftH (H.scale e)
    addConstant e = liftH (H.addConstant e)
    add           = liftH2 H.add
    sub           = liftH2 H.sub
    mul           = liftH2 H.mul
    divide        = liftH2 H.divide
    scaleRecip e  = liftH (H.scaleRecip e)
    equal         = liftH2' H.equal

instance (ShapedContainer a, Eq (Unwrapped a t)) => Eq (a s t) where
    (==) = liftH2' (==)

instance (ShapedContainer a, Show (a n e), Num (Unwrapped a e)) => Num (a n e) where
    (+) = liftH2 (+)
    (*) = liftH2 (*)
    (-) = liftH2 (-)
    negate = liftH negate
    abs = liftH abs
    signum = liftH signum
    fromInteger = error "fromInteger: Data.Packed.Static.Common"

instance (ShapedContainer a, Show (a n e), Fractional (Unwrapped a e)) => Fractional (a n e) where
    (/) = liftH2 (/)
    recip = liftH recip
    fromRational = error "fromRational: Data.Packed.Static.Common"

instance (ShapedContainer a, Show (a n e), Floating (Unwrapped a e)) => Floating (a n e) where
    pi      = error "pi: Data.Packed.Static.Common"
    exp     = liftH exp
    sqrt    = liftH sqrt
    log     = liftH log
    (**)    = liftH2 (**)
    logBase = liftH2 logBase
    sin     = liftH sin
    tan     = liftH tan
    cos     = liftH cos
    asin    = liftH asin
    atan    = liftH atan
    acos    = liftH acos
    sinh    = liftH sinh
    tanh    = liftH tanh
    cosh    = liftH cosh
    asinh   = liftH asinh
    atanh   = liftH atanh
    acosh   = liftH acosh

instance (ShapedContainer a, H.Normed (Unwrapped a e)) => H.Normed (a n e) where
    pnorm p = H.pnorm p . unWrap
