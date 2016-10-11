{-# LANGUAGE TypeFamilies #-}
module VectorV where

newtype VectorV plot v = VectorV { runVectorV :: v }
data Vector2D a = Vector2D { vx :: !a
                           , vy :: !a
                           } deriving(Show)

runVector :: Vector2D a -> (a, a)
runVector v = (vx v, vy v)

infixl 6 ^+^
infixl 7 *^

class VectorSpace v where
  type Scalar v
  (^+^) :: v -> v -> v
  (*^) :: Scalar v -> v -> v
  negateV :: v -> v
  zeroV :: v

class (VectorSpace v) => BasisSpace v where
  basisVectors :: [v]

instance (VectorSpace v) => VectorSpace (VectorV p v) where
  type Scalar (VectorV p v) = Scalar v
  (VectorV v1) ^+^ (VectorV v2) = VectorV (v1 ^+^ v2)
  s *^ (VectorV v) = VectorV (s *^ v)
  negateV (VectorV v) = VectorV (negateV v)
  zeroV = VectorV zeroV

instance (Num a) => VectorSpace (Vector2D a) where
  type Scalar (Vector2D a) = a
  (Vector2D x1 y1) ^+^ (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
  s *^ (Vector2D x y) = Vector2D (s * x) (s * y)
  negateV (Vector2D x y) = Vector2D (negate x) (negate y)
  zeroV = Vector2D 0 0

instance (Num a) => BasisSpace (Vector2D a) where
  basisVectors = [Vector2D 1 0, Vector2D 0 1]

instance (BasisSpace v) => BasisSpace (VectorV p v) where
  basisVectors = fmap VectorV basisVectors
