module Interval where

data Bound a = Open a | Closed a | Infty

newtype Interval a = Interval (Bound a) (Bound a)

lowerBound :: (Ord a) => Bound a -> a -> Bool
lowerBound Infty _      = True
lowerBound (Open a) b   = a < b
lowerBound (Closed a) b = a <= b

upperBound :: (Ord a) => Bound a -> a -> Bool
upperBound Infty _      = True
upperBound (Open a) b   = a > b
upperBound (Closed a) b = a >= b

element :: (Ord a) => a -> Interval a -> Bool
element e (Interval l u) = l `lowerBound` e && u `upperBound` e

instance (Eq a) => Eq (Bound a) where
  (Open a) == (Open b)     = a == b
  (Closed a) == (Closed b) = a == b
  Infty == Infty           = True
  _ == _                   = False

instance (Show a) => Show (Interval a) where
  Show (Interval l u) = showL ++ "," ++ showU
    where showL = case l of
            Infty -> "(-Inf"
            Closed a -> "[" ++ show a
            Open a   -> "(" ++ show a
          showU = case u of
            Infty -> "Inf)"
            Closed a -> show a ++ "]"
            Open a   -> show a ++ ")"


instance (Eq a) => Eq (Interval a) where
  (Interval l1 u1) == (Interval l2 u2) = l1 == l2 && u1 == u2

-- Don't use with product, though...
combineBounds :: (Num a) => (a -> a -> a) -> Bound a -> Bound a -> Bound a
combineBounds op Infty _               = Infty
combineBounds op _ Infty               = Infty
combineBounds op (Open a) (Open b)     = Open (op a b)
combineBounds op (Closed a) (Closed b) = Closed (op a b)
combineBounds op (Open a) (Closed b)   = Open (op a b)
combineBounds op (Closed a) (Open b)   = Open (op a b)

instance (Num a) => Num Interval where
  (Interval l u) + (Interval l' u') = Interval (combineBounds (+) l l') (combineBounds (+) u u')
  (Interval l u) - (Interval l' u') = Interval (combineBounds (-) l l') (combineBounds (-) u u')
  -- This is as meaningful as we can get...
  fromIntegral i = Interval (Closed $ fromIntegral i) (Closed $ fromIntegral i)
  (*) (Interval (Closed a1) (Closed b1)) (Interval (Closed a2) (Closed b2)) = Interval (Closed $ minimum l) (Closed $ maximum l)
          where l = [a1 * a2, b1 * a2, a1 * b2, b1 * b2]
  (*) (Interval (Open a1) (Open b1)) (Interval (Open a2) (Open b2)) = Interval (Open $ minimum l) (Open $ maximum l)
          where l = [a1 * a2, b1 * a2, a1 * b2, b1 * b2]
