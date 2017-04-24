{-# Language DeriveFunctor #-}

module Progress where

scaleProgress :: Int -> Progress a -> Progress a
scaleProgress n = go 1
  where
    go _ Done                 = Done
    go i (Success x y)        = Success x (go (i+1) y)
    go i (Step x) | i >= n    = Step (go 1 x)
                  | otherwise = go (i+1) x

data Progress a = Step (Progress a) | Success a (Progress a) | Done
  deriving (Read, Show, Functor)

takeWhileProgress :: (a -> Bool) -> Progress a -> Progress a
takeWhileProgress _ Done = Done
takeWhileProgress p (Step xs) = Step (takeWhileProgress p xs)
takeWhileProgress p (Success x xs)
  | p x = Success x (takeWhileProgress p xs)
  | otherwise = Done

filterProgress :: (a -> Bool) -> Progress a -> Progress a
filterProgress _ Done = Done
filterProgress p (Step xs) = Step (filterProgress p xs)
filterProgress p (Success x xs)
  | p x = Success x (filterProgress p xs)
  | otherwise = Step (filterProgress p xs)
