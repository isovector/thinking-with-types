module NoImpredicativeTypes where

ex :: (forall x. x -> x) -> Int
ex _ = 0

{-

const :: a -> b -> a
const a _ = a

-- # badEx
ex :: (forall x. x -> x) -> Int
ex = const 0

-}


