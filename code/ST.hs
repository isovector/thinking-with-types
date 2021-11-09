-- # pragmas

module ST where

-- # imports
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

newtype ST s a = ST  -- ! 1
  { unsafeRunST :: a
  }

-- # functor
instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

-- # applicative
instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

-- # monad
instance Monad (ST s) where
  ST a >>= f = seq a $ f a

newtype STRef s a = STRef  -- ! 1
  { unSTRef :: IORef a
  }

newSTRef :: a -> ST s (STRef s a)  -- ! 1
newSTRef =
  pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef =
  pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref =
  pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

runST
    :: (forall s. ST s a)  -- ! 1
    -> a
runST x = unsafeRunST x

{-

-- # runSTType
runST :: (forall s. ST s a) -> a

-}

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref

{-

-- # signature
runST
    :: (forall s. ST s (STRef s Bool)) -- ! 1
    -> STRef s Bool  -- ! 2

-}

