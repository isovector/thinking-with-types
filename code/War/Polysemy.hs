module War.Polysemy where

import Control.Monad (ap)

data Free f a
  = Pure a
  | Bind (f (Free f a))

data Console a
  = GetLine (String -> a)
  | PutLine String a
  deriving Functor

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Bind b) = Bind ((fmap . fmap) f b)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

-- # MonadFree
instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Bind b >>= f = Bind (fmap (>>= f) b)

foldFree
    :: Monad m
    => (forall x. f x -> m x)
    -> Free f a
    -> m a
foldFree _   (Pure a) = pure a
foldFree run (Bind b) = run b >>= foldFree run

sayHello :: Free Console ()
sayHello = do
  send $ PutLine "What is your name?" ()
  name <- send $ GetLine id
  send $ PutLine ("Hello, " <> name) ()
  where
    send f = Bind $ fmap pure f

sayHelloImproved :: CodensityT (Free Console) ()
sayHelloImproved = do
  send $ PutLine "What is your name?" ()
  name <- send $ GetLine id
  send $ PutLine ("Hello, " <> name) ()
  where
    send f = liftCodensity $ Bind $ fmap pure f

sayHello2 :: Free Console ()
sayHello2 =
  Bind $ PutLine "What is your name?" $
  Bind $ GetLine $ \name ->
  Bind $ PutLine ("Hello, " <> name) $
  Pure ()



interpretConsole :: Console a -> IO a
interpretConsole (GetLine f) = do
  result <- getLine
  pure $ f result
interpretConsole (PutLine str a) = do
  putStrLn str
  pure a

newtype CodensityT m a = CodensityT
  { unCodensityT :: forall r. (a -> m r) -> m r
  }

instance Functor (CodensityT m) where
  fmap f (CodensityT c) = CodensityT $ \c' -> c (c' . f)

instance Applicative (CodensityT m) where
  pure a = CodensityT $ \c -> c a
  CodensityT f <*> CodensityT a = CodensityT $ \br -> f $ \ab -> a $ br . ab

-- # MonadCodensityT
instance Monad (CodensityT m) where
  return = pure
  CodensityT m >>= f = CodensityT $ \c ->
    m $ \a -> unCodensityT (f a) c

liftCodensity :: Functor f => Free f a -> CodensityT (Free f) a
liftCodensity t = CodensityT $ \k -> t >>= k

