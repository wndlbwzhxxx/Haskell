{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module MyStateT where

import Control.Monad (ap)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

state :: Monad m => (s -> (a, s)) -> MyStateT s m a
state f = MyStateT $ return . f

get :: Monad m => MyStateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> MyStateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> MyStateT s m ()
modify f = state $ \s -> ((), f s)

runState :: Monad m => MyStateT s m a -> s -> m (a, s)
runState = runMyStateT

evalState :: Monad m => MyStateT s m a -> s -> m a
evalState m s = do
    (a, _) <- runMyStateT m s
    return a

execState :: Monad m => MyStateT s m a -> s -> m s
execState m s = do
    (_, s') <- runMyStateT m s
    return s'

instance Monad m => Functor (MyStateT s m) where
    fmap f m = MyStateT $ \s -> do
        (a, s') <- runMyStateT m s
        return (f a, s')

instance Monad m => Applicative (MyStateT s m) where
    pure a = MyStateT $ \s -> return (a, s)
    (<*>) = ap

instance Monad m => Monad (MyStateT s m) where
    m >>= k = MyStateT $ \s -> do
        (a, s') <- runMyStateT m s
        runMyStateT (k a) s'

instance MonadTrans (MyStateT s) where
    lift ma = MyStateT $ \s -> do
        a <- ma
        return (a, s)

instance MonadIO m => MonadIO (MyStateT s m) where
    liftIO = lift . liftIO