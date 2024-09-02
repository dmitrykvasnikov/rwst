module StateT where

import           Result

data StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Monad m) => Functor (StateT s m) where
  fmap ab sa = StateT $ \s -> fmap (\(a, s') -> (ab a, s')) . runStateT sa $ s

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  StateT sab <*> StateT sa = StateT $ \s -> do
    (ab, s') <- sab s
    (a, s'') <- sa s'
    return (ab a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT sa >>= asb = StateT $ \s -> sa s >>= \(a, s') -> runStateT (asb a) s'

evalState :: (Monad m) => StateT s m a -> s -> m a
evalState sa s = runStateT sa s >>= \(a, _) -> return a

execState :: (Monad m) => StateT s m a -> s -> m s
execState sa s = runStateT sa s >>= \(_, s') -> return s'

get :: (Monad m) => StateT s m s
get = gets id

gets :: (Monad m) => (s -> b) -> StateT s m b
gets sb = StateT $ \s -> return (sb s, s)

put :: (Monad m) => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> return ((), f s)
