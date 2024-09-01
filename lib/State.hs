module State where

data State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap ab sa = State $ \s ->
    let (a, s') = runState sa s
     in (ab a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  sab <*> sa = State $ \s ->
    let (ab, s') = runState sab s
        (a, s'') = runState sa s'
     in (ab a, s'')

instance Monad (State s) where
  return = pure
  sa >>= asb = State $ \s ->
    let (a, s') = runState sa s
     in runState (asb a) s'

evalState :: State s a -> s -> a
evalState sa = fst . runState sa

execState :: State s a -> s -> s
execState sa = snd . runState sa

get :: State s s
get = gets id

gets :: (s -> b) -> State s b
gets sb = State $ \s -> (sb s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
