module Result where

data Result a
  = Empty
  | Result a

instance (Show a) => Show (Result a) where
  show Empty      = "Empty"
  show (Result a) = "Result " <> show a

instance Functor Result where
  fmap _ Empty      = Empty
  fmap f (Result a) = Result . f $ a

instance Applicative Result where
  pure a = Result a
  Empty <*> _         = Empty
  (Result fab) <*> fb = fmap fab fb

instance Monad Result where
  return = pure
  Empty >>= _        = Empty
  (Result a) >>= afb = afb a
