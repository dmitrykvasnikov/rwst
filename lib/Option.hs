module Option where

data Option a b = No a
                | Yes b

instance (Show a, Show b) => Show (Option a b) where
  show (No n)  = "No " <> show n
  show (Yes y) = "Yes " <> show y

instance Functor (Option a) where
  fmap _ (No no) = No no
  fmap f (Yes a) = Yes . f $ a

instance Applicative (Option a) where
  pure a = Yes a
  (No no) <*> _   = (No no)
  (Yes ab) <*> oa = fmap ab oa

instance Monad (Option a) where
  return = pure
  (No no) >>= _   = (No no)
  (Yes a) >>= aob = aob a
