module Reader where

newtype Reader r a = Reader {runReader :: r -> a}

-- fmap :: (a -> b) -> Reader r a -> Reader r b
instance Functor (Reader r) where
  fmap ab ra = Reader $ \r ->
    let a = runReader ra r
     in ab a

-- pure :: a -> Reader r a
-- <*>  :: Reader r (a -> b) -> Reader r a -> Reader r b
instance Applicative (Reader r) where
  pure a = Reader (const a)
  rab <*> ra = Reader $ \r ->
    let ab = runReader rab r
        a = runReader ra r
     in ab a

-- >>= :: Reader r a -> (a -> Reader r b) -> Reader r b
instance Monad (Reader r) where
  return = pure
  ra >>= arb = Reader $ \r ->
    let a = runReader ra r
     in runReader (arb a) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f ra = Reader $ (runReader ra) . f
