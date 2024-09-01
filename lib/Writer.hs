module Writer where

newtype Writer w a = Writer {runWriter :: (a, w)}

instance Functor (Writer w) where
  fmap ab wa =
    Writer $
      let (a, w) = runWriter wa
       in (ab a, w)

instance (Monoid w) => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  wab <*> wa =
    Writer $
      let (ab, w') = runWriter wab
          (a, w'') = runWriter wa
       in (ab a, w' <> w'')

instance (Monoid w) => Monad (Writer w) where
  return = pure
  wa >>= awb =
    Writer $
      let (a, w') = runWriter wa
          (b, w'') = runWriter (awb a)
       in (b, w' <> w'')

tell :: w -> Writer w ()
tell w = Writer ((), w)

listen :: Writer w a -> Writer w (a, w)
listen wa =
  Writer $
    let (a, w) = runWriter wa
     in ((a, w), w)

listens :: (w -> b) -> Writer w a -> Writer w (a, b)
listens wb wa =
  Writer $
    let (a, w) = runWriter wa
     in ((a, wb w), w)

censor :: (w -> w) -> Writer w a -> Writer w a
censor ww wa =
  Writer $
    let (a, w) = runWriter wa
     in (a, ww w)

pass :: Writer w (a, w -> w) -> Writer w a
pass wa =
  Writer $
    let ((a, ww), w) = runWriter wa
     in (a, ww w)
