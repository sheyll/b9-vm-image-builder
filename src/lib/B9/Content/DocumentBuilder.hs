{-# LANGUAGE DeriveFunctor #-}
-- EXPERIMENT!!!

module B9.Content.DocumentBuilder where

import Control.Applicative

data Bindings r = Let [(String, String)] (Bindings r)
                | Each [(String, [String])] (Bindings r)
                | Verbatim r
   deriving (Functor)

data WithBindings a = WithBindings [(String,String)] a

bind :: Bindings r -> WithBindings r
bind = undefined

data Free m a = Do (m (Free m a)) | Done a

instance (Functor m) => Monad (Free m) where
  return         = Done
  (Do m) >>= f = Do (fmap (>>= f) m)
  (Done r) >>= f = f r

instance Functor m => Functor (Free m) where
  fmap f ma = (ma >>= (\a -> return (f a)))

instance (Functor m) => Applicative (Free m) where
  pure = return
  f <*> x = f >>= (\f' -> x >>= return . f')

data Bindings' r a = Let' [(String, String)] a
                   | Each' [(String, [String])] a
                   | Verbatim' r a
   deriving (Functor)
type BindingsM r a = Free (Bindings' r) a

liftF :: Functor m => m a -> Free m a
liftF x = Do (fmap Done x)

b_let :: [(String,String)] -> BindingsM r ()
b_let env = liftF (Let' env ())

b_each :: [(String,[String])] -> BindingsM r ()
b_each each = liftF (Each' each ())

b_verbatim :: r -> BindingsM r ()
b_verbatim x = liftF (Verbatim' x ())

xxx3 :: BindingsM Bool ()
xxx3 = do
  b_let [("x" , "10.23415")]
  b_verbatim True
  b_each [("y", ["1","2"])]
  b_verbatim False

ppxx :: Show r => BindingsM r () -> IO ()
ppxx (Do (Let' bs next)) = do
  putStrLn ("Let " ++ show bs ++ "\nIn:")
  ppxx next
ppxx (Do (Each' bs next)) = do
  putStrLn ("Each " ++ show bs ++ "\nIn:")
  ppxx next
ppxx (Do (Verbatim' r next)) = do
  putStrLn ("Verbatim " ++ show r)
  ppxx next
ppxx (Done _) = return ()
