data T a = N | R a (T a)
         deriving (Eq, Ord, Show, Read) 
          

instance Functor T where
  -- fmap :: (a -> b) -> f a -> f b
  fmap g N       = N
  fmap g (R a (as)) = R (g a) (fmap g as)
      
