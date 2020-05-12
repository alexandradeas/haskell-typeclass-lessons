module Data.APList (APList(..)) where

data APList a = APValue a (APList a)
              | Empty

instance Show a => Show (APList a) where
  show Empty = "[]"
  show array = "[" ++ showValue array ++ "]"
    where
      showValue (APValue head Empty) = show head
      showValue (APValue head tail)  = show head ++ ", " ++ showValue tail

instance Eq a => Eq (APList a) where
  (==) Empty                 Empty                 = True
  (==) (APValue head1 tail1) (APValue head2 tail2) = head1 == head2 && tail1 == tail2
  (==) _                     _                     = False

-- Ord compare -> Ordering (LT, EQ, GT)