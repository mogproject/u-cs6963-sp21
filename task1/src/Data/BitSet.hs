module Data.BitSet
  ( BitSet,
    Data.BitSet.empty,
    Data.BitSet.null,
    Data.BitSet.complement,
    member,
    notMember,
    insert,
    delete,
    union,
    intersection,
    difference,
    symmetricDifference,
    toList,
    fromList,
  )
where

import Data.Bits (complement, shift, xor, (.&.), (.|.))

-- | Bitset of length n with an integer value. Currently, n must fit within Int's bit size.
data BitSet = BitSet Int Int

-- | Checks equality of two bitsets.
instance Eq BitSet where
  (BitSet n a) == (BitSet m b) =
    let u = (1 `shift` n) - 1
     in n == m && (a .&. u) == (b .&. u)

-- | Represents the value as a binary string of length n.
instance Show BitSet where
  show bs@(BitSet n _) = concatMap (\x -> if x `member` bs then "1" else "0") [n -1, n -2 .. 0]

-- | Creates an empty bitset of length n.
empty :: Int -> BitSet
empty n = BitSet n 0

-- | Checks if the bitset is empty.
null :: BitSet -> Bool
null (BitSet _ value) = value == 0

-- | Returns the complement set.
complement :: BitSet -> BitSet
complement (BitSet n value) = BitSet n $ ((1 `shift` n) - 1) `xor` value

-- | Returns if the element is in the set.
member :: Int -> BitSet -> Bool
member x (BitSet n value) | 0 <= x && x < n = value .&. (1 `shift` x) /= 0
member _ _ = False

-- | Returns if the element is in the set.
notMember :: Int -> BitSet -> Bool
notMember x b = not $ member x b

-- | Inserts an element in the set.
insert :: Int -> BitSet -> BitSet
insert x (BitSet n value) | 0 <= x && x < n = BitSet n (value .|. (1 `shift` x))
insert _ _ = undefined

-- | Deletes an element in the set.
delete :: Int -> BitSet -> BitSet
delete x (BitSet n value) | 0 <= x && x < n = BitSet n (value .&. Data.Bits.complement (1 `shift` x))
delete _ _ = undefined

-- | Returns the union of two bitsets.
union :: BitSet -> BitSet -> BitSet
union (BitSet n a) (BitSet m b) | n == m = BitSet n $ a .|. b
union _ _ = undefined

-- | Returns the intersection of two bitsets.
intersection :: BitSet -> BitSet -> BitSet
intersection (BitSet n a) (BitSet m b) | n == m = BitSet n $ a .&. b
intersection _ _ = undefined

-- | Returns the difference of two bitsets.
difference :: BitSet -> BitSet -> BitSet
difference (BitSet n a) (BitSet m b) | n == m = BitSet n $ a `xor` (a .&. b)
difference _ _ = undefined

-- | Returns the symmetric difference of two bitsets.
symmetricDifference :: BitSet -> BitSet -> BitSet
symmetricDifference (BitSet n a) (BitSet m b) | n == m = BitSet n $ a `xor` b
symmetricDifference _ _ = undefined

-- | Creates a list of the elements in the set.
toList :: BitSet -> [Int]
toList bs@(BitSet n _) = filter (`member` bs) [0 .. n -1]

-- | Creates a bitset from the maximum set size and a list of elements.
fromList :: Int -> [Int] -> BitSet
fromList n = foldl (flip insert) $ BitSet n 0
