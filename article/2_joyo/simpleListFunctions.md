# Common Haskell List Functions

## Basic List Access

### head
**Signature**:
```haskell
head :: [a] -> a
```

**Description**:  
Returns the first element of a list. Throws an error on empty list.

**Examples**:
```haskell
head [1,2,3,4,5] -- Returns: 1
head "Hello"     -- Returns: 'H'
```

### last
**Signature**:
```haskell
last :: [a] -> a
```

**Description**:  
Returns the last element of a list. Throws an error on empty list.

**Examples**:
```haskell
last [1,2,3,4,5] -- Returns: 5
last "Hello"     -- Returns: 'o'
```

### tail
**Signature**:
```haskell
tail :: [a] -> [a]
```

**Description**:  
Returns all elements of a list except the first one. Throws an error on empty list.

**Examples**:
```haskell
tail [1,2,3,4,5] -- Returns: [2,3,4,5]
tail "Hello"     -- Returns: "ello"
```

### init
**Signature**:
```haskell
init :: [a] -> [a]
```

**Description**:  
Returns all elements of a list except the last one. Throws an error on empty list.

**Examples**:
```haskell
init [1,2,3,4,5] -- Returns: [1,2,3,4]
init "Hello"     -- Returns: "Hell"
```

## List Information

### length
**Signature**:
```haskell
length :: [a] -> Int
```

**Description**:  
Returns the number of elements in a list.

**Examples**:
```haskell
length [1,2,3,4,5] -- Returns: 5
length []          -- Returns: 0
```

### null
**Signature**:
```haskell
null :: [a] -> Bool
```

**Description**:  
Tests whether a list is empty.

**Examples**:
```haskell
null []           -- Returns: True
null [1,2,3]      -- Returns: False
```

### elem
**Signature**:
```haskell
elem :: Eq a => a -> [a] -> Bool
```

**Description**:  
Tests whether an element is present in a list.

**Examples**:
```haskell
elem 3 [1,2,3,4,5] -- Returns: True
elem 'x' "Hello"   -- Returns: False
```

### (!!)
**Signature**:
```haskell
(!!) :: [a] -> Int -> a
```

**Description**:  
Returns the element at the specified index (zero-based). Throws an error if index is out of bounds.

**Examples**:
```haskell
[1,2,3,4,5] !! 2  -- Returns: 3
"Hello" !! 1      -- Returns: 'e'
```

## List Combination

### (++)
**Signature**:
```haskell
(++) :: [a] -> [a] -> [a]
```

**Description**:  
Concatenates two lists.

**Examples**:
```haskell
[1,2,3] ++ [4,5,6]  -- Returns: [1,2,3,4,5,6]
"Hello" ++ " World"  -- Returns: "Hello World"
```

## List Calculations

### sum
**Signature**:
```haskell
sum :: Num a => [a] -> a
```

**Description**:  
Computes the sum of a list of numbers.

**Examples**:
```haskell
sum [1,2,3,4,5]  -- Returns: 15
sum []           -- Returns: 0
```

### product
**Signature**:
```haskell
product :: Num a => [a] -> a
```

**Description**:  
Computes the product of a list of numbers.

**Examples**:
```haskell
product [1,2,3,4]  -- Returns: 24
product []         -- Returns: 1
```

## List Transformation

### reverse
**Signature**:
```haskell
reverse :: [a] -> [a]
```

**Description**:  
Reverses the order of elements in a list.

**Examples**:
```haskell
reverse [1,2,3,4,5]  -- Returns: [5,4,3,2,1]
reverse "Hello"      -- Returns: "olleH"
```

### maximum
**Signature**:
```haskell
maximum :: Ord a => [a] -> a
```

**Description**:  
Returns the maximum element in a non-empty list.

**Examples**:
```haskell
maximum [1,5,3,2,4]  -- Returns: 5
maximum "Hello"      -- Returns: 'o'
```

### minimum
**Signature**:
```haskell
minimum :: Ord a => [a] -> a
```

**Description**:  
Returns the minimum element in a non-empty list.

**Examples**:
```haskell
minimum [1,5,3,2,4]  -- Returns: 1
minimum "Hello"      -- Returns: 'H'
```

## List Slicing

### take
**Signature**:
```haskell
take :: Int -> [a] -> [a]
```

**Description**:  
Returns the first n elements of a list. If n is negative or zero, returns empty list.

**Examples**:
```haskell
take 3 [1,2,3,4,5]  -- Returns: [1,2,3]
take 2 "Hello"      -- Returns: "He"
```

### drop
**Signature**:
```haskell
drop :: Int -> [a] -> [a]
```

**Description**:  
Removes the first n elements of a list. If n is negative or zero, returns the original list.

**Examples**:
```haskell
drop 3 [1,2,3,4,5]  -- Returns: [4,5]
drop 2 "Hello"      -- Returns: "llo"
```

## List Combination Operations

### zip
**Signature**:
```haskell
zip :: [a] -> [b] -> [(a,b)]
```

**Description**:  
Pairs up two lists into a list of tuples. If lists are of unequal length, excess elements are discarded.

**Examples**:
```haskell
zip [1,2,3] ['a','b','c']  -- Returns: [(1,'a'),(2,'b'),(3,'c')]
zip [1,2] ['a','b','c']    -- Returns: [(1,'a'),(2,'b')]
```

### repeat
**Signature**:
```haskell
repeat :: a -> [a]
```

**Description**:  
Creates an infinite list of identical elements.

**Examples**:
```haskell
take 5 (repeat 3)  -- Returns: [3,3,3,3,3]
take 3 (repeat 'a')  -- Returns: "aaa"
```

### concat
**Signature**:
```haskell
concat :: [[a]] -> [a]
```

**Description**:  
Flattens a list of lists into a single list.

**Examples**:
```haskell
concat [[1,2],[3,4],[5,6]]  -- Returns: [1,2,3,4,5,6]
concat ["Hello", " ", "World"]  -- Returns: "Hello World"
```