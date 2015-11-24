Fizzbuzz is a chance to examine contemporary Haskell

This blog post is for people who have seen a bit of Haskell; maybe have read
some (Learn You A Haskell)[http://learnyouahaskell.com/], and appreciate 
seeing the building process in action. I spend a considerable amount of time
in ghci, because I find it useful as a development tool. The first thing
to do is install stack, then we'll get right into ghci and start unpacking
fizzbuzz.

To follow along, you'll want to 
```
stack ghci
```
```
*FizzBuzz> ["fizz " | 15 `mod` 3 == 0]
["fizz "]

```

```
Prelude> (\i -> ["fizz " | i `mod` 3 == 0]) 15
["fizz "]

Prelude> (\i -> ["buzz " | i `mod` 5 == 0]) 11
[]
```

```
Prelude> let fizz3 = (\i -> ["fizz " | i `mod` 3 == 0])
Prelude> let buzz5 = (\i -> ["buzz " | i `mod` 5 == 0])
```

```
Prelude Data.Semigroup> fizz3 9
["fizz "]
Prelude Data.Semigroup> buzz5 9
[]
```

```
(fizz3 <> fizz5) 9

["fizz "]
```

```
Prelude Data.Semigroup> :t (fizz3 <> fizz5) 9
(fizz3 <> fizz5) 9 :: [[Char]]
```
```
Prelude Data.Semigroup> :t (fizz3 <> fizz5)
(fizz3 <> fizz5) :: Integral a => a -> [[Char]]
```


from [`Data.Semigroup`](https://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html)
```
class Semigroup a where Source

Minimal complete definition

Nothing

Methods

(<>) :: a -> a -> a infixr 6 Source

An associative operation.

(a <> b) <> c = a <> (b <> c)

If a is also a Monoid we further require

(<>) = mappend
```


