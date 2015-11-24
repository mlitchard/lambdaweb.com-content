```
stack ghci
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


