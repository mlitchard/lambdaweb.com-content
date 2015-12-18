## Dirty Dirty I/O And The Errors It Makes Us Handle

What do we have thus far?

1. A fizzbuzz function.
2. A feeder function for fizzbuzz.
3. No way for anyone to use these.

Well then, lets go ahead and make a command line executable for our fuzzbuzz.

```haskell
module Main where
  import FizzBuzz
  import System.Environment

main :: IO ()
main = do
  input <- getArgs
```

Okay, now what? `getArgs` has type `String :: IO ()`, and we need an `Integer`.
Well, can we get a `String -> Integer`? Asking [Hoogle](https://www.haskell.org/hoogle/) we get a big fat "Nope". 

But look at what we did find:

```haskell
read :: Read a => String -> a
```
In our case we would need an `a :: Integer`. And lucky for us,
`Integer` has a `Read` instance:
[Here Be Dragons](http://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.Read.html#line-464)

Let's unpack `read`, there's something cool going on there, and it's called
`id`.

> The 'read' function reads input from a string, which must be
> completely consumed by the input process.

```haskell
read :: Read a => String -> a
read s = either error id (readEither s)
```
We'll go left to right, starting with the [`either`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Either.html) function.
```haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c 
```

> Case analysis for the Either type. If the value is Left a, apply the first function to a; if it is Right b, apply the second function to b.

Both [`error`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:error) and [`id`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:id) use a returned value given by [`readEither`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Text-Read.html).
```haskell
readEither :: Read a => String -> Either String a 
```

> Parse a string using the Read instance. Succeeds if there is exactly one valid result. A Left value indicates a parse error.

So if someone wanted to be a jerkface and give our fizzbuzz program a string that no one would recognize as an `Integer`, but we expected one, `readEither` would do this:
```haskell
Prelude> :m + Text.Read
Prelude Text.Read> readEither "foogle" :: Either String Integer
Left "Prelude.read: no parse"
```
`either` takes the `Left String` and transforms it into a `String`, which is then passed to `error`.

Here's where it gets hairy, because `read` uses `error`, it's a [partial function](https://wiki.haskell.org/Partial_functions). And we want total functions,
every time.

Well now, we can't use `read`, because it's gross. What else is there? While investigating `readEither`, you may have come across [`readMaybe`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Text-Read.html). It's almost exactly what we need.

I say almost, because in the event of bad input it only returns a `Nothing`, which tells us exactly nothing about why the transform failed. It would be better to use `Either`. We could use a case statement to transform a `Maybe` into an `Either`, but since this is needed so often, someone made a function to do it, [`maybeToEither'](https://hackage.haskell.org/package/MissingH-1.3.0.1/docs/Data-Either-Utils.html).

This allows us to have a simple function that looks like this:
```haskell
convertToDigit :: String -> Either String Integer
convertToDigit str =
maybeToEither "Not An Integer" (readMaybe str)
```
Does `Left String` looks awful to you? It should. We have a strong type system in Haskell, but it can't help us if we resort to the quick and the dirty. We can do better.

```haskell
data FizzError = NotAnInteger deriving Show
```

If you think that's overkill for one error condition you'd be right. We'll add more values soon.

In the meantime we can improve the above function like so:

```haskell
convertToDigit :: String -> Either FizzError Integer
convertToDigit str =
maybeToEither NotAndInteger (readMaybe str)
```

Much better. Having handled that error condition, what others could we encounter? Before we transform the `String` to an `Integer`, we need to make sure we get exactly one `String`. We could just drop excess input silently, but this might 
lead a person that the extraneous input was being used somehow. Best to just 
handle this condition, as we need to address the condition of getting no
input as well. We can express these errors as values of our sum type, `FizzError`.

```haskell
data FizzError =
     NotAnInteger
   | OnlyOne
   | NoInput

mustHaveOne :: [String] -> Either FizzError String
mustHaveOne (arg:[]) = Right arg
mustHaveOne []       = Left NoInput
mustHaveOne _        = Left OnlyOne
```

We *could* have the compiler generate a `Show` instance for us, but that
would be impolite to our users. Instead, let's be more clear about our 
expectations when they err.

```haskell
instance Show FizzError where
  show NotAnInteger = "not an integer"
  show OnlyOne      = "Just pass in one number that decribes how many"    ++
                      " fibonacci numbers you want for fizzbuzz."
  show NoInput      = "You need to pass in an integer that describes how" ++
                      " many fibonacci numbers you want for fizzbuzz."
```

We now have a function accounts for one error condition, another accounting for the other two, the fibnonacci feeder function and 
the actual fizzbuzz function.

And all we have to do is put it together right?
Sure, but there's a bad way where we pretend that haskell doesn't have
support for monads, and there's a way using monads that make the bad way better.

Here's what happens if we use case as a control structure.

```haskell
main :: IO ()
main = do
  input <- getArgs
  let res = case (mustHaveOne input) of
              Left err  -> Left err
              Right str ->
                case (convertToDigit str) of
                  Left err -> Left err
                  Right int ->
                    Right $ map (fizzbuzz . fib) ((\x -> [1 .. x]) int)
  putStrLn (show res)
```

`case` becomes staircase. Hard to read, hard to modify. Painful to behold.
We can do better, because [`Either`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Either.html) is a monad.

```haskell
instance Monad (Either e) where
  Left  l >>= _ = Left l
  Right r >>= k = k r
```

What this means is, with a little modification we can make a cleaner
[control structure](https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling) that is easy to read and extend.

First we change the semantics of our code a little.

```haskell
fizzbuzz :: Integer -> Either FizzError String
fizzbuzz i = Right $ fromMaybe (show i) $ getOption fizzbuzz'
  where
    fizzbuzz' =
      ["fizz" | i `rem` 3 == 0] <>
      ["buzz" | i `rem` 5 == 0] <>
      ["bang!" | isPrime i]

    fib :: Integer -> Either FizzError Integer
    fib n = Right $ (round $ phi ** fromIntegral n / sq5)
      where
        sq5 = sqrt 5 :: Double
        phi = (1 + sq5) / 2
```

We'll benefit from using this lambda

```haskell
    (\x -> Right [1 .. x])
```

Then we can do this:

```haskell
fizzBuzzFib :: [String] -> Either FizzError [String]
fizzBuzzFib str =
  mapM fizzbuzz          =<<
  mapM fib               =<<
  (\x -> Right [1 .. x]) =<<
  convertToDigit         =<<
  mustHaveOne str
```

Before running the commands below, take a look at the [.cabal file](https://www.haskell.org/cabal/users-guide/developing-packages.html#editing-the-.cabal-file) and get a feel for how the build is being organized. Also make sure `$HOME/.local/bin` is in your `$PATH`.

We can now see some fizzbuzz action. The expected input is the an `Integer` n,
where n is the nth fibonacci number. A list from 1 to n is then generated,
which is used to generate a list of the first n fibonacci numbers. It's this list our fizzbuzz machine uses.

    stack clean
    stack install
    Copied executables to /home/mlitchard/.local/bin:
    - fizzbuzzfib-exe
    mlitchard@mlitchard-personal:~/projects/git/fizzbuzzfib$ fizzbuzzfib-exe
    You need to pass in an integer that describes how many fibonacci numbers you want for fizzbuzz.

Let's try again

    mlitchard@mlitchard-personal:~/projects/git/fizzbuzzfib$ fizzbuzzfib-exe "Bippity Boppity Boo"
    not an integer

And Again

    mlitchard@mlitchard-personal:~/projects/git/fizzbuzzfib$ fizzbuzzfib-exe 10 5 15
    Just pass in one number that decribes how many fibonacci numbers you want for fizzbuzz.

Enough messing around, buzz me!
`mlitchard@mlitchard-personal:~/projects/git/fizzbuzzfib$ fizzbuzzfib-exe 20`

> "1"
> "1"
> "bang!"
> "fizzbang!"
> "buzzbang!"
> "8"
> "bang!"
> "fizz "
> "34"
> "buzz "
> "bang!"
> "fizz "
> "bang!"
> "377"
> "buzz "
> "fizz "
> "bang!"
> "2584"
> "4181"
> "fizzbuzz "


Composition is the solution to the unprecedented complexity we find in software engineering, compared to other engineering disciplines, and [monads](https://www.youtube.com/watch?v=ZhuHCtR3xq8) is one tool that allow us to compose constructs [that aren't functions](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html).


