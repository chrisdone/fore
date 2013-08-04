# Fore

A Haskell compiler which compiles GHC's Core language to JavaScript.

## What is Core?

Consider the following Haskell program:

```haskell
module Main where

main = do
  putStrLn "Behold, the number of the beast:"
  print (666::Int)
  putStrLn (foo ())
  return ()

class Foo a where
  foo :: a -> String

instance Foo () where foo _ = "Mwuhahaha."
```

When GHC compiles your code, it desugares, simplifies, and translates
it to Core, after which it will implement sharing and inlining, among
other optimizations. So the above program compiled to Core, is—after
stripping out redundant information not necessary for this
explanation—the following:

```haskell
$cfoo :: () -> String
$cfoo = \ _ -> unpackCString# "Mwuhahaha."
main :: IO ()
main =
  >> @ IO $fMonadIO @ () @ ()
    (System.IO.putStrLn
       (unpackCString# "Behold, the number of the beast:"))
    (>> @ IO $fMonadIO @ () @ ()
       (System.IO.print
          @ Int $fShowInt (I# 666))
       (>> @ IO $fMonadIO @ () @ ()
          (System.IO.putStrLn (unpackCString# "Mwuhahaha."))
          (return @ IO $fMonadIO @ () ())))
:main :: IO ()
:main = runMainIO @ () main
```

Note that the `foo` string has been inlined by the compiler.

## How to Read Core

Core has its own syntax for type annotations (the `@`), and class
instance dictionaries. For example, all functions in Haskell which
make use of a class instance, e.g.

    show :: Show a => a -> String

are converted to

    show :: Show a -> a -> String

Meaning that the `show` now takes an additional argument. That
argument is the particular implementation for the Show class. So if I
write:

    show 123

then that is converted to something like

    show instanceForInt 123

Hence, in Core, our `print 666` becomes

    (System.IO.print @ Int $fShowInt (I# 666))

Meaning to pass the `Int` instance for the `Show` class to `print`,
because `print` needs to pass it to `show`.

## Translating to JavaScript

At present, the translation of the above example looks like this:

```javascript
var main$Main$foo = function(a) {
  return tpl
};
var zdcfoo = function(ds) {
  return new $(function() {
    return ghczmprim$GHC$CString$unpackCStringzh("Mwuhahaha.")
  })
};
var main$Main$zdfFooZLZR = zdcfoo;
var main$Main$main = new $(function() {
  return __(base$GHC$Base$zgzg(base$GHC$Base$zdfMonadIO)(
      base$System$IO$putStrLn(ghczmprim$GHC$CString$unpackCStringzh(
          "Behold, the number of the beast:"))))(__(base$GHC$Base$zgzg(
        base$GHC$Base$zdfMonadIO)(base$System$IO$print(base$GHC$Show$zdfShowInt)(
          666)))(__(base$GHC$Base$zgzg(base$GHC$Base$zdfMonadIO)(
          base$System$IO$putStrLn(ghczmprim$GHC$CString$unpackCStringzh(
              "Mwuhahaha."))))(__(base$GHC$Base$return(base$GHC$Base$zdfMonadIO))(
          ghczmprim$GHC$Tuple$Z0T))))
});
var main$ZCMain$main = new $(function() {
  return base$GHC$TopHandler$runMainIO(main$Main$main)
});
```
There are two functions of interest in here:

* `$` — This is a JavaScript object that creates a thunk.
* `__` — This function forces its argument.

Top-level declarations are thunked, and all functions are forced
before applying them to arguments, with the exception of built-ins
which are known to be fully normalized and thus don't need forcing.

An experimental optimization that I'm trying is to replace that
nesting for the IO monad with normal JavaScript sequence expressions:

```javascript
var main$Main$main = new $(function() {
  return _(base$System$IO$putStrLn(ghczmprim$GHC$CString$unpackCStringzh(
        "Behold, the number of the beast:"))),
  _(base$System$IO$print(base$GHC$Show$zdfShowInt)(666)),
  _(base$System$IO$putStrLn(ghczmprim$GHC$CString$unpackCStringzh("Mwuhahaha."))),
  __(base$GHC$Base$return(base$GHC$Base$zdfMonadIO))(ghczmprim$GHC$Tuple$Z0T)
});
```

Which is certainly easier to read and is more efficient, in code space
and runtime performance. But it still has to be tested.

## Running the JavaScript

If we run the generated JavaScript with node, we get:

    Behold, the number of the beast:
    666
    Mwuhahaha.
