# Provided Code

We've provided a couple of bits of code.  Some you may want to look over; some you'll have to edit.

## Wasm AST

The Wasm AST is located in [`Syntax.hs`](../provided-src/Syntax.hs).  It describes the *input* that your interpreter will have to consume.  You should have a look through it.  (You may want to look in parallel at the Wasm spec's [description of Wasm programs](https://webassembly.github.io/spec/core/syntax/index.html))

## Parser

Our parser is located in [`Parser.hs`](../provided-src/Parser.hs).  You shouldn't need to understand or modify this code.

## Main

We've also provided a starting point for your interpreter, in [`Main.hs`](../src/Main.hs).  You'll need to look over and modify this code.

The `main` function in `Main.hs` handles:

* Reading a source file and its arguments from the input
* Parsing the source file
* Invoking your interpreter
* Printing the results

Of course, we don't know what your interpreter will be.  The function `interp :: Module -> [Word32] -> [Word32]` is your starting point, and is called from our `main` function.

You can change the type of `interp`, if you want; for example, if you wanted to rely on `IO`.  If you do so, however, *you* are responsible for updating `main` to call your `interp` function correctly.

You can write your entire interpreter in `Main.hs`, but that might become hard to read.  Feel free to add more modules; but make sure to keep the `other-modules` list in `Wasm.cabal` up to date!
