# Program Execution

The most logical way to think about Wasm's execution model is as a *kind* of small-step evaluation.

It's like small step evaluation in that you want to think about individual, relatively simple transitions of the program's state.  You won't have *recursive* evaluation of individual commands.

It's unlike small step evaluation in that the program is *already* broken down into small steps.  This means you won't need much that corresponds to "congruence rules"

## Program State

At the top level, a Wasm program's state has a couple of components: the stack, the local variables (which the official spec calls the frame), the global variables, and the instruction stream.

(You should probably define a type called something like `WasmState` in your implementation that packages these up.  Your implementation will then include a function like `step :: WasmState -> WasmState`.)

Unfortunately, things are not quite that simple.  You will need to execute function calls, function returns, and (for extra credit) can implement blocks and loops.

For this reason, I suggest you think about the program state as a *stack* of *contexts*, where each context contains its own stack and instructions.  You'll also want a *stack* of sets of local variables (each function call gets its own parameters and locals).

Your state will only need a single set of global variables (as we're not consider programs spread across multiple modules.)

(*Note:* This isn't how the Wasm spec describes the state.  They have a single stack, but it's effectively subdivided by special stack entries they call "labels".  For me, thinking of it as a stack of stacks made a lot more sense.)

## Numerics

We're only implementing one Wasm data type: 32-bit integers (`i32`).  We'll represent these with the Haskell type [`Word32`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Word.html#t:Word32); that is to say, we're picking an *unsigned* representation by default.

Most of Wasm's numeric operations can be realized using the [`Num`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#t:Num) operations on `Word32`.

For bitwise operations, use the functions from [`Data.Bits`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Bits.html#t:Bits).

There is one complication: for a few operations (division and shifts), Wasm has both *signed* and *unsigned* variations on operations.  In Haskell, the same thing is achieved by using different types: `Int32` is the *signed* type, and `Word32` is the *unsigned* type.  The `fromIntegral` function will convert between these types (without changing the underlying bits), so you can use it to switch to `Int32` to implement signed operations.

You are likely to also discover that there are a variety of functions from the `Data.List` library that want `Int`s, not `Word32`s.  You have two options here.  One is, again, to use `fromIntegral` to convert your `Word32`s to `Int`s.  The other is that many of the `Data.List` functions have *generic* versions that will work for `Word32`.  For example, while `drop :: Int -> [a] -> [a]` only works with integer counts, `dropGeneric :: Num b => b -> [a] -> [a]` will work with a `Word32` count.

## Functions

Functions are the top-level Wasm control abstraction; you should implement function calls and returns.

Suppose you call a function a with type `[i32,i32] -> [i32,i32,i32]`.  What happens?

* The function needs two arguments.  These are the top two values of the stack.  Those values get removed from the stack to be pass to the function.

* The function gets a new stack, and a new set of local variables.  The first two local variables (serving as parameters) get the two values you removed from the stack of the calling function.

Now, the function goes off and does whatever it's going to do.  At some point it'll either execute the `return` instruction or simply run out of instructions.  Then what happens?

* The function has three results.  These are the top three values on the function's stack.  These values get removed from the function's stack to be returned.  (*Note:* there may be more values than that on the function's stack.)

* The three results get pushed onto the *calling* stack, and the calling code resumes executing.  The remaining values of the called function's stack, it's local variables, and so forth, are all discarded.

Of course, you can imagine functions that don't *seem* to play by the rules.  For example, suppose you had something like:

```wasm
  (func (type 0)
    i32.add)
```

Whatever type `0` happens to be, this function is supposed to start with a fresh stack.  That means there are no values on the stack for `i32.add` to add.

In practice, these kinds of functions are ruled out by *validation*, which checks that all Wasm functions and blocks respect the stack.

For this project, you can assume that you only get valid programs.

## Blocks

The other Wasm control structures use blocks.  Blocks behave a little like local function calls.

Suppose you start executing a block like:

```wasm
  block (type 0)
    ...
  end
```

where type `0` is `[i32,i32] -> [i32,i32]`.  (Instead of `block`, this could also be a `loop` or and `if`; the ideas are the same.)

What happens when you enter this block:

* The block expects two values on the stack.  The top two values from the outer block's are popped to provide these values.

* The block gets a new stack, with those two values.  It doesn't get new local variables.

Now, the block goes off and does whatever it's going to do.  Suppose that we exit this block, either by running out of instructions or by executing a `br` or `br_if`.

* The block is supposed to leave three values on the stack.  These are the top three values from the block's stack.  They get popped.

* The rest of the block's stack is discarded.  We push the three results to the outer block's stack.

If you're not doing the extra credit, you do need to maintain the stack properly on entering and leaving `if` blocks.

For extra credit, however, you can implement `block`, `loop`, `br`, and `br_if`.  A couple of notes:

* The number of values to return are determined by the block you're branching to.  So if you execute `br 1`, the number of values to return are determined by the block type of the containing block.

* Branching to a `block` (or indeed, an `if`) *exits* that block.  Branching to a `loop` *continues* the loop.  This means that you probably need to keep track of the loop body as part of its context.

* `loop` do *not* continue if you just reach the end.  Instead, you exit the `loop`.
