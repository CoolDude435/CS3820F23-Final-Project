# Program Structure

Wasm programs are described by collections of modules.  We're going to limit ourselves to programs of **one** module.

Each module contains:

* A list of types
* A list of global variables
* A list of functions
* A starting point (function to begin executing)

and some other things that we're not implementing.

Each of these lists is accessed by *index*, not by *name*.  So, we might call function `4`, or read from global variable `2`.  List are indexed from `0`.

The Wasm programs we use contain type lists.  However, the parser transforms references into that list into the corresponding types, so you don't have to worry about the type list.

The list of global variables simply gives each its type and *initializing expression*.  Technically, initializing expressions are allowed to be either constants or to read from other globals.  We will simplify this by requiring that the list of initialzing instructions for a global variable be a singleton list with just one constant expression.

(Technically, each module has its own list of global variables.  Because we're only considering programs with a single module, we can safely say "the global variables" instead of "this module's global variables".)

The starting point is just a function number, like `4` or `0`.

A Wasm function has several components: the function's type, its local variable list, and its body (a list of instructions).

## Types and locals

A function's type has an input *list* of (value) types, and an output *list* of (value) types.  For example:

* `[i32] -> [i32]` is the type of a function that has one `i32` argument and one `i32` result.
* `[] -> []` is the type of a function that takes no arguments, and produces no results
* `[i32,i32] -> [i32,i32,i32]` is the type of a function that takes two `i32` arguments and produces three `i32` results.

Technically, the starting point of a module is required to be a function of type `[] -> []`.  However, as this will make it difficult to observe what your programs do, we relax this restriction.

A function's local variable list is, like globals, a list of types.  Again, as we only support the `i32` value type, locals will always be of that type.

Locals and function parameters are both accessed by index.  Indices start with the parameters, and then move on to local variables.  Suppose a function has two arguments and two locals.  Then the arguments will be accessed as indices `0` and `1`, and the two locals will get indices `2` and `3`.

Indices of locals and parameters are separate from indices for globals, or those for functions.

## Instructions

There are a variety of Wasm instructions:

* *Numeric* instructions provide operations like addition, multiplication, or bitwise operators
* *Parametic* instructions manipulate the values of the stack, independent of their type
* *Variable* instructions access the local and global variables
* *Control* instructions provide flow of control, like loops, conditionals, and function calls.

Most Wasm instructions operate on the *stack*, part of the program state.  For example, the addition instruction `i32.add` doesn't have any explicit parameters.  Instead, it adds the top two values on the stack.

Here's the Wasm instruction sequence to add the number `4` and `5`:

```wasm
  i32.const 4
  i32.const 5
  i32.add
```

The first two instructions push the values `4` and `5` on the stack.  The final operation adds them; at the end, the stack contains the value `9`.

Many Wasm instructions do have explicit arguments.  The `i32.const` instruction, for example, needs to include which constant value to push.  Another example are the variable operations, which need to know which variable to access. 

Here's the Wasm instruction sequence to increment the `0`th local variable:

```wasm
  local.get 0
  i32.const 1
  i32.add
  local.set 0
```

## Control Instructions

Unlike most low-level languages, Wasm provides *structured* control.  This means that Wasm doesn't have labels or arbitrary jumps/GOTOs.  Instead, Wasm's control structure are (somewhat) more like those you'd find in languages like Java or Python.

Suppose we want to execute one block of instructions if the value on the top of the stack is `0`, and a different block if it's non-zero.  We'd use code something like the following:

```wasm
  i32.eqz
  if (type 0)
    ... ;; instruction sequence 1
  else
    ... ;; instruction sequence 2
  end
```

This illustrates many of the defining features of Wasm control operators.

* Control operators may still access the stack; in this case, the `if` looks at the top value on the stack to see which branch to execute.
* Wasm block operators all come with a *block type*; in this case, let's assume that type `0` is `[] -> [i32]`.  This type indicates how the block uses the stack.  In this case, the type asserts that the `if` doesn't consume any (more) values from the stack, but will leave one `i32` value on the stack
* Wasm block operators contain other instruction sequences.

Branches in Wasm count up the number of blocks, rather than referring to absolute labels.  Here's a loop that runs `n` times, where `n` is the value in local `0`:

```wasm
  loop (type 0)
    ... ;; loop presumably does something useful
    local.get 0
    i32.const 1
    i32.sub
    local.tee 0  ;; updates local 0 and leaves its value on the stack.
    i32.const 0
    i32.ne       ;; top of the stack contains whether local 0 is *not* 0.
    br_if 0      ;; jump back to the loop *if* the value on the top of the stack is true.
  end
```

The essential observation here is that, rather than referring to the `loop` by a branch label, or an absolute address, we refer to it as the 0th containing block.
