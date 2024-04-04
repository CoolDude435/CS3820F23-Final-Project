# Testing and Evaluation

Your project will be evaluated in four major components.  Each will have its own set of tests.

## Testing framework

The testing framework consists of a collection of Wasm modules, along with a test runner program that compares your interpretation of these modules with the expected interpretation.  Because these are distributed as complete modules, at a *minimum*, your implementation will need to be able to call and execute the starting function of a module.

The test runner is in [TestRunner.hs](`../provided-src/TestRunner.hs`).  You should not need to change anything about this file.  In particular, the test runner interacts with your interpreter by calling `cabal run Wasm`, just as you might.

To invoke the test runner, you can call `cabal run Test`.  At a *minimum*, you must provide a set of tests to run.  For example, you could call `cabal run Test tests/all_tests` or `cabal run Tests tests/numeric/numeric_tests`.

You can also provide a range of tests to run.  For example, `cabal run Test tests/all_tests 1 20`

Test files contain individual tests, and other test files by inclusion.  An individual test looks like

```
("tests/calls/mul_rec.wast", [11,121], [1331])
```

This means that running [`mul_rec.wast`](../tests/calls/mul_rec.wast) with arguments `11` and `121` should produce the result `1331`.

## Components

The four components of the project are:

* Numerics (40%)
* Local and global variables (20%)
* Conditionals (20%)
* Function calls and returns (20%)

In each case, your score will be based on the *percentage* of tests that you pass.  For example, if you pass 80% of the numerics tests, then you'll get 32/40 for numerics.  This means that every *individual* test case that you pass adds to your final grade.

## Extra credit

For extra credit, you can implement parts of the Wasm spec that aren't included in the project.  For example, you could implement blocks, loops, and branches.  Or, you could implement additional data types.

Implementing blocks, loops, and branches will be worth *25%* extra credit.  If you'd like to implement other additional features, please get in touch (on Piazza or Github issues) so we can work out exact details.
