# Overview

Your goal in this project is to implement an interpreter for a subset of [Web assembly](https://webassembly.org/) (Wasm).

Wasm is a low-level language, standardized by the W3C, and implemented in Firefox, Chrome, and Safari (among others).  Its goal is to be a portable format for executable programs.  It was originally designed as [a (very small) subset of Javascript](https://en.wikipedia.org/wiki/Asm.js), but is now better viewed as an independent language.

The Wasm spec is available [here](https://webassembly.github.io/spec/core/index.html), and is the final authority for how Wasm works.

# Project Goals

When complete, your project will:

* Load programs in Wasm text format

* Execute those programs

* Output the computed values

Your task is the middle bullet.  We've provided code to load programs, generating an [AST](../provided-src/Syntax.hs), and to print the result of your interpretation function.

# Limitations

We will not be implementing the full Wasm spec.  Our implementation will be limited as follows.

* Wasm defines multiple integer, floating point, and vector types.  We will only implement 32-bit integers.

* We will not implement Wasm's memory or reference operations.  We will implement local and global variables.

* We will not implement function tables or indirect branches.

* We will implement conditionals, functions calls and returns, but not blocks or loops.

# Extra credit

You may attempt omitted portions of the Wasm spec as extra credit.  

The provided code already supports blocks, loops, branches, and conditional branches.  These are probably the most impactful additional features to support.

If you want to implement features beyond those, please get in touch on Piazza or by opening a GitHub issue.

# Additional documentation

The remaining documentation for the project is broken into several files:

* [Wasm](Wasm.md) An overview of web assembly, and a guide to reading the Wasm spec.

* [Provided](Provided.md) An overview of the code we have provided

* [Tests](Tests.md) A description of the tests we have provided and the testing and grading methodology for the project.
