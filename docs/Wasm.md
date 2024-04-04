# Web assembly

Wasm is a low-level, imperative, stack-based language.

* **Low-level**: Wasm doesn't have compound expressions (like `1 + (2 + 3)`), structured data (like pairs or data types), or higher-order functions.

* **Imperative**: Wasm programs operate by changing the program state (variables, stack, and so forth)

* **Stack-based**: Wasm uses a *stack* for intermediate storage, function arguments, and the like.  Other stack-based languages include Java Bytecode (the JVM's executable format), .Net Common Intermediate Language, and Forth.

Your ultimate reference for Wasm is [its specification]; here, we try to summarize key details of the spec and relate them to other things you've seen in CS:3820.

The summary is divided into:

* [Program structure](Structure.md) discusses how Wasm programs are described.
* [Program execution](Execution.md) discusses how Wasm programs are executed.
