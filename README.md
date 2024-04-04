[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/_6a-uimT)
[![Open in Codespaces](https://classroom.github.com/assets/launch-codespace-7f7980b617ed060a017424585567c406b6ee15c891e84e1186181d67ecf80aa0.svg)](https://classroom.github.com/open-in-codespaces?assignment_repo_id=12857721)
# The Project

This is the CS:3820 Fall '23 project.

Your task is to implement an interpreter for (a subset of) Web Assembly (Wasm).

* The root of the documentation for the project is [here](docs/Index.md)

* We've provided a syntax tree and parser for Wasm, in `provided-src/Syntax.hs` and `provided-src/Parser.hs`.  You shouldn't need to change these (unless your planning to do some of the extra credit, and not necessarily even then.)

* We've also provided the shell of a main function, in `src/Main.hs`.  You will probably want to change code there.

At the moment, the `tests` folder just contains some whole program tests:

* [`fact_rec`](tests/fact_rec.wast) and [`fact_loop`](tests/fact_loop.wast) compute the factorial of 5, recursively or iteratively

* [`collatz_sum`](tests/collatz_sum.wast) computes the sum of the Collatz sequence from 7.

We'll add a comprehensive set of tests within the next week or so.
