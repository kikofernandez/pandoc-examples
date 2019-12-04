# Type checker implementation

This is part of the supplementary material.
This folder contains the code for the type checker as well as a simple test.

## Code structure

The code structure follows the common `stack` project structure.
The `src` folder contains the different extensions to the compiler.
All the extensions contain two files: `Typechecker.hs` and `AST.hs`.
The `Typechecker.hs` contains the type checking code;
the `AST.hs` contains the abstract syntax tree of the language as well as
helper functions.

If you wish to follow the extension order as defined in the paper,
please read the code in the folders according to this order:

1. `Initial`
2. `Reader`
3. `Backtrace`
4. `Warning`
5.1. `MultiError`
5.2 `Applicative`
6. `PhantomPhases`
7. `Final`, contains all the extensions

## Test

To build the project, one needs to have the following requirements:

- `stack`

If you do not have `stack`, please follow their
[installation instructions](https://docs.haskellstack.org/en/stable/README/).

To run the simple tests, just type `stack test`.
The tests showcase how the compiler gives more information as we add more
extensions. The use of the phantom types to type state cannot be shown,
but the reader is welcome to visit the file `PhantomPhases/Typechecker.hs`
and test our example from Section 7 of the paper (running `stack build` to
check that GHC throws an error).
