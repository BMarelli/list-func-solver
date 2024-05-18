# Recursive List Functions

A Haskell embedded DSL that parses and evaluates expressions of recursive list functions.

For more information, see the [documentation](RLF.md).
Also see in spanish the chapter 4 in the [paper](https://dcc.fceia.unr.edu.ar/sites/default/files/uploads/materias/Libro_3_Mar.pdf).

## Installation
1. Clone the repository.
2. `cd` into the repository and run `stack setup`.
3. Run `stack build`.
4. Run `stack run` to start the REPL.

## Usage
The REPL accepts expressions of recursive list functions. The following functions are available:
- `zero_left`: add zeros to the left of a list.
- `zero_right`: add zeros to the right of a list.
- `delete_left`: delete the leftmost element of a list.
- `delete_right`: delete the rightmost element of a list.
- `succ_left`: add 1 to the leftmost element of a list.
- `succ_right`: add 1 to the rightmost element of a list.

You can find the definition of these functions in the [documentation](RLF.md).

## Examples
There are some functions defined in the [testcases folder](test/testcases)
