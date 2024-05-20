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
The REPL accepts expressions of recursive list functions.

### Basic list functions
The following functions are available:
- `zero_left`: add zeros to the left of a list.
- `zero_right`: add zeros to the right of a list.
- `delete_left`: delete the leftmost element of a list.
- `delete_right`: delete the rightmost element of a list.
- `succ_left`: add 1 to the leftmost element of a list.
- `succ_right`: add 1 to the rightmost element of a list.

### Composing list functions
You can compose list functions to create new functions using the `.` operator. For example, `zero_left . succ_right` is a new function that adds zeros to the left of a list and then adds 1 to the rightmost element.

### Repetition of list functions
You can use the repetitions of a function by enclosing it in `{` `}`. For example, `{ f }` is a new function that apply the function `f` until the first and last element of the list are equals.

### The power operator
You can use the power operator `(_)^k` to apply a function `k` times. For example,
`(f)^3` is a new function that apply the function `f` three times.

You can find the definition of these functions in the [documentation](RLF.md).

## FList TAD
You can find the definition of the FList TAD in the [documentation](TAD.md). To use the different instance of the FList TAD in the REPL, you can pass the instance to the application of the function to the list. For example to use the instance of [Sequence](src/FList/SeqFList.hs) you can use the following expression:
```
FL> zero_left.{ succ_left }.delete_right [4,56,32,1] <Seq>
```

## Examples
There are some functions defined in the [testcases folder](test/testcases)
