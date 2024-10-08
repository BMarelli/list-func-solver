# Recursive List Functions

A DSL that parses and evaluates expressions of recursive list functions (_RLF_). The interpreter is implemented in Haskell.

For more information about _RLF_, go to the [documentation](docs/RLF.md).
Also see (_in spanish_) the chapter 4 in the [paper](https://dcc.fceia.unr.edu.ar/sites/default/files/uploads/materias/Libro_3_Mar.pdf).

## Installation

1. Clone the repository.
2. `cd` into the repository.
3. Run the script that is in the [setup](./setup) folder with the command `./setup/*.setup.sh`.

Once the docker container is running, you can access the REPL by running `cabal run`.

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

You can find the definition of these functions in the [documentation](docs/RLF.md).

### Printing the result

To print the _ASCII_ representation of the result of an expression, you can use the `print` operator. This operator print the result of the expression and return it. For example:

```
FL> print succ_left [103,101,108,108,111,32,119,111,114,108,100,33]
hello world!
[104,101,108,108,111,32,119,111,114,108,100,33]
```

### Creating local variables in expressions

You can create local variables in expressions using the `let in` syntax. This syntax allows you to store a value in a local variable and use it in the resto of the expression. For example:

```
FL> let x = succ_left in x [1,2,3,4]
[2,2,3,4]

FL> swap (let _x = succ_left [1,2,3,4] in zero_right _x)
[0,2,3,4,2]
```

## FList ADT

You can find the definition of the FList ADT in the [documentation](docs/ADT.md). To use the different instance of the FList ADT in the REPL, you can pass the instance to the application of the function to the list using `<` `>`. For example to use the instance of [Sequence](src/FList/Sequence.hs) you can use the following expression:

```
FL> zero_left.{ succ_left }.delete_right [4,56,32,1] <Seq>
[1,4,56,32]
```

## Examples

There are some functions defined in the [testcases folder](test/testcases)
