# CSCI-400 Lab #2 (Team Lab)

## Setting up the Repository
- **READ THIS FIRST!!**
- **DO NOT CLONE THIS REPO!!!**
- This README and accompanying skeleton code are contained in
  the read-only *assignment* repository --
  you will be able to pull from here, but not push.
- To get started on the assignment, first go to your
  *personal/team* repository for this assignment, and
  carefully follow the `INSTRUCTIONS.md` file there.
- The basic idea is: you will first clone your personal/team
  repository (**not** this assignment repo), and then add a (read only) remote to pull
  the skeleton code from this assignment repository.

## Basic Instructions

- Typing `make` should build your code.
- Typing `make lab2` should build your Lab #2 code, and run its unit tests.
- Each unit test is a tuple of the form `(optional_name, input, expected_output)`,
  where `optional_name` can either be `None` or `Some(x)`, where `x` is a
  human-readable name for the unit test.
- Note that the generated file `lab2.types` shows the types for each of the
  functions in that file.

## Rubric

- The total score is out of 100 points.
- You will submit your own (passing) test cases for each task.
- The instructors/TAs will run your code with a "secret" test-case suite, and
  grade based on number of passing tests.
- Please provide concise documentation for each piece of code you write.

| Item                                        | Points |
|---------------------------------------------|--------|
| Task 1: submit passing test cases           | 10     |
| Task 1: pass "secret" instructor test cases | 35     |
| Task 2: submit passing test cases           | 10     |
| Task 2: pass "secret" instructor test cases | 35     |
| Documentation                               | 10     |

## Using Your Interpreter from the Command Line

- Typing `./javascript -lab2 some_file.js` will run your code using the JavaScript
  expression stored in `some_file.js` as input.
- Typing `./javascript -lab2` will do the same, but will allow you to enter a
  JavaScript expression directly rather than reading from a file (press CTRL-D
  when finished entering the expression).

## Summary

Your goal in this lab will be to add OCaml code to complete the functionality
described in `lab2.ml`.
Skeleton code is provided, and you will need to fill in the body of several
functions. Each location where you need to replace a placeholder expression
with your own code is marked with a `TODO` comment.

## Task 1

In this part of the lab, we will begin building our JavaScript interpreter.
We will start by building a function which evaluates simple JavaScript *expressions*.
For example, when given a JavaScript expression such as `1+1`, your code
should return the value `2`.

For now, we will only consider *well-typed* expressions, such as the above.
In this lab, expressions such as `true + 1` or `100 && false`
will be considered to be *malformed*,
both in our expression evaluator, and in our type checker (Task 2), and
your evaluator should return `UndefVal` for such expressions.
See the instructor-provided test cases for several examples.

Here is the Lab #2 grammar for expressions (note that this is a subset of
the JavaScript grammar discussed in class).

- **expression** *e* ::= *v* | *uop* *e* | *e* *bop* *e*

- **value** *v* ::= *n* | *b*

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- Edit `lab2.ml` at the locations indicated by `TODO` comments, and
  complete the `eval` function as described.
- Add at least 5 new *non-trivial* unit tests for this function, at
  the location indicated by the `TODO` comment.

## Task 2

Beyond evaluating expressions, you should also implement a basic bottom-up
typechecker for expressions. For example, when given a JavaScript expression
such as `1 + (2 + 3)`, your typechecker should indicate that this expression
is of type *number*.

The types needed for the above expressions are `number` and `boolean`.

A typechecking failure (e.g., a malformed input expression, as described in Task 1)
is indicated by returning `None`.

- Edit `lab2.ml` at the locations indicated by `TODO` comments, and
  complete the `typecheck` function as described.
- Add at least 5 new *non-trivial* unit tests for this function, at
  the location indicated by the `TODO` comment.

## Documentation

- Please provide concise documentation for each of the features you implement.
