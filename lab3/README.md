# CSCI-400 Lab #3 (Individual/Team Lab)

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
- Typing `make lab3` should build your Lab #3 code, and run its unit tests.
- Each unit test is a tuple of the form `(optional_name, input, expected_output)`,
  where `optional_name` can either be `None` or `Some(x)`, where `x` is a
  human-readable name for the unit test.
- Note that the generated file `lab3.types` shows the types for each of the
  functions in that file.

## Rubric

- The total score is out of 100 points.
- You will submit your own (passing) test cases for each task.
- The instructors/TAs will run your code with a "secret" test-case suite, and
  grade based on number of passing tests.
- Please provide concise documentation for each piece of code you write.

| Item                                        | Points |
|---------------------------------------------|--------|
| Task 1: submit passing test cases           |  5     |
| Task 1: pass "secret" instructor test cases | 25     |
| Task 2: submit passing test cases           |  5     |
| Task 2: pass "secret" instructor test cases | 25     |
| Task 3: submit passing test cases           |  5     |
| Task 3: pass "secret" instructor test cases | 25     |
| Documentation                               | 10     |

## Using Your Interpreter from the Command Line

- Typing `./javascript -lab3 some_file.js` will run your code using the JavaScript
  expression stored in `some_file.js` as input.
- Typing `./javascript -lab3` will do the same, but will allow you to enter a
  JavaScript expression directly rather than reading from a file (press CTRL-D
  when finished entering the expression).

## Summary

Your goal in this lab will be to add OCaml code to complete the functionality
described in `lab3.ml`.
Skeleton code is provided, and you will need to fill in the body of several
functions. Each location where you need to replace a placeholder expression
with your own code is marked with a `TODO` comment.

In this lab, we will use the skills developed in Lab 2 to continue building an
interpreter for a simple subset of JavaScript. The following is the grammar for
the Lab 3 JavaScript subset. Notice that we will now add support for *immutable variables*,
*strings*, *conditionals*, and a *console print operator*.
Notice also that the JavaScript automatic type conversions must be performed,
e.g., the program `"one" + 2` should evaluate to `"one2"`.

You can check your work by comparing your evaluator's output to that of an existing
JavaScript interpreter such as `nodejs`.

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p*

- **expression** *e* ::= *x* | *v* | *uop* *e* | *e* *bop* *e*
                | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)`

- **value** *v* ::= *n* | *b* | *s* | `undefined`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **identifier** *x*

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- **string** *s*

## Task 1

Add support for the console print operator. Evaluating a program such as
`console.log(e)` should evaluate expression `e`, and print the resulting value.

Secondly, add support for conditional expressions. This is JavaScript's "inline *if*".
Add at least 3 unit tests for this conditional functionality
to the `cond_eval_tests` list (location marked with `TODO`).

## Task 2

Add support for strings. Note that the `+`, `<`, `<=`, `>`, `>=` operators
work differently for strings versus numbers.
Add at least 3 unit tests for your string functionality
to the `str_eval_tests` list (location marked with `TODO`).

## Task 3

Add support for immutable (`const`) variables. The comments at the beginning of `lab3.ml` point
you to some functions that will be useful in manipulating the environment to
read/push variable bindings. Note that in our interpreter, we will allow "redefinition" of
variables, e.g. consider `const x = 1; const x = 2; x` to be a valid program, which
evaluates to the value `2`.

Add at least 3 unit tests for your variable-related functionality
to the `var_eval_tests` list (location marked with `TODO`).

## Documentation

- Please provide concise documentation (comments in the code) for each of the features you implement.
