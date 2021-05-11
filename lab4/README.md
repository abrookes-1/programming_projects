# CSCI-400 Lab #4 (Individual/Team Lab)

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
  repository (**not** this assignment repo), and then add a
  (read only) remote to pull
  the skeleton code from this assignment repository.

## Basic Instructions

- Typing `make` should build your code.
- Typing `make lab4` should build your Lab #4 code, and run its unit tests.
- Each unit test is a tuple of the form `(optional_name, input, expected_output)`,
  where `optional_name` can either be `None` or `Some(x)`, where `x` is a
  human-readable name for the unit test.
- Note that the generated file `lab4.types` shows the types for each of the
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

- Typing `./javascript -lab4 some_file.js` will run your code using the JavaScript
  expression stored in `some_file.js` as input.
- Typing `./javascript -lab4` will do the same, but will allow you to enter a
  JavaScript expression directly rather than reading from a file (press CTRL-D
  when finished entering the expression).

## Summary

Your goal in this lab will be to add OCaml code to complete the functionality
described in `lab4.ml`.
Skeleton code is provided, and you will need to fill in the body of several
functions. Each location where you need to replace a placeholder expression
with your own code is marked with a `TODO` comment.

In this lab, we will use the skills developed in Lab 3 to continue building a
"big-step" interpreter for a simple subset of JavaScript. The following is
the grammar for the Lab 4 JavaScript subset. Notice that we will now add
support for *recursive higher-order functions*, and *function calls*.
In this lab, you do not need to properly support *static scope*, since
as we have seen in lecture, in order to handle this properly, we would need to
move to the small-step semantics. To put this in concrete terms, with
proper static scope, the following code should produce `11`, but our big-step
evaluation rules cause `13` to be produced (*dynamic scope*), which is
fine for the purposes of this lab.
```
const x = 5;
const f = function(y){ return x + y };
(function(z) { const x = 7; return f(6) })(0)
```

You can check your work by comparing your evaluator's output to that of an
existing JavaScript interpreter such as `nodejs`.

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p*

- **block** *bl* ::= `return` *e* | `const` *x* `=` *e* `;` *bl*

- **expression** *e* ::= *x* | *v* | *uop* *e* | *e* *bop* *e*
                | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)` | *e* `(` *e* `)`

- **value** *v* ::= *n* | *b* | *s* | `undefined` | `function` *x* `(` *x* `)` `{` *bl* `}`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **identifier** *x*

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- **string** *s*

## Task 1

Add support for function definitions. Evaluating a program such as
`const f = function(x){ return x+1 }; console.log("Function: "+f)` should
evaluate the program, and pretty-print the function.

Add at least 2 unit tests for this functionality
to the `func_eval_tests` list (location marked with `TODO`).

## Task 2

Add support for function calls. For example, evaluating the following
```
const f = function(x){ return x+1 }; const r = f(2); r+3
```
should result in the value `6`.

Note: you can use the Node.js JavaScript interpreter to check the results of
your interpreter (this is the `nodejs` or `node` command on Linux).

Add at least 5 unit tests for your function-call functionality
to the `call_eval_tests` list (location marked with `TODO`).

## Documentation

- Please provide concise documentation (comments in the code) for each
  of the features you implement.
