# CSCI-400 Homework #3

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
  repository (NOT this assignment repo), and then add a (read only) remote to pull
  the skeleton code from this assignment repository.

## Basic Instructions

- Typing `make` should build your code.
- Typing `make hw03` should build your Homework #3 code, and run its unit tests.
- Each unit test is a tuple of the form `(optional_name, input, expected_output)`,
  where `optional_name` can either be `None` or `Some(x)`, where `x` is a
  human-readable name for the unit test.

## Task 1:

- Complete the lexer functionality in `homework03.ml`
  (places where you need to add/replace code are labeled with `TODO`).
- Use only the functions provided in the skeleton code -- do not use
  any OCaml library functions!
- Add at least 5 new *non-trivial* test cases for the
  Lexer functionality.

## Task 2:

- Complete the Parser functionality in `homework03.ml`
  (places where you need to add/replace code are labeled with `TODO`).
- The grammar is a small subset of the JavaScript expression grammar
  discussed in class (see the comments in `homework03.ml` for details).
- Use a top-down (recursive descent) parsing strategy such as the one
  described in class.
- Use only the functions provided in the skeleton code -- do not use
  any OCaml library functions!
- The abstract syntax tree (AST) data structure returned by the parser
  is found in `javascript_ast.ml` (see the examples in the instructor-
  provided test cases for the parser).
- Add at least 5 new *non-trivial* test cases for the Parser functionality.

