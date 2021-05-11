open Javascript_ast
open Javascript_main

(*
 * Check javascript_ast.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 * - read_environment        -- look up a variable's value in the environment
 * - push_environment        -- add a variable binding to the environment
 * - empty_env               -- the empty environment
 *)

(* basic tests to show how the value conversion functions work (do not modify) *)
let simple_to_num_tests = ("Simple ToNum Conversions", to_num, (fun n1 n2 -> eq_float (n1,n2)), Some(str_value,str_float), [
  (None, NumVal(123.0), 123.0);
  (None, BoolVal(true), 1.0);
  (None, StrVal(""),    0.0);
])
let simple_to_bool_tests = ("Simple ToBool Conversions", to_bool, (=), Some(str_value,string_of_bool), [
  (None, BoolVal(true),  true);
  (None, NumVal(1.0),    true);
  (None, StrVal("true"), true);
])
let simple_to_str_tests = ("Simple ToStr Conversions", to_str, (=), Some(str_value,(fun x -> x)), [
  (None, StrVal("hello"), "hello");
  (None, BoolVal(true),   "true");
  (None, NumVal(1.234),   "1.234");
  (None, NumVal(1.000),   "1");
  (None, NumVal(0.00),    "0");
  (None, NumVal(100.01),  "100.01");
])

(*
 * (eval p) should reduce a program in initial environment env to a *value*
 * (if Node.js produces a *value* for an example JavaScript program,
 * your evaluator should produce that same value).
 *)

(* evaluate a program *)
let rec eval : (environment_t*program_t) -> value_t = fun (env, p) -> match p with
| ExprProgram(_,e) -> UndefVal (* TODO *)
| StmtProgram(_,s,p2) -> UndefVal (* TODO *)

(* evaluate a statement *)
and eval_stmt : (environment_t*stmt_t) -> environment_t = fun (env,s) -> match s with
| ConstStmt(_,x,e) -> env (* TODO *)
| _ -> raise (UnimplementedStmt(s))

(* evaluate a value *)
and eval_expr : (environment_t*expr_t) -> value_t = fun (env,e) -> match e with
| ValExpr(p,v) -> UndefVal (* TODO *)
| VarExpr(p,x) -> UndefVal (* TODO *)
| BopExpr(_,e1,AndBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,OrBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,PlusBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,MinusBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,TimesBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,DivBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,LteBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,LtBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,GteBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,GtBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,EqBop,e2) -> UndefVal (* TODO *)
| BopExpr(_,e1,NeqBop,e2) -> UndefVal (* TODO *)
| UopExpr(_,NegUop,e) -> UndefVal (* TODO *)
| UopExpr(_,NotUop,e) -> UndefVal (* TODO *)
| IfExpr(p,e1,e2,e3) -> UndefVal (* TODO *)
| PrintExpr(p,e) -> UndefVal (* TODO *)
(* other expression types unimplemented *)
| _ -> raise (UnimplementedExpr(e))

(* basic tests for the evaluator (do not modify) *)
let simple_expr_eval_tests = ("Simple Expression Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "1 + true",                    NumVal(2.0));
  (None, parse_string "false + true",                NumVal(1.0));
  (None, parse_string "100 || 200",                  NumVal(100.0));
  (None, parse_string "-false",                      NumVal(0.0));
  (None, parse_string "1 + 1",                       NumVal(2.0));
  (None, parse_string "3 + (4 + 5)",                 NumVal(12.0));
  (None, parse_string "3 * (4 + 5)",                 NumVal(27.0));
  (None, parse_string "-6 * 90 - 8",                 NumVal(-548.0));
  (None, parse_string "-100 + 50",                   NumVal(-50.0));
  (None, parse_string "true && (false || true)",     BoolVal(true));
  (None, parse_string "true && (false || !true)",    BoolVal(false));
  (None, parse_string "1 < 2",                       BoolVal(true));
  (None, parse_string "100 === 100",                 BoolVal(true));
  (None, parse_string "100 === 101",                 BoolVal(false));
  (None, parse_string "100 !== 200",                 BoolVal(true));
  (None, parse_string "true === true",               BoolVal(true));
])
let simple_print_eval_tests = ("Simple Print Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "console.log(\"\")",           UndefVal);
])
let simple_cond_eval_tests = ("Simple Conditional Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "(1 < 2) ? 123 : 124",         NumVal(123.0));
])
let simple_str_eval_tests = ("Simple String Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "\"aaa\" < \"aaaa\"",          BoolVal(true));
  (None, parse_string "\"bbb\" < \"aaa\"",           BoolVal(false));
  (None, parse_string "\"hello\"+\" \"+\"world\"",   StrVal("hello world"));
])
let simple_var_eval_tests = ("Simple Variable Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "const x = 1; x+1",            NumVal(2.0));
  (None, parse_string "const x=1; const y=2; x+y",   NumVal(3.0));
  (None, parse_string "const x=3; const y=x*2+1; y", NumVal(7.0));
])
(* note - you can use the following to print a program for debugging *)
(* let _ = Printf.printf "RESULT = %s\n" (str_program (parse_string "const x = 1 + 1; x * 2")) *)

let cond_eval_tests = ("Conditional Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (* TODO *)
])
let str_eval_tests = ("String Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (* TODO *)
])
let var_eval_tests = ("Variable Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (* TODO *)
])
