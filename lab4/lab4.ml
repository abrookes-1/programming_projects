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

(*
 * (eval p) should reduce a program in initial environment env to a *value*
 * In general, if Node.js produces a *value* for an example JavaScript
 * program, your evaluator should produce that same value.
 * You should support basic JavaScript (recursive higher-order) functions,
 * but do not need to support static scope.
 * See the assignment writeup for more details.
 *)

(* evaluate a program *)
let rec eval : (environment_t*program_t) -> value_t =
fun (env, p) ->
match p with
| ExprProgram(_,e) -> eval_expr (env, e)
| StmtProgram(_,s,p2) ->
  let newEnv = eval_stmt (env, s) in
  eval (newEnv, p2)

(* evaluate a block *)
and eval_block : (environment_t*block_t) -> value_t =
fun (env, p) ->
match p with
| ReturnBlock(_,e) -> eval_expr (env, e)
| StmtBlock(_,s,b2) ->
let _ = eval_stmt (env, s) in
eval_block (env, b2)

(* evaluate a statement *)
and eval_stmt : (environment_t*stmt_t) -> environment_t =
fun (env,s) ->
match s with
| ConstStmt(_,x,e) ->
  let ev = eval_expr (env,e) in
  push_environment (env, x, (Immutable,ev))
  (* let (name_map,val_map) = env in
  val *)
| _ -> raise (UnimplementedStmt(s))

(* evaluate a value *)
and eval_expr : (environment_t*expr_t) -> value_t =
fun (env,e) ->
match e with
| ValExpr(p,v) -> v
| VarExpr(p,x) -> (* read_environment : (environment_t*string) *)
  (match read_environment (env, x) with
  | Some(access, value) -> value
  | None -> UndefVal
  )
| BopExpr(_,e1,bop,e2) ->
  let ev1 = eval_expr (env,e1) in
  let ev2 = eval_expr (env,e2) in
  (match ev1 with
  | NumVal(_) ->
    let ev1_float = (match ev1 with
    | NumVal(v) -> v
    | BoolVal(v) -> if v then 1.0 else 0.0
    | _ -> 0.0
    ) in
    let ev2_float = (match ev2 with
    | NumVal(v) -> v
    | BoolVal(v) -> if v then 1.0 else 0.0
    | _ -> 0.0
    ) in
    (match bop with
    | PlusBop -> NumVal(ev1_float +. ev2_float)
    | MinusBop -> NumVal(ev1_float -. ev2_float)
    | TimesBop -> NumVal(ev1_float *. ev2_float)
    | DivBop -> NumVal(ev1_float /. ev2_float)
    | LtBop -> BoolVal(ev1_float < ev2_float)
    | GtBop -> BoolVal(ev1_float > ev2_float)
    | LteBop -> BoolVal(ev1_float <= ev2_float)
    | GteBop -> BoolVal(ev1_float >= ev2_float)
    | EqBop -> BoolVal(ev1_float = ev2_float)
    | NeqBop -> BoolVal(ev1_float != ev2_float)
    | OrBop -> NumVal(if ev1_float != 0.0 then ev1_float else ev2_float)
    | AndBop -> UndefVal
    )
  | BoolVal(_) ->
    let ev1_float = (match ev1 with
    | NumVal(v) -> v
    | BoolVal(v) -> if v then 1.0 else 0.0
    | _ -> 0.0
    ) in
    let ev2_float = (match ev2 with
    | NumVal(v) -> v
    | BoolVal(v) -> if v then 1.0 else 0.0
    | _ -> 0.0
    ) in
    (match bop with
    | PlusBop -> NumVal(ev1_float +. ev2_float)
    | MinusBop -> NumVal(ev1_float -. ev2_float)
    | TimesBop -> NumVal(ev1_float *. ev2_float)
    | DivBop -> NumVal(ev1_float /. ev2_float)
    | LtBop -> BoolVal(ev1_float < ev2_float)
    | GtBop -> BoolVal(ev1_float > ev2_float)
    | LteBop -> BoolVal(ev1_float <= ev2_float)
    | GteBop -> BoolVal(ev1_float >= ev2_float)
    | EqBop -> BoolVal(ev1_float = ev2_float)
    | NeqBop -> BoolVal(ev1_float != ev2_float)
    | OrBop -> BoolVal(if ev1_float = 1.0 || ev2_float = 1.0 then true else false)
    | AndBop -> BoolVal(if ev1_float = 1.0 && ev2_float = 1.0 then true else false)
    )
  | StrVal(_) ->
    let ev1_str = (match ev1 with
    | StrVal(v) -> v
    | _ -> ""
    ) in
    let ev2_str = (match ev2 with
    | StrVal(v) -> v
    | _ -> ""
    ) in
    (match bop with
    | PlusBop -> StrVal(String.concat "" [ev1_str; ev2_str])
    | LtBop -> BoolVal(ev1_str < ev2_str)
    | GtBop -> BoolVal(ev1_str > ev2_str)
    | LteBop -> BoolVal(ev1_str <= ev2_str)
    | GteBop -> BoolVal(ev1_str >= ev2_str)
    | EqBop -> BoolVal(ev1_str = ev2_str)
    | NeqBop -> BoolVal(ev1_str != ev2_str)
    | _ -> UndefVal
    )
  | _ -> UndefVal (* first expr is not string or bool *)
  )
| UopExpr(_,uop,e) ->
  let ev = (match eval_expr (env,e) with
    | NumVal(v) -> v
    | BoolVal(v) -> if v then 1.0 else 0.0
    | StrVal(v) -> float_of_int (int_of_string v)
    | _ -> 0.0
  ) in
  (match uop with
    | NotUop -> BoolVal(if ev = 1.0 then false else true)
    | NegUop -> NumVal(-1.0 *. ev)
  )
| IfExpr(p,e1,e2,e3) ->
  (match eval_expr (env, e1) with
  | NumVal(v) -> if v != 0.0 then eval_expr (env,e2) else eval_expr (env,e3)
  | BoolVal(v) -> if v then eval_expr (env,e2) else eval_expr (env,e3)
  | _ -> eval_expr (env,e3)
  )
| PrintExpr(p,e) -> UndefVal (* TODO *)
| CallExpr(p,e,el) -> 
  let f = eval_expr (env, e) in
  let name_map, lambda = (match f with
    | ClosureVal(n,l) -> (n,l)
    | StrVal(a) -> raise (Not_found)
    | _ -> raise (UnimplementedExpr(e))
  ) in

  let fname, arg_names, stmt_block, return = lambda in

  let env = (match fname with
  | Some(fn) -> push_environment (env, fn, (Immutable,f))
  | None -> env
  ) in

  (* (environment_t*typed_ident_t list*expr_t list) -> environment_t *)
  let rec push_args = fun (env,idl,expl) ->
  (match idl with
  | [] -> env
  | typeid::idrest ->
    let id, _ = typeid in
    (match expl with
    | [] -> env
    | exp::exprest -> 
      let ev5 = eval_expr (env,exp) in
      let new_env1 = push_environment (env, id, (Immutable,ev5)) in
      push_args (new_env1, idrest, exprest)
    )
  ) in

  let new_env = push_args (env, arg_names, el) in

  eval_block (new_env, stmt_block)

| _ -> raise (UnimplementedExpr(e))

(* basic regression tests for the evaluator (do not modify) *)
let simple_expr_eval_tests = ("Simple Expression Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "1 + true",                     NumVal(2.0));
  (None, parse_string "false + true",                 NumVal(1.0));
  (None, parse_string "100 || 200",                   NumVal(100.0));
  (None, parse_string "-false",                       NumVal(0.0));
  (None, parse_string "1 + 1",                        NumVal(2.0));
  (None, parse_string "3 + (4 + 5)",                  NumVal(12.0));
  (None, parse_string "3 * (4 + 5)",                  NumVal(27.0));
  (None, parse_string "-6 * 90 - 8",                  NumVal(-548.0));
  (None, parse_string "-100 + 50",                    NumVal(-50.0));
  (None, parse_string "true && (false || true)",      BoolVal(true));
  (None, parse_string "true && (false || !true)",     BoolVal(false));
  (None, parse_string "1 < 2",                        BoolVal(true));
  (None, parse_string "100 === 100",                  BoolVal(true));
  (None, parse_string "100 === 101",                  BoolVal(false));
  (None, parse_string "100 !== 200",                  BoolVal(true));
  (None, parse_string "true === true",                BoolVal(true));
  (None, parse_string "0 / 0",                        NumVal(Float.nan));
  (None, parse_string "console.log(\"Hello World\")", UndefVal);
  (None, parse_string "(1 < 2) ? 123 : 124",          NumVal(123.0));
  (None, parse_string "\"aaa\" < \"aaaa\"",           BoolVal(true));
  (None, parse_string "\"bbb\" < \"aaa\"",            BoolVal(false));
  (None, parse_string "\"hello\"+\" \"+\"world\"",    StrVal("hello world"));
  (None, parse_string "const x = 1; x+1",             NumVal(2.0));
  (None, parse_string "const x=1; const y=2; x+y",    NumVal(3.0));
  (None, parse_string "const x=3; const y=x*2+1; y",  NumVal(7.0));
  (None, parse_string "!false",  BoolVal(true));
  (None, parse_string "- 1",  NumVal(-1.0));
  (None, parse_string "- - 1",  NumVal(1.0));
  (None, parse_string "const x = 1; x",  NumVal(1.0));
])

(* basic tests for the evaluator (do not modify) *)
let simple_func_eval_tests = ("Simple Function Definition Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "function test(x){const x = 123; return x}",
         ClosureVal(StringMap.empty,(
             Some("test"),
             [("x",None)],
             StmtBlock(NoPos,
                 ConstStmt(NoPos,
                     "x",
                     ValExpr(NoPos,NumVal(123.0))),
                 ReturnBlock(NoPos,VarExpr(NoPos,"x"))),
             None)));
])
(* note - you can use the following to print a program for debugging *)
(* let _ = Printf.printf "RESULT = %s\n" (str_program (parse_string "const x = 1 + 1; x * 2")) *)

let func_eval_tests = ("Function Definition Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "function test(x, y, z, a, b, c){return x + y + z}",
         ClosureVal(StringMap.empty,(
             Some("test"),
             [("x",None);
             ("y",None);
             ("z",None);
             ("a",None);
             ("b",None);
             ("c",None)],
             ReturnBlock(NoPos,
              BopExpr(
                NoPos,
                BopExpr(NoPos, VarExpr(NoPos,"x"), PlusBop, VarExpr(NoPos,"y")),
                PlusBop,
                VarExpr(NoPos,"z")
              )
             ),
             None)));
  (None, parse_string "function test(x){const x = 123123123; return 2 * x}",
        ClosureVal(StringMap.empty,(
            Some("test"),
            [("x",None)],
            StmtBlock(NoPos,
                ConstStmt(NoPos,
                    "x",
                    ValExpr(NoPos,NumVal(123123123.0))),
                ReturnBlock(NoPos,BopExpr(NoPos, ValExpr(NoPos,NumVal(2.0)), TimesBop, VarExpr(NoPos,"x")))),
            None)));
])

let simple_call_eval_tests = ("Simple Call Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "const f = function(x){return x+1}; f(1)",                    NumVal(2.0));
  (None, parse_string "const y = 5; const f = function(x){return x+1}; f(y)",       NumVal(6.0));
  (None, parse_string "const f = function t(x){return x===0 ? 0 : x+t(x-1)}; f(5)", NumVal(15.0));
  (None, parse_string "const f = function(x){return x===0 ? 0 : x+(x-1)}; f(5)", NumVal(9.0));
])

let call_eval_tests = ("Call Evaluation", (fun p -> eval (empty_env,p)), eq_value, Some(str_program,str_value), [
  (None, parse_string "const f = function(x){return x}; f(1)",                    NumVal(1.0));
  (None, parse_string "const f = function(x, y, z){return x * y * z / 2}; f(2, 6, 7)",  NumVal(42.0));
  (None, parse_string "const x = 1; const y = 2; const z=10; const sum = function(a, b, c){return a + b + c}; sum(x, y, z)",  NumVal(13.0));
  (None, parse_string "const if = function(a, b, c){return a ? b : c}; if(true, 0.0, 1.0)",  NumVal(0.0));
  (None, parse_string "const if = function(a, b, c){return None}; if(2.0, 0.0, 1.0)",  UndefVal);
  (None, parse_string "const or = function(a, b){return a || b}; const if = function(a, b, c){return a ? b : c}; if(or(true, false), \"a\", \"b\")",  StrVal("a"));
  (None, parse_string "const or = function(a, b){return a || b}; const if = function(a, b, c){return a ? b : c}; if(or(false, false), \"a\", \"b\")",  StrVal("b"));
])
