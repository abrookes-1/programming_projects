open Javascript_utils
open Javascript_ast
open Javascript_main

(****************************************)
(** Expression Evaluator functionality **)
(****************************************)

(* exception indicating that a program is more
 * than just a single expression *)
exception NotExpr of program_t
(* exception indicating unimplemented input
 * expression *)
exception Unimplemented of expr_t 

(* add printer for above exceptions *)
let _ = Printexc.register_printer (fun ex -> match ex with
| NotExpr(p) -> Some(Printf.sprintf "NotExpr(%s)" (str_program p))
| Unimplemented(e) -> Some(Printf.sprintf "Unimplemented(%s)" (str_expr e))
| _ -> None)

(* extract expression from a single-expression program *)
let get_expr = fun p ->
  match p with
  | ExprProgram(_,e) -> e
  | _ -> raise (NotExpr(p))

(* parse input string as an expression *)
let parse_expr = fun s ->
  let p = parse_string s in
  get_expr p

(* (eval e) should convert input expression e into a *value* if e is
 * a well-formed expression (otherwise, UndefExpr should be returned). *)
let rec eval = fun e -> match e with
| ValExpr(p,v) ->
   v
| UopExpr(p,NotUop,ValExpr(_,v)) ->             (* ! *)
  (match v with
  | BoolVal(b1) ->
    let n = not b1 in BoolVal(n)
  | _ -> UndefVal)
| UopExpr(p,NegUop,ValExpr(_,v)) ->             (* - *)
  (match v with
  | NumVal(n1) ->
    let n = -. n1 in NumVal(n)
  | _ -> UndefVal)
| UopExpr(p,uop,e) ->                           (* nested uop expression *)
  let value = eval e in
  let valExpr = ValExpr(NoPos,value) in
  let expr = UopExpr(p,uop,valExpr) in
  eval expr
| BopExpr(p,ValExpr(_,v1),PlusBop,ValExpr(_,v2)) -> (* + *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = n1 +. n2 in NumVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),TimesBop,ValExpr(_,v2)) -> (* * *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = n1 *. n2 in NumVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),MinusBop,ValExpr(_,v2)) -> (* - *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = n1 -. n2 in NumVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),DivBop,ValExpr(_,v2)) -> (* / *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = n1 /. n2 in NumVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),EqBop,ValExpr(_,v2)) -> (* === *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = (n1 = n2) in BoolVal(n)
  | (StrVal(s1),StrVal(s2)) ->
    let n = (s1 = s2) in BoolVal(n)
  | (BoolVal(b1),BoolVal(b2)) ->
    let n = (b1 = b2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),NeqBop,ValExpr(_,v2)) -> (* !== *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = (n1 <> n2) in BoolVal(n)
  | (StrVal(s1),StrVal(s2)) ->
    let n = (s1 <> s2) in BoolVal(n)
  | (BoolVal(b1),BoolVal(b2)) ->
    let n = (b1 <> b2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),LtBop,ValExpr(_,v2)) -> (* < *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = (n1 < n2) in BoolVal(n)
  | (StrVal(s1),StrVal(s2)) ->
    let n = (s1 < s2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),LteBop,ValExpr(_,v2)) -> (* <= *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = (n1 <= n2) in BoolVal(n)
  | (StrVal(s1),StrVal(s2)) ->
    let n = (s1 <= s2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),GtBop,ValExpr(_,v2)) -> (* > *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = (n1 > n2) in BoolVal(n)
  | (StrVal(s1),StrVal(s2)) ->
    let n = (s1 > s2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),GteBop,ValExpr(_,v2)) -> (* >= *)
  (match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) ->
    let n = (n1 >= n2) in BoolVal(n)
  | (StrVal(s1),StrVal(s2)) ->
    let n = (s1 >= s2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),OrBop,ValExpr(_,v2)) -> (* || *)
  (match (v1,v2) with
  | (BoolVal(b1),BoolVal(b2)) ->
    let n = (b1 || b2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,ValExpr(_,v1),AndBop,ValExpr(_,v2)) -> (* && *)
  (match (v1,v2) with
  | (BoolVal(b1),BoolVal(b2)) ->
    let n = (b1 && b2) in BoolVal(n)
  | _ -> UndefVal)
| BopExpr(p,e1,bop,e2) ->                         (* bop nested *)
  let v1 = eval e1 in
  let v2 = eval e2 in
  let ve1 = ValExpr(NoPos,v1) in
  let ve2 = ValExpr(NoPos,v2) in
  let expr = BopExpr(p,ve1,bop,ve2) in
  (* if bop = TimesBop || bop = DivBop || bop = PlusBop || bop = MinusBop || bop = AndBop || bop = OrBop then *)
    eval expr
  (* else
    let _ = Printf.printf "*******EXPR = %s\n" (str_expr expr) in
    UndefVal *)
(* other expression types unimplemented *)
| _ -> raise (Unimplemented(e))

(* basic tests for the evaluator (do not modify) *)
let simple_eval_tests = ("Simple Evaluator", eval, eq_value, Some(str_expr,str_value), [
  (None, parse_expr "1 + true",                 UndefVal); (* 1 *)
  (None, parse_expr "false + true",             UndefVal);
  (None, parse_expr "100 || 200",               UndefVal);
  (None, parse_expr "-false",                   UndefVal);
  (None, parse_expr "1 + 1",                    NumVal(2.0)); (* 5 *)
  (None, parse_expr "3 + (4 + 5)",              NumVal(12.0));
  (None, parse_expr "3 * (4 + 5)",              NumVal(27.0));
  (None, parse_expr "-100 + 50",                NumVal(-50.0));
  (None, parse_expr "true && (false || true)",  BoolVal(true));
  (None, parse_expr "true && (false || !true)", BoolVal(false)); (* 10 *)
  (None, parse_expr "1 < 2",                    BoolVal(true));
  (None, parse_expr "100 === 100",              BoolVal(true));
  (None, parse_expr "100 === 101",              BoolVal(false));
  (None, parse_expr "100 !== 200",              BoolVal(true));
  (None, parse_expr "true === true",            BoolVal(true)); (* 15 *)
])
(* note - you can use the following to print an expression for debugging *)
(* let _ = Printf.printf "RESULT = %s\n" (str_expr (parse_expr "(789698768787687687 <= 234523452345) || (\"this\" === \"that\")")) *)

let eval_tests = ("Evaluator", eval, eq_value, Some(str_expr,str_value), [
  (None, parse_expr "1 + (400 / 400) * 20",                             NumVal(21.0));               (* 1 *)
  (None, parse_expr "(1000 * 1000) > (1000 / 1000)",                    BoolVal(true));
  (None, parse_expr "(09870987 <= 09870987) && (09870987 >= 09870987)", BoolVal(true));
  (None, parse_expr "(09870987 <= 09870987) && (09870987 >= 09870987)", BoolVal(true));
  (None, parse_expr "!true + 162 + 987987",                             UndefVal);
  (None, parse_expr "!false || (162 >= 987987)",                        BoolVal(true));
  (None, parse_expr "(09870987 <= 09870987) && (09870987 >= 09870987)", BoolVal(true));
  (None, parse_expr "234523452345 && 23523452345 + 90809879087",        UndefVal);
  (None, parse_expr "\"one\" + \"two\" + \"three\" + \"four\"",         UndefVal);
  (None, parse_expr "\"one\" < 2",                                      UndefVal);                   (* 10 *)
  (None, parse_expr "\"one\" < \"two\"",                                BoolVal(true));
  (None, parse_expr "\"one\" <= \"one\"",                               BoolVal(true));
])

(****************************************)
(** Typechecker functionality          **)
(****************************************)

(* (typecheck e) should typecheck expression e, and
 * return Some(t) if e is a well-formed expression having
 * type t (otherwise, None should be returned). *)
let rec typecheck = fun e -> match e with
| ValExpr(p,NumVal(_)) ->
  Some(NumType)
| ValExpr(p,BoolVal(_)) ->
  Some(BoolType)
| ValExpr(p,StrVal(_)) ->
  Some(StrType)
| UopExpr(p,NotUop,e) ->                    (* ! *)
  let etype = typecheck e in
  (match etype with
  | Some(BoolType) -> Some(BoolType)
  | _ -> None)
| UopExpr(p,NegUop,e) ->                    (* - *)
  let etype = typecheck e in
  (match etype with
  | Some(NumType) -> Some(NumType)
  | _ -> None)
| BopExpr(p,e1,PlusBop,e2) ->                (* + *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(NumType)
  | _ -> None)
| BopExpr(p,e1,MinusBop,e2) ->                (* - *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(NumType)
  | _ -> None)
| BopExpr(p,e1,TimesBop,e2) ->                (* * *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(NumType)
  | _ -> None)
| BopExpr(p,e1,DivBop,e2) ->                (* / *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(NumType)
  | _ -> None)
| BopExpr(p,e1,EqBop,e2) ->                (* === *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(BoolType)
  | Some(BoolType), Some(BoolType) -> Some(BoolType)
  | Some(StrType), Some(StrType) -> Some(BoolType)
  | _ -> None)
| BopExpr(p,e1,NeqBop,e2) ->                (* !== *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(BoolType)
  | Some(BoolType), Some(BoolType) -> Some(BoolType)
  | Some(StrType), Some(StrType) -> Some(BoolType)
  | _ -> None)
| BopExpr(p,e1,LtBop,e2) ->                (* < *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(BoolType)
  | Some(StrType), Some(StrType) -> Some(BoolType)
  | _ -> None)
| BopExpr(p,e1,GtBop,e2) ->                (* > *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(BoolType)
  | Some(StrType), Some(StrType) -> Some(BoolType)
  | _ -> None)
| BopExpr(p,e1,LteBop,e2) ->                (* <= *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(BoolType)
  | Some(StrType), Some(StrType) -> Some(BoolType)
  | _ -> None)
| BopExpr(p,e1,GteBop,e2) ->                (* >= *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(NumType), Some(NumType) -> Some(BoolType)
  | Some(StrType), Some(StrType) -> Some(BoolType)
  | _ -> None)
| BopExpr(p,e1,AndBop,e2) ->                (* && *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(BoolType), Some(BoolType) -> Some(BoolType)
  | _ -> None)
| BopExpr(p,e1,OrBop,e2) ->                (* || *)
  let e1type = typecheck e1 in
  let e2type = typecheck e2 in
  (match (e1type, e2type) with
  | Some(BoolType), Some(BoolType) -> Some(BoolType)
  | _ -> None)
(* other expression types unimplemented *)
| _ -> raise (Unimplemented(e))

(* basic tests for the typechecker (do not modify) *)
let simple_typecheck_tests = ("Simple Typechecker", typecheck, ((=) : typ_t option -> typ_t option -> bool), Some(str_expr,str_option str_typ), [
  (Some("malformed1"), parse_expr "3 + (true + 5)",       None);
  (Some("malformed2"), parse_expr "false < true",         None);
  (Some("simple add"), parse_expr "1 + 1",                Some(NumType));
  (Some("right add"),  parse_expr "3 + (4 + 5)",          Some(NumType));
  (Some("num equal"),  parse_expr "100 === 200",          Some(BoolType));
  (Some("bool equal"), parse_expr "true === true",        Some(BoolType));
  (Some("comparison"), parse_expr "false || (100 < 200)", Some(BoolType));
])

let typecheck_tests = ("Typechecker", typecheck, ((=) : typ_t option -> typ_t option -> bool), Some(str_expr,str_option str_typ), [
  (None, parse_expr "-1",                                                                                     Some(NumType));
  (None, parse_expr "!true",                                                                                  Some(BoolType));
  (None, parse_expr "!!!!!!!!!!!!!!!!!!!!!true",                                                              Some(BoolType));
  (None, parse_expr "(true && false && (true || false)) || true && true && false && (true && false && true)", Some(BoolType));
  (None, parse_expr "(789698768787687687 <= 234523452345) || (1234 === 1234)",                                Some(BoolType));
  (None, parse_expr "(789698768787687687 <= 234523452345) || (1234 === false)",                               None);
  (None, parse_expr "\"this\" === \"that\"",                                                                  Some(BoolType));
  (None, parse_expr "(-1) + (-1)",                                                                            Some(NumType));
  (None, parse_expr "\"this\" < \"this\"",                                                                    Some(BoolType));
  (None, parse_expr "\"this\" + \"this\" + \"this\" + (\"this\" + 1)",                                        None);
])
