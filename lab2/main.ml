open Javascript_ast
open Javascript_main
open Flags
open Testing

let get_input_ast i =
  let i = (match i with
  | Some(i) -> Printf.printf "reading from file...\n"; i
  | None -> Printf.printf "reading from stdin (press CTRL+D when finished)...\n"; flush stdout; stdin) in
  let result = get_ast i in
  result

let _ =
let i = parse_command_line () in 
(* run unit tests *)
if !Flags.test then (
  let (total,failed) = 
  (List.fold_left (fun acc mode ->
  Printf.printf "running %s tests...\n" (get_assignment_desc mode);
  match mode with
  | (Lab2) ->
    reset_test_counts ();
    let _ = list_check Lab2.simple_eval_tests in
    let _ = list_check Lab2.simple_typecheck_tests in
    let _ = list_check Lab2.eval_tests in
    let _ = list_check Lab2.typecheck_tests in
    get_test_counts acc
  ) (0,0) !Flags.mode) in
  Printf.printf "total = %d; failed = %d\n" total failed
) else (
  let input_ast = get_input_ast i in
  Printf.printf "parsed input: %s\n" (Javascript_ast.str_program input_ast);
  List.iter (fun mode ->
  Printf.printf "%s result...\n" (get_assignment_desc mode);
  let result = (match mode with
  | Lab2 ->
    let e = (Lab2.get_expr input_ast) in
    let eval = Javascript_ast.str_value (Lab2.eval e) in
    let typ = str_option Javascript_ast.str_typ (Lab2.typecheck e) in
    Printf.sprintf "eval: %s\ntype: %s" eval typ
  ) in
  Printf.printf "%s\n" result
  ) !Flags.mode
);
exit 0
