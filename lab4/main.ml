open Javascript_ast
open Javascript_main
open Flags
open Testing

(* https://stackoverflow.com/a/61348066/1613162 *)
let my_really_read_string in_chan =
    let res = Buffer.create 1024 in
    let rec loop () =
        match input_line in_chan with
        | line ->
            Buffer.add_string res line;
            Buffer.add_string res "\n";
            loop ()
        | exception End_of_file -> Buffer.contents res
    in
    loop ()

let get_channel i =
  let i = (match i with
  | Some(i) -> Printf.printf "reading from file...\n"; i
  | None -> Printf.printf "reading from stdin (press CTRL+D when finished)...\n"; flush stdout; stdin) in
  i

let get_input_ast i =
  let result = get_ast (get_channel i) in
  result

let get_input_str i =
  let i = get_channel i in
  my_really_read_string i

let _ =
let i = parse_command_line () in 
(* run unit tests *)
if !Flags.test then (
  let (total,failed) = 
  (List.fold_left (fun acc mode ->
  Printf.printf "running %s tests...\n" (get_assignment_desc mode);
  match mode with
  | (Lab4) ->
    reset_test_counts ();
    let _ = list_check Lab4.simple_expr_eval_tests in
    let _ = list_check Lab4.simple_func_eval_tests in
    let _ = list_check Lab4.simple_call_eval_tests in
    let _ = list_check Lab4.func_eval_tests in
    let _ = list_check Lab4.call_eval_tests in
    get_test_counts acc
  ) (0,0) !Flags.mode) in
  Printf.printf "total = %d; failed = %d\n" total failed
) else (
  let input_str = get_input_str i in
  List.iter (fun mode ->
    Printf.printf "Running %s...\n" (get_assignment_desc mode);
    let result = (match mode with
    | (Lab4) ->
      let input_ast = parse_string input_str in
      Printf.printf "parsed input: %s\n" (Javascript_ast.str_program input_ast);
      let v = Lab4.eval (empty_env,input_ast) in
      let v_num = to_num v in
      let v_bool = to_bool v in
      let v_str = to_str v in
      let eval = Javascript_ast.str_value v in
      Printf.sprintf "------------------------------\nEVAL:    %s\n------------------------------\nto_num:  %s\nto_bool: %s\nto_str:  %s\n"(*"\nfinal env:\n%s\n"*)
        eval (str_float v_num) (string_of_bool v_bool) v_str (*(str_option str_environment !Lab4.final_env)*)
    ) in
    Printf.printf "%s\n" result
  ) !Flags.mode
);
exit 0
