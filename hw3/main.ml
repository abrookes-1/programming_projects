open Javascript_main
open Flags
open Testing

let _ =
let i = parse_command_line () in (match i with
| Some(i) ->
    let result = get_ast i in
    ignore result;
    (* Ast.print_grammar 0 result;
    print_newline(); *)
    print_string (Javascript_ast.str_program result);
    print_newline ();
| _ -> ()
);
let (total,failed) = 
(match !Flags.test with
| Some("hw03") ->
  reset_test_counts ();
  let _ = list_check Homework03.extract_tests_int in
  let _ = list_check Homework03.extract_tests_char in
  let _ = list_check Homework03.simple_lexer_tests in
  let _ = list_check Homework03.simple_parser_tests in
  let _ = list_check Homework03.lexer_tests in
  let _ = list_check Homework03.parser_tests in
  get_test_counts ()
| _ -> (0,0)
) in
Printf.printf "total = %d; failed = %d\n" total failed;
exit 0
