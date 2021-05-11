open Javascript_ast
open Javascript_utils

(* convert string to number (float) *)
let string_to_num s = float_of_string s

(* check if a character is a numeric digit *)
let is_digit = fun d -> match d with
| '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
| _ -> false

(* convert character to string *)
let char_to_string = fun c -> (String.make 1 c)

(* get the first character of a string *)
let string_head = fun s -> s.[0]
(* get remaining characters after first character *)
let string_tail = fun s -> String.sub s 1 ((String.length s) - 1)

(* convert list of characters to string *)
let rec char_list_to_string = fun stream -> match stream with
| [] -> ""
| a::more -> ((String.make 1 a)^(char_list_to_string more))

(* convert string to list of characters *)
let rec string_to_char_list = fun st -> match st with
| "" -> []
| _ -> (string_head st)::(string_to_char_list (string_tail st))

(* extract characters c from beginning of stream until (f c) becomes false *)
let extract = fun (stream,f) ->
  let rec helper = fun (st,active) ->
    match st with
    | [] -> ([],[])
    | a::more ->
      let b = (f a) && active in
      let (x,y) = helper (more,b) in
      if b then ((a::x),y) else (x,(a::y))
    in
  helper (stream,true)

(* examples of extracting from int list *)
let extract_tests_int = ("Extract (Int)", extract, [
  (None, ([],fun x -> x=0), ([],[]));
  (None, ([1;2;3;4;5;6],fun x -> false), ([],[1;2;3;4;5;6]));
  (None, ([1;2;3;4;5;6],fun x -> true), ([1;2;3;4;5;6],[]));
  (None, ([1;2;3;4;5],fun x -> x<4), ([1;2;3],[4;5]));
  (None, ([1;2;3;4;5],fun x -> x<5), ([1;2;3;4],[5]))
])
(* examples of extracting from char list *)
let extract_tests_char = ("Extract (Char)", extract, [
  (None, (['a';'b';'c'],fun x -> false), ([],['a';'b';'c']));
  (None, (['1';'.';'2';'y'],fun x -> is_digit x || x=='.'), (['1';'.';'2'],['y']));
])

(*************************************************************)
(** Lexer functionality                                     **)
(*************************************************************)

(* type for tokens *)
type token =
| KeywordToken of string
| OpToken of string
| NumToken of string
| StrToken of string

exception LexingError of char

let rec str_token = fun t -> match t with
| KeywordToken(s) -> Printf.sprintf "Keyword(%s)" s
| OpToken(s) -> Printf.sprintf "Op(%s)" s
| NumToken(s) -> Printf.sprintf "Num(%s)" s
| StrToken(s) -> Printf.sprintf "Str(%s)" s

let str_token_list = fun l -> "["^(str_x_list str_token l ",")^"]"

(* input:  list of characters
 * output: list of tokens
 *)
let rec lexer = fun stream -> match stream with
| [] -> []
| 't'::'r'::'u'::'e'::more -> KeywordToken("true")::(lexer more)
| 'f'::'a'::'l'::'s'::'e'::more -> KeywordToken("false")::(lexer more)
| '&'::'&'::more -> OpToken("&&")::(lexer more)
| '|'::'|'::more -> OpToken("||")::(lexer more)
| '+'::more -> OpToken("+")::(lexer more)
| '-'::more -> OpToken("-")::(lexer more)
| '*'::more -> OpToken("*")::(lexer more)
| '/'::more -> OpToken("/")::(lexer more)
| ' '::more -> (lexer more)
| '\t'::more -> (lexer more)
| '\r'::more -> (lexer more)
| '\n'::more -> (lexer more)
| '"'::more ->
  let (x,y) = extract (more,fun x -> x<>'"') in
  StrToken(char_list_to_string x)::(match y with [] -> [] | _::more -> lexer more)
| ch::more ->
  if is_digit ch then
    let (x,y) = extract (more,fun x -> is_digit x || x=='.') in
    NumToken(char_list_to_string ([ch]@x))::(lexer y)
  else
    (* throw out unmatched chars *)
    (lexer more)

(* basic tests for the lexer (do not modify) *)
let simple_lexer_tests = ("Simple Lexer", lexer, [
  (None, string_to_char_list "1.234truefalse  \"one two\"123", [NumToken("1.234"); KeywordToken("true"); KeywordToken("false"); StrToken("one two"); NumToken("123")]);
  (None, string_to_char_list "1.234 + 2 + 6 +0+1", [NumToken("1.234"); OpToken("+"); NumToken("2"); OpToken("+"); NumToken("6"); OpToken("+"); NumToken("0"); OpToken("+"); NumToken("1")]);
  (None, string_to_char_list "1", [NumToken("1")]);
  (None, string_to_char_list "\"string\"", [StrToken("string")]);
  (None, string_to_char_list "\"one\"\"two\"", [StrToken("one");StrToken("two")])
])

let lexer_tests = ("Lexer", lexer, [
  (None, string_to_char_list "\"one\" \"plus\" + \"İİİİİİIIII\"", [StrToken("one");StrToken("plus");OpToken("+");StrToken("İİİİİİIIII");]);
  (None, string_to_char_list "++ + ++    \r\n\r", [OpToken("+");OpToken("+");OpToken("+");OpToken("+");OpToken("+");]);
  (None, string_to_char_list "\"asdfasdf\" \"asdflkj\"  \"asdfasdf\"", [StrToken("asdfasdf");StrToken("asdflkj");StrToken("asdfasdf")]);
  (None, string_to_char_list "\"asdfasdf\" \"asdflkj\" 121345.12345+ \"asdfasdf\"", [StrToken("asdfasdf");StrToken("asdflkj");NumToken("121345.12345");OpToken("+");StrToken("asdfasdf")]);
  (None, string_to_char_list "\"     \" ++\t\r \r\n \"    asdfasdf  asdfasdf   \"", [StrToken("     ");OpToken("+");OpToken("+");StrToken("    asdfasdf  asdfasdf   ")]);
])

(*************************************************************)
(** Parser functionality                                    **)
(*************************************************************)

let str_parser_result = fun (a,b) ->
  Printf.sprintf "%s, %s" (str_option str_expr a) (str_token_list b)

(* Parser should recognize the following grammar, using a
 * top-down (recursive descent) strategy.
 * Note that parse_expr handles the "expr" nonterminal,
 * parse_times_expr handles the "times_expr" nonterminal, etc.:
 *
 *       expr       ::= times_exp | times_expr plus_op expr
 *       times_expr ::= unary_exp | unary_expr times_op times_expr
 *       unary_expr ::= const | unary_op const
 *       plus_op    ::= "+" | "-" | "||"
 *       times_op   ::= "*" | "/" | "&&"
 *       unary_op   ::= "!" | "-"
 *       const      ::= "true" | "false" | number | string
 *)
(* TODO *)
let rec parser = fun stream ->
  parse_expr stream

and parse_expr = fun stream -> match (parse_expr1 stream) with
| (Some(e),more) -> (Some(e),more)
| _ -> parse_times_expr stream

and parse_times_expr = fun stream -> match (parse_expr2 stream) with
(* | NumToken(x)::more -> (Some(ValExpr(NoPos,NumVal(string_to_num x))),more)
| _ -> (None,stream) *)
| (Some(e),more) -> (Some(e),more)
| _ -> parse_expr stream

and parse_expr1 = fun stream ->
match (parse_times_expr stream) with
| (Some(e1),st2) -> (match (parse_plus_op st2) with
  | (Some(op),st3) -> (match (parse_expr st3) with
    | (Some(e2),st4) -> (Some(BopExpr(NoPos,e1,op,e2)),st4)
    | _ -> (None,stream)
  )
  | _ -> (None,stream)
)
| _ -> (None,stream)

and parse_expr2 = fun stream ->
match (parse_times_expr stream) with
| (Some(e1),st2) -> (match (parse_times_op st2) with
  | (Some(op),st3) -> (match (parse_times_expr st3) with
    | (Some(e2),st4) -> (Some(BopExpr(NoPos,e1,op,e2)),st4)
    | _ -> (None,stream)
  )
  | _ -> (None,stream)
)
| _ -> (None,stream)

and parse_plus_op = fun stream -> match stream with
| OpToken("+")::more -> (Some(PlusBop),more)
| OpToken("-")::more -> (Some(MinusBop),more)
| OpToken("||")::more -> (Some(OrBop),more)
(* | OpToken("*")::more -> (Some(TimesBop),more)
| OpToken("/")::more -> (Some(DivBop),more)
| OpToken("&&")::more -> (Some(AndBop),more) *)
| _ -> (None,stream)

and parse_times_op = fun stream -> match stream with
| OpToken("*")::more -> (Some(TimesBop),more)
| OpToken("/")::more -> (Some(DivBop),more)
| OpToken("&&")::more -> (Some(AndBop),more)
| _ -> (None,stream)

(* basic parser tests (do not modify) *)
let simple_parser_tests = ("Simple Parser", parser, [
  (None, [NumToken("123");OpToken("-");NumToken("23")], (Some(BopExpr(NoPos,ValExpr(NoPos,NumVal(123.0)),MinusBop,ValExpr(NoPos,NumVal(23.0)))),[]));
  (None, lexer (string_to_char_list "1 + 2 + 3 * 5 + 6"), (Some(BopExpr(NoPos,ValExpr(NoPos,NumVal(1.0)),PlusBop,BopExpr(NoPos,ValExpr(NoPos,NumVal(2.0)),PlusBop,BopExpr(NoPos,BopExpr(NoPos,ValExpr(NoPos,NumVal(3.0)),TimesBop,ValExpr(NoPos,NumVal(5.0))),PlusBop,ValExpr(NoPos,NumVal(6.0)))))),[]))
])

let parser_tests = ("Parser", parser, [
  (* (None, lexer (string_to_char_list "123 - 23 + 26"), (Some(BopExpr(NoPos,ValExpr(NoPos,NumVal(123.0)),MinusBop,BopExpr(NoPos,ValExpr(NoPos,NumVal(23.0))),PlusBop,ValExpr(NoPos,NumVal(26.0)))),[])); *)
])

let input = lexer (string_to_char_list "1 + 2 + 3 * 5 + 6")
let output = parser input
let _ = print_string (str_token_list input)
let _ = print_string "\n"
let _ = print_string (str_parser_result output)
let _ = print_string "\n"
let _ = print_string (str_parser_result (Some(BopExpr(NoPos,ValExpr(NoPos,NumVal(1.0)),PlusBop,BopExpr(NoPos,ValExpr(NoPos,NumVal(2.0)),PlusBop,BopExpr(NoPos,BopExpr(NoPos,ValExpr(NoPos,NumVal(3.0)),TimesBop,ValExpr(NoPos,NumVal(5.0))),PlusBop,ValExpr(NoPos,NumVal(6.0)))))),[]))
let _ = print_string "\n"