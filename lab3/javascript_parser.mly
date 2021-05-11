/*
(* THIS IS AN AUTO-GENERATED FILE PRODUCED BY URUZ!
 * DO NOT EDIT THIS FILE, since any changes will be
 * lost when the build is reset via "make clean".
 * This file is based on a user-specified EBNF
 * grammar, which can be edited as desired.
 *) */
%{
   open Javascript_ast;;
   open Javascript_utils;;

%}

%token EOF
%token <char> BLOCK_1_1 EXPR1_11_1 EXPR1_16_1 EXPR1_16_3 EXPR1_18_1 EXPR1_19_1 EXPR1_3_0 EXPR1_4_1 EXPR1_5_1 EXPR1_6_1 EXPR1_7_1 EXPR1_9_1 IDENT_LIST_1_1_0_0_0 LAMBDA1_0_4 LAMBDA1_0_5 LAMBDA1_0_7 STMT_2_1
%token <float> NUMBER
%token <string> EXPR1_10_1 EXPR1_12_1 EXPR1_13_1 EXPR1_14_1 EXPR1_15_1 EXPR1_17_0 EXPR1_17_2 EXPR1_8_1 IDENT1 STRING VALUE1_1_0 VALUE1_2_0
%token BLANKS CONST_KW FALSE_KW FUNC_KW LET_KW MULTI_LINE_COMMENT RET_KW SINGLE_LINE_COMMENT TRUE_KW UNDEF_KW
/*(* starting with lowest precedence:*)*/
%nonassoc EXPR1_16_3 TEMP_PREC_12 /*(* 12 *)*/
%nonassoc EXPR1_16_1 TEMP_PREC_11 /*(* 11 *)*/
%left EXPR1_15_1 TEMP_PREC_10 /*(* 10 *)*/
%left EXPR1_14_1 TEMP_PREC_9 /*(* 9 *)*/
%left EXPR1_12_1 EXPR1_13_1 TEMP_PREC_8 /*(* 8 *)*/
%left EXPR1_8_1 EXPR1_9_1 EXPR1_10_1 EXPR1_11_1 TEMP_PREC_7 /*(* 7 *)*/
%left EXPR1_6_1 EXPR1_7_1 TEMP_PREC_5 /*(* 5 *)*/
%left EXPR1_4_1 EXPR1_5_1 TEMP_PREC_4 /*(* 4 *)*/
%nonassoc EXPR1_3_0 TEMP_PREC_3 /*(* 3 *)*/
%left EXPR1_19_1 TEMP_PREC_2 /*(* 2 *)*/
%nonassoc EXPR1_18_1 TEMP_PREC_1 /*(* 1 *)*/
/*(* ^^ highest precedence *)*/
%start start /*(* the entry point *)*/
%type <Javascript_ast.program_t> start
%%
start:
| startAux {let x = $1 in ignore x; (x)}
;

startAux:
| program EOF { $1 }
;

program:
| programAux {let x = $1 in ignore x; (x)}
;

programAux:
| expr1 {let x = ($1) in ignore x; ((let (x_1) = x in ExprProgram(get_current_pos (),x_1)))}
| stmt BLOCK_1_1 programAux {let x = ($1,$3) in ignore x; ((let (x_1,x_3) = x in StmtProgram(get_current_pos (),x_1,x_3)))}
;

stmt:
| stmtAux {let x = $1 in ignore x; (x)}
;

stmtAux:
| CONST_KW IDENT1 STMT_2_1 expr1 { ConstStmt(get_current_pos (), $2, $4) }
| LET_KW IDENT1 STMT_2_1 expr1 { LetStmt(get_current_pos (), $2, $4) }
| expr1 STMT_2_1 expr1 {let x = ($1,$3) in ignore x; ((let (x_1,x_3) = x in AssignStmt(get_current_pos (),x_1,x_3)))}
;

expr1:
| expr1Aux {let x = $1 in ignore x; (x)}
;

expr1Aux:
| IDENT1 { VarExpr(get_current_pos (),$1) }
| value1 { ValExpr(get_current_pos (),$1) }
| EXPR1_7_1 expr1Aux %prec TEMP_PREC_3 { UopExpr(get_current_pos (),NegUop,$2) }
| EXPR1_3_0 expr1Aux { UopExpr(get_current_pos (),NotUop,$2) }
| expr1Aux EXPR1_4_1 expr1Aux { BopExpr(get_current_pos (),$1,TimesBop,$3) }
| expr1Aux EXPR1_5_1 expr1Aux { BopExpr(get_current_pos (),$1,DivBop,$3) }
| expr1Aux EXPR1_6_1 expr1Aux { BopExpr(get_current_pos (),$1,PlusBop,$3) }
| expr1Aux EXPR1_7_1 expr1Aux { BopExpr(get_current_pos (),$1,MinusBop,$3) }
| expr1Aux EXPR1_8_1 expr1Aux { BopExpr(get_current_pos (),$1,LteBop,$3) }
| expr1Aux EXPR1_9_1 expr1Aux { BopExpr(get_current_pos (),$1,LtBop,$3) }
| expr1Aux EXPR1_10_1 expr1Aux { BopExpr(get_current_pos (),$1,GteBop,$3) }
| expr1Aux EXPR1_11_1 expr1Aux { BopExpr(get_current_pos (),$1,GtBop,$3) }
| expr1Aux EXPR1_12_1 expr1Aux { BopExpr(get_current_pos (),$1,EqBop,$3) }
| expr1Aux EXPR1_13_1 expr1Aux { BopExpr(get_current_pos (),$1,NeqBop,$3) }
| expr1Aux EXPR1_14_1 expr1Aux { BopExpr(get_current_pos (),$1,AndBop,$3) }
| expr1Aux EXPR1_15_1 expr1Aux { BopExpr(get_current_pos (),$1,OrBop,$3) }
| expr1Aux EXPR1_16_1 expr1Aux EXPR1_16_3 expr1Aux { IfExpr(get_current_pos (),$1,$3,$5) }
| EXPR1_17_0 EXPR1_19_1 EXPR1_17_2 EXPR1_18_1 expr1Aux LAMBDA1_0_4 { PrintExpr(get_current_pos (),$5) }
| expr1Aux EXPR1_18_1 expr_list LAMBDA1_0_4 { CallExpr(get_current_pos (), $1,$3) }
| expr1Aux EXPR1_19_1 IDENT1 { FieldExpr(get_current_pos (),$1,$3) }
| LAMBDA1_0_5 field_list LAMBDA1_0_7 { ObjectExpr(get_current_pos (),$2) }
| EXPR1_18_1 expr1Aux LAMBDA1_0_4 { $2 }
;

value1:
| value1Aux {let x = $1 in ignore x; (x)}
;

value1Aux:
| NUMBER { NumVal($1) }
| VALUE1_1_0 { NumVal(Float.nan) }
| VALUE1_2_0 { NumVal(Float.infinity) }
| bool { BoolVal($1) }
| STRING { StrVal($1) }
| UNDEF_KW { UndefVal }
| lambda1 { ClosureVal(StringMap.empty, $1) }
;

lambda1:
| lambda1Aux {let x = $1 in ignore x; (x)}
;

lambda1Aux:
| FUNC_KW lambda1_0_1 EXPR1_18_1 ident_list LAMBDA1_0_4 LAMBDA1_0_5 block LAMBDA1_0_7 { ($2, List.map (fun v -> (v,None)) $4, $7, None) }
;

lambda1_0_1:
| lambda1_0_1Aux {let x = $1 in ignore x; (x)}
;

lambda1_0_1Aux:
| Empty {None}
| IDENT1 {Some($1)}
;

block:
| blockAux {let x = $1 in ignore x; (x)}
;

blockAux:
| RET_KW expr1 {let x = ($2) in ignore x; ((let (x_2) = x in ReturnBlock(get_current_pos (),x_2)))}
| stmt BLOCK_1_1 blockAux {let x = ($1,$3) in ignore x; ((let (x_1,x_3) = x in StmtBlock(get_current_pos (),x_1,x_3)))}
;

field_list:
| field_listAux {let x = $1 in ignore x; (x)}
;

field_listAux:
| Empty { [] }
| field field_list_1_1 { $1::$2 }
;

field_list_1_1:
| field_list_1_1Aux {let x = $1 in ignore x; (List.rev x)}
;

field_list_1_1Aux:
| Empty {[]}
| field_list_1_1Aux field_list_1_1_0 {let x = $2 in (x)::$1}
;

field_list_1_1_0:
| field_list_1_1_0Aux {let x = $1 in ignore x; (x)}
;

field_list_1_1_0Aux:
| IDENT_LIST_1_1_0_0_0 field { $2 }
;

expr_list:
| expr_listAux {let x = $1 in ignore x; (x)}
;

expr_listAux:
| Empty { [] }
| expr1 expr_list_1_1 { $1::$2 }
;

expr_list_1_1:
| expr_list_1_1Aux {let x = $1 in ignore x; (List.rev x)}
;

expr_list_1_1Aux:
| Empty {[]}
| expr_list_1_1Aux expr_list_1_1_0 {let x = $2 in (x)::$1}
;

expr_list_1_1_0:
| expr_list_1_1_0Aux {let x = $1 in ignore x; (x)}
;

expr_list_1_1_0Aux:
| IDENT_LIST_1_1_0_0_0 expr1 { $2 }
;

ident_list:
| ident_listAux {let x = $1 in ignore x; (x)}
;

ident_listAux:
| Empty { [] }
| IDENT1 ident_list_1_1 { $1::$2 }
;

ident_list_1_1:
| ident_list_1_1Aux {let x = $1 in ignore x; (List.rev x)}
;

ident_list_1_1Aux:
| Empty {[]}
| ident_list_1_1Aux ident_list_1_1_0 {let x = $2 in (x)::$1}
;

ident_list_1_1_0:
| ident_list_1_1_0Aux {let x = $1 in ignore x; (x)}
;

ident_list_1_1_0Aux:
| IDENT_LIST_1_1_0_0_0 IDENT1 { $2 }
;

field:
| fieldAux {let x = $1 in ignore x; (x)}
;

fieldAux:
| IDENT1 EXPR1_16_3 expr1 { ($1, $3) }
;

bool:
| boolAux {let x = $1 in ignore x; (x)}
;

boolAux:
| TRUE_KW { true }
| FALSE_KW { false }
;

Empty:
| {}
;

%%
(* footer code *)
