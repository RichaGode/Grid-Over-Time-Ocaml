type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | EXP
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | ELIF
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | ACCESS
  | MAIN
  | NEW
  | DEF
  | AT_STEP
  | SELF
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | STRING of (string)
  | SLITERAL of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 52 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* MOD *);
  268 (* EXP *);
  269 (* ASSIGN *);
  270 (* NOT *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GT *);
  276 (* GEQ *);
  277 (* AND *);
  278 (* OR *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* ELIF *);
  283 (* FOR *);
  284 (* WHILE *);
  285 (* INT *);
  286 (* BOOL *);
  287 (* FLOAT *);
  288 (* ACCESS *);
  289 (* MAIN *);
  290 (* NEW *);
  291 (* DEF *);
  292 (* AT_STEP *);
  293 (* SELF *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  294 (* LITERAL *);
  295 (* BLIT *);
  296 (* ID *);
  297 (* FLIT *);
  298 (* STRING *);
  299 (* SLITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\007\000\007\000\003\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\000\000\002\000\003\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\010\000\011\000\012\000\013\000\
\001\000\003\000\004\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\014\000\000\000\000\000\
\009\000\015\000\000\000\000\000\000\000\000\000\017\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\030\000\
\000\000\029\000\032\000\018\000\000\000\000\000\000\000\045\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\021\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\000\000\000\
\000\000\000\000\025\000\000\000\000\000\000\000\023\000\000\000\
\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\011\000\012\000\017\000\024\000\028\000\
\018\000\044\000\045\000\051\000\077\000\078\000"

let yysindex = "\011\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\230\254\093\255\000\000\011\255\004\255\
\044\255\046\255\000\000\081\255\011\255\000\000\036\255\011\255\
\000\000\000\000\050\255\041\255\099\255\048\255\000\000\000\000\
\048\255\048\255\048\255\096\255\104\255\111\255\000\000\000\000\
\008\255\000\000\000\000\000\000\216\255\125\000\069\255\000\000\
\000\000\181\000\113\255\048\255\048\255\048\255\048\255\048\255\
\000\000\048\255\048\255\048\255\048\255\048\255\048\255\048\255\
\048\255\048\255\048\255\048\255\048\255\000\000\000\000\000\000\
\145\000\114\255\165\000\181\000\115\255\110\255\181\000\094\255\
\094\255\000\000\000\000\216\000\216\000\051\255\051\255\051\255\
\051\255\212\000\197\000\153\255\048\255\153\255\000\000\048\255\
\092\255\238\255\000\000\181\000\153\255\048\255\000\000\116\255\
\153\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\119\255\000\000\
\000\000\120\255\000\000\000\000\000\000\000\000\000\000\097\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\127\255\000\000\000\000\000\000\000\000\000\000\
\194\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\255\000\000\000\000\127\255\000\000\123\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\064\255\000\000\128\255\012\255\004\000\
\032\000\000\000\000\000\092\000\109\000\168\255\054\000\062\000\
\084\000\115\000\005\255\000\000\000\000\000\000\000\000\000\000\
\125\255\000\000\000\000\072\255\000\000\129\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\110\000\000\000\033\000\000\000\000\000\111\000\
\000\000\171\255\226\255\205\255\000\000\000\000"

let yytablesize = 492
let yytable = "\046\000\
\009\000\074\000\048\000\049\000\050\000\044\000\097\000\044\000\
\099\000\055\000\044\000\001\000\047\000\013\000\047\000\103\000\
\027\000\047\000\027\000\106\000\056\000\073\000\050\000\075\000\
\076\000\079\000\044\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\005\000\
\006\000\007\000\030\000\019\000\031\000\032\000\020\000\016\000\
\033\000\030\000\104\000\021\000\008\000\023\000\034\000\033\000\
\027\000\058\000\059\000\060\000\061\000\034\000\098\000\035\000\
\036\000\100\000\052\000\037\000\038\000\052\000\030\000\050\000\
\031\000\071\000\053\000\025\000\033\000\053\000\039\000\040\000\
\041\000\042\000\034\000\043\000\022\000\039\000\040\000\041\000\
\042\000\029\000\043\000\035\000\036\000\014\000\015\000\037\000\
\038\000\052\000\017\000\014\000\017\000\017\000\060\000\061\000\
\017\000\053\000\039\000\040\000\041\000\042\000\017\000\043\000\
\054\000\072\000\093\000\096\000\101\000\095\000\105\000\017\000\
\017\000\006\000\007\000\017\000\017\000\050\000\022\000\026\000\
\022\000\022\000\051\000\026\000\022\000\026\000\017\000\017\000\
\017\000\017\000\022\000\017\000\000\000\047\000\000\000\000\000\
\000\000\000\000\000\000\022\000\022\000\000\000\000\000\022\000\
\022\000\000\000\030\000\000\000\031\000\000\000\000\000\000\000\
\033\000\000\000\022\000\022\000\022\000\022\000\034\000\022\000\
\039\000\000\000\039\000\000\000\000\000\039\000\000\000\035\000\
\036\000\000\000\000\000\037\000\038\000\000\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\040\000\
\041\000\042\000\031\000\043\000\031\000\000\000\000\000\031\000\
\031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
\057\000\000\000\000\000\000\000\000\000\000\000\058\000\059\000\
\060\000\061\000\000\000\000\000\000\000\000\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\102\000\000\000\
\000\000\000\000\000\000\000\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\000\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\033\000\000\000\033\000\000\000\
\000\000\033\000\033\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\000\000\000\000\000\000\005\000\006\000\007\000\
\034\000\000\000\034\000\000\000\000\000\034\000\034\000\034\000\
\000\000\000\000\008\000\000\000\000\000\000\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\040\000\000\000\
\040\000\000\000\000\000\040\000\000\000\000\000\041\000\000\000\
\041\000\000\000\000\000\041\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\042\000\000\000\042\000\000\000\
\000\000\042\000\000\000\000\000\037\000\000\000\037\000\000\000\
\000\000\037\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\037\000\037\000\000\000\038\000\000\000\038\000\
\037\000\037\000\038\000\043\000\000\000\043\000\000\000\000\000\
\043\000\000\000\000\000\038\000\038\000\000\000\000\000\070\000\
\000\000\038\000\038\000\058\000\059\000\060\000\061\000\043\000\
\043\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\092\000\000\000\000\000\000\000\058\000\
\059\000\060\000\061\000\000\000\000\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\094\000\
\000\000\000\000\000\000\058\000\059\000\060\000\061\000\000\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\058\000\059\000\060\000\061\000\000\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\058\000\059\000\060\000\061\000\000\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\058\000\059\000\060\000\061\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\064\000\065\000\066\000\067\000"

let yycheck = "\030\000\
\000\000\053\000\033\000\034\000\035\000\001\001\092\000\003\001\
\094\000\002\001\006\001\001\000\001\001\040\001\003\001\101\000\
\001\001\006\001\003\001\105\000\013\001\052\000\053\000\054\000\
\055\000\056\000\022\001\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\029\001\
\030\001\031\001\002\001\040\001\004\001\005\001\003\001\015\000\
\008\001\002\001\102\000\006\001\042\001\021\000\014\001\008\001\
\024\000\007\001\008\001\009\001\010\001\014\001\093\000\023\001\
\024\001\096\000\003\001\027\001\028\001\006\001\002\001\102\000\
\004\001\005\001\003\001\040\001\008\001\006\001\038\001\039\001\
\040\001\041\001\014\001\043\001\004\001\038\001\039\001\040\001\
\041\001\040\001\043\001\023\001\024\001\001\001\002\001\027\001\
\028\001\002\001\002\001\001\001\004\001\005\001\009\001\010\001\
\008\001\002\001\038\001\039\001\040\001\041\001\014\001\043\001\
\002\001\001\001\001\001\006\001\025\001\003\001\003\001\023\001\
\024\001\003\001\003\001\027\001\028\001\003\001\002\001\001\001\
\004\001\005\001\003\001\003\001\008\001\024\000\038\001\039\001\
\040\001\041\001\014\001\043\001\255\255\031\000\255\255\255\255\
\255\255\255\255\255\255\023\001\024\001\255\255\255\255\027\001\
\028\001\255\255\002\001\255\255\004\001\255\255\255\255\255\255\
\008\001\255\255\038\001\039\001\040\001\041\001\014\001\043\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\023\001\
\024\001\255\255\255\255\027\001\028\001\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\038\001\039\001\
\040\001\041\001\001\001\043\001\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\001\001\255\255\255\255\255\255\255\255\255\255\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\255\255\
\255\255\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\255\255\255\255\255\255\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\029\001\030\001\031\001\
\001\001\255\255\003\001\255\255\255\255\006\001\007\001\008\001\
\255\255\255\255\042\001\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\001\001\255\255\
\003\001\255\255\255\255\006\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\001\001\255\255\003\001\255\255\
\255\255\006\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\015\001\016\001\255\255\001\001\255\255\003\001\
\021\001\022\001\006\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\015\001\016\001\255\255\255\255\003\001\
\255\255\021\001\022\001\007\001\008\001\009\001\010\001\021\001\
\022\001\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\007\001\008\001\009\001\010\001\007\001\008\001\
\009\001\010\001\015\001\016\001\017\001\018\001\019\001\020\001\
\017\001\018\001\019\001\020\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  EXP\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  ACCESS\000\
  MAIN\000\
  NEW\000\
  DEF\000\
  AT_STEP\000\
  SELF\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  STRING\000\
  SLITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 35 "parser.mly"
            ( _1 )
# 363 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                 ( ([], [])               )
# 369 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 39 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 377 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 40 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 385 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 44 "parser.mly"
     ( { typ = _1;
   fname = _2;
   formals = List.rev _4;
   locals = List.rev _7;
   body = List.rev _8 } )
# 400 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                  ( [] )
# 406 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 52 "parser.mly"
                  ( _1 )
# 413 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                             ( [(_1,_2)]     )
# 421 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                             ( (_3,_4) :: _1 )
# 430 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
          ( Int   )
# 436 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
          ( Bool  )
# 442 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
          ( Float )
# 448 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
            ( String  )
# 455 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                     ( [] )
# 461 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 66 "parser.mly"
                     ( _2 :: _1 )
# 469 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
               ( (_1, _2) )
# 477 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                   ( [] )
# 483 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 73 "parser.mly"
                   ( _2 :: _1 )
# 491 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                            ( Expr _1               )
# 498 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 77 "parser.mly"
                                            ( Return _2             )
# 505 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 78 "parser.mly"
                                            ( Block(List.rev _2)    )
# 512 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 520 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 529 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 539 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                                            ( While(_3, _5)         )
# 547 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                  ( Noexpr )
# 553 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                  ( _1 )
# 560 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
                     ( Literal(_1)            )
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
                     ( Fliteral(_1)           )
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 92 "parser.mly"
                     ( BoolLit(_1)            )
# 581 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                     ( Id(_1)                 )
# 588 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
                     ( Sliteral(_1)           )
# 595 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 603 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 611 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 627 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 643 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 667 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 675 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 683 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 691 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                              ( Unop(Neg, _2)  )
# 698 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                              ( Unop(Not, _2)  )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                              ( Assign(_1, _3) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 110 "parser.mly"
                              ( Call(_1, _3)   )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                              ( _2             )
# 728 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                  ( [] )
# 734 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 116 "parser.mly"
               ( List.rev _1 )
# 741 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                            ( [_1] )
# 748 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                         ( _3 :: _1 )
# 756 "parser.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
