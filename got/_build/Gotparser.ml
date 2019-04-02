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
  | VOID
  | STRING
  | ACCESS
  | NEW
  | DEF
  | AT_STEP
  | SELF
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "Gotparser.mly"
open Ast
# 51 "Gotparser.ml"
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
  288 (* VOID *);
  289 (* STRING *);
  290 (* ACCESS *);
  291 (* NEW *);
  292 (* DEF *);
  293 (* AT_STEP *);
  294 (* SELF *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  295 (* LITERAL *);
  296 (* BLIT *);
  297 (* ID *);
  298 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\003\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\000\000\002\000\
\003\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\000\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\010\000\011\000\012\000\013\000\
\014\000\001\000\003\000\004\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\015\000\000\000\
\000\000\009\000\016\000\000\000\000\000\000\000\000\000\018\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\031\000\000\000\030\000\019\000\000\000\000\000\000\000\045\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\022\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\000\000\000\
\000\000\000\000\026\000\000\000\000\000\000\000\024\000\000\000\
\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\018\000\025\000\029\000\
\019\000\044\000\045\000\051\000\077\000\078\000"

let yysindex = "\025\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\255\039\255\000\000\143\255\
\005\255\049\255\063\255\000\000\069\255\143\255\000\000\042\255\
\143\255\000\000\000\000\044\255\040\255\090\255\047\255\000\000\
\000\000\047\255\047\255\047\255\098\255\100\255\104\255\000\000\
\000\000\037\255\000\000\000\000\207\255\110\000\070\255\000\000\
\000\000\166\000\114\255\047\255\047\255\047\255\047\255\047\255\
\000\000\047\255\047\255\047\255\047\255\047\255\047\255\047\255\
\047\255\047\255\047\255\047\255\047\255\000\000\000\000\000\000\
\130\000\116\255\150\000\166\000\115\255\118\255\166\000\067\255\
\067\255\000\000\000\000\186\000\186\000\003\255\003\255\003\255\
\003\255\182\000\255\254\157\255\047\255\157\255\000\000\047\255\
\095\255\229\255\000\000\166\000\157\255\047\255\000\000\122\255\
\157\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\134\255\
\000\000\000\000\141\255\000\000\000\000\000\000\000\000\000\000\
\099\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\120\255\000\000\000\000\000\000\000\000\
\000\000\185\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\255\000\000\000\000\120\255\000\000\144\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\093\255\000\000\146\255\089\255\251\255\
\020\000\000\000\000\000\113\255\142\255\042\000\050\000\072\000\
\080\000\003\000\102\000\000\000\000\000\000\000\000\000\000\000\
\128\255\000\000\000\000\102\255\000\000\147\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\106\000\000\000\031\000\000\000\000\000\114\000\
\000\000\221\255\225\255\205\255\000\000\000\000"

let yytablesize = 462
let yytable = "\046\000\
\010\000\074\000\048\000\049\000\050\000\058\000\059\000\060\000\
\061\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\073\000\050\000\075\000\076\000\
\079\000\001\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\055\000\015\000\
\016\000\031\000\014\000\032\000\033\000\020\000\017\000\034\000\
\031\000\056\000\104\000\021\000\024\000\035\000\034\000\028\000\
\097\000\028\000\099\000\028\000\035\000\098\000\036\000\037\000\
\100\000\103\000\038\000\039\000\022\000\106\000\050\000\031\000\
\023\000\032\000\071\000\060\000\061\000\034\000\040\000\041\000\
\042\000\043\000\026\000\035\000\030\000\040\000\041\000\042\000\
\043\000\047\000\015\000\047\000\036\000\037\000\047\000\052\000\
\038\000\039\000\052\000\052\000\018\000\053\000\018\000\018\000\
\053\000\054\000\018\000\053\000\040\000\041\000\042\000\043\000\
\018\000\037\000\072\000\037\000\093\000\095\000\037\000\101\000\
\027\000\018\000\018\000\096\000\105\000\018\000\018\000\037\000\
\037\000\023\000\027\000\023\000\023\000\037\000\037\000\023\000\
\006\000\018\000\018\000\018\000\018\000\023\000\038\000\007\000\
\038\000\047\000\050\000\038\000\051\000\027\000\023\000\023\000\
\000\000\000\000\023\000\023\000\038\000\038\000\031\000\000\000\
\032\000\000\000\038\000\038\000\034\000\000\000\023\000\023\000\
\023\000\023\000\035\000\005\000\006\000\007\000\008\000\009\000\
\000\000\000\000\000\000\036\000\037\000\000\000\000\000\038\000\
\039\000\032\000\000\000\032\000\000\000\000\000\032\000\032\000\
\032\000\032\000\032\000\040\000\041\000\042\000\043\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\057\000\
\000\000\000\000\000\000\000\000\000\000\058\000\059\000\060\000\
\061\000\000\000\000\000\000\000\000\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\102\000\000\000\000\000\
\000\000\000\000\000\000\058\000\059\000\060\000\061\000\000\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\033\000\000\000\033\000\000\000\000\000\
\033\000\033\000\033\000\043\000\000\000\043\000\000\000\000\000\
\043\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\000\000\000\000\000\000\034\000\000\000\034\000\043\000\
\043\000\034\000\034\000\034\000\000\000\005\000\006\000\007\000\
\008\000\009\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\039\000\000\000\039\000\000\000\000\000\039\000\
\000\000\000\000\040\000\000\000\040\000\000\000\000\000\040\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\041\000\000\000\041\000\000\000\000\000\041\000\000\000\000\000\
\042\000\000\000\042\000\000\000\000\000\042\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\044\000\000\000\
\044\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\070\000\000\000\000\000\000\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\044\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\092\000\000\000\000\000\000\000\
\058\000\059\000\060\000\061\000\000\000\000\000\000\000\000\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\094\000\000\000\000\000\000\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\000\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\000\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\058\000\059\000\060\000\061\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\064\000\065\000\066\000\067\000"

let yycheck = "\031\000\
\000\000\053\000\034\000\035\000\036\000\007\001\008\001\009\001\
\010\001\007\001\008\001\009\001\010\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\052\000\053\000\054\000\055\000\
\056\000\001\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\002\001\001\001\
\002\001\002\001\041\001\004\001\005\001\041\001\016\000\008\001\
\002\001\013\001\102\000\003\001\022\000\014\001\008\001\025\000\
\092\000\001\001\094\000\003\001\014\001\093\000\023\001\024\001\
\096\000\101\000\027\001\028\001\006\001\105\000\102\000\002\001\
\004\001\004\001\005\001\009\001\010\001\008\001\039\001\040\001\
\041\001\042\001\041\001\014\001\041\001\039\001\040\001\041\001\
\042\001\001\001\001\001\003\001\023\001\024\001\006\001\003\001\
\027\001\028\001\006\001\002\001\002\001\002\001\004\001\005\001\
\003\001\002\001\008\001\006\001\039\001\040\001\041\001\042\001\
\014\001\001\001\001\001\003\001\001\001\003\001\006\001\025\001\
\001\001\023\001\024\001\006\001\003\001\027\001\028\001\015\001\
\016\001\002\001\025\000\004\001\005\001\021\001\022\001\008\001\
\003\001\039\001\040\001\041\001\042\001\014\001\001\001\003\001\
\003\001\032\000\003\001\006\001\003\001\003\001\023\001\024\001\
\255\255\255\255\027\001\028\001\015\001\016\001\002\001\255\255\
\004\001\255\255\021\001\022\001\008\001\255\255\039\001\040\001\
\041\001\042\001\014\001\029\001\030\001\031\001\032\001\033\001\
\255\255\255\255\255\255\023\001\024\001\255\255\255\255\027\001\
\028\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001\009\001\010\001\039\001\040\001\041\001\042\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\001\001\
\255\255\255\255\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\001\001\255\255\255\255\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\001\001\255\255\003\001\255\255\255\255\
\006\001\007\001\008\001\001\001\255\255\003\001\255\255\255\255\
\006\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\001\001\255\255\003\001\021\001\
\022\001\006\001\007\001\008\001\255\255\029\001\030\001\031\001\
\032\001\033\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\001\001\255\255\003\001\255\255\255\255\006\001\
\255\255\255\255\001\001\255\255\003\001\255\255\255\255\006\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\001\001\255\255\003\001\255\255\255\255\006\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\022\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\007\001\008\001\009\001\010\001\
\007\001\008\001\009\001\010\001\015\001\016\001\017\001\018\001\
\019\001\020\001\017\001\018\001\019\001\020\001"

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
  VOID\000\
  STRING\000\
  ACCESS\000\
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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 34 "Gotparser.mly"
            ( _1 )
# 352 "Gotparser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "Gotparser.mly"
                 ( ([], [])               )
# 358 "Gotparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 38 "Gotparser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 366 "Gotparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 39 "Gotparser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 374 "Gotparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 43 "Gotparser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 389 "Gotparser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "Gotparser.mly"
                  ( [] )
# 395 "Gotparser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 51 "Gotparser.mly"
                  ( _1 )
# 402 "Gotparser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "Gotparser.mly"
                             ( [(_1,_2)]     )
# 410 "Gotparser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "Gotparser.mly"
                             ( (_3,_4) :: _1 )
# 419 "Gotparser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "Gotparser.mly"
          ( Int   )
# 425 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "Gotparser.mly"
          ( Bool  )
# 431 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "Gotparser.mly"
          ( Float )
# 437 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "Gotparser.mly"
          ( Void  )
# 443 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "Gotparser.mly"
            ( String  )
# 449 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "Gotparser.mly"
                     ( [] )
# 455 "Gotparser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 66 "Gotparser.mly"
                     ( _2 :: _1 )
# 463 "Gotparser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "Gotparser.mly"
               ( (_1, _2) )
# 471 "Gotparser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "Gotparser.mly"
                   ( [] )
# 477 "Gotparser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 73 "Gotparser.mly"
                   ( _2 :: _1 )
# 485 "Gotparser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "Gotparser.mly"
                                            ( Expr _1               )
# 492 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 77 "Gotparser.mly"
                                            ( Return _2             )
# 499 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 78 "Gotparser.mly"
                                            ( Block(List.rev _2)    )
# 506 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "Gotparser.mly"
                                            ( If(_3, _5, Block([])) )
# 514 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "Gotparser.mly"
                                            ( If(_3, _5, _7)        )
# 523 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "Gotparser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 533 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "Gotparser.mly"
                                            ( While(_3, _5)         )
# 541 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "Gotparser.mly"
                  ( Noexpr )
# 547 "Gotparser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "Gotparser.mly"
                  ( _1 )
# 554 "Gotparser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "Gotparser.mly"
                     ( Literal(_1)            )
# 561 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "Gotparser.mly"
              ( Fliteral(_1)           )
# 568 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 92 "Gotparser.mly"
                     ( BoolLit(_1)            )
# 575 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "Gotparser.mly"
                     ( Id(_1)                 )
# 582 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "Gotparser.mly"
                     ( Binop(_1, Add,   _3)   )
# 590 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "Gotparser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 598 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "Gotparser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 606 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "Gotparser.mly"
                     ( Binop(_1, Div,   _3)   )
# 614 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "Gotparser.mly"
                     ( Binop(_1, Equal, _3)   )
# 622 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "Gotparser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 630 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "Gotparser.mly"
                     ( Binop(_1, Less,  _3)   )
# 638 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "Gotparser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 646 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "Gotparser.mly"
                     ( Binop(_1, Greater, _3) )
# 654 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "Gotparser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 662 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "Gotparser.mly"
                     ( Binop(_1, And,   _3)   )
# 670 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "Gotparser.mly"
                     ( Binop(_1, Or,    _3)   )
# 678 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "Gotparser.mly"
                         ( Unop(Neg, _2)      )
# 685 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "Gotparser.mly"
                     ( Unop(Not, _2)          )
# 692 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "Gotparser.mly"
                     ( Assign(_1, _3)         )
# 700 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 109 "Gotparser.mly"
                              ( Call(_1, _3)  )
# 708 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "Gotparser.mly"
                       ( _2                   )
# 715 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "Gotparser.mly"
                  ( [] )
# 721 "Gotparser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 114 "Gotparser.mly"
               ( List.rev _1 )
# 728 "Gotparser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "Gotparser.mly"
                            ( [_1] )
# 735 "Gotparser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "Gotparser.mly"
                         ( _3 :: _1 )
# 743 "Gotparser.ml"
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
