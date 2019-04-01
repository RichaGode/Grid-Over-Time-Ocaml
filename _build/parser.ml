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
  299 (* SLITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\003\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\000\000\002\000\
\003\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\000\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\055\000\000\000\010\000\011\000\012\000\014\000\
\013\000\001\000\003\000\004\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\015\000\000\000\
\000\000\009\000\016\000\000\000\000\000\000\000\000\000\018\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\031\000\000\000\030\000\033\000\019\000\000\000\000\000\000\000\
\046\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\022\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\049\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\024\000\
\000\000\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\018\000\025\000\029\000\
\019\000\045\000\046\000\052\000\078\000\079\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\234\254\069\255\000\000\200\255\
\006\255\064\255\068\255\000\000\073\255\200\255\000\000\037\255\
\200\255\000\000\000\000\045\255\041\255\091\255\048\255\000\000\
\000\000\048\255\048\255\048\255\094\255\095\255\098\255\000\000\
\000\000\008\255\000\000\000\000\000\000\227\255\110\000\071\255\
\000\000\000\000\166\000\092\255\048\255\048\255\048\255\048\255\
\048\255\000\000\048\255\048\255\048\255\048\255\048\255\048\255\
\048\255\048\255\048\255\048\255\048\255\048\255\000\000\000\000\
\000\000\130\000\100\255\150\000\166\000\099\255\102\255\166\000\
\043\255\043\255\000\000\000\000\201\000\201\000\051\255\051\255\
\051\255\051\255\197\000\182\000\161\255\048\255\161\255\000\000\
\048\255\079\255\249\255\000\000\166\000\161\255\048\255\000\000\
\104\255\161\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\113\255\
\000\000\000\000\115\255\000\000\000\000\000\000\000\000\000\000\
\101\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\119\255\000\000\000\000\000\000\000\000\
\000\000\205\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\255\000\000\000\000\119\255\000\000\118\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\255\000\000\120\255\011\255\
\175\255\020\000\000\000\000\000\116\255\145\255\042\000\050\000\
\072\000\080\000\102\000\005\255\000\000\000\000\000\000\000\000\
\000\000\131\255\000\000\000\000\038\255\000\000\123\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\105\000\000\000\032\000\000\000\000\000\095\000\
\000\000\170\255\225\255\204\255\000\000\000\000"

let yytablesize = 477
let yytable = "\047\000\
\010\000\075\000\049\000\050\000\051\000\045\000\098\000\045\000\
\100\000\056\000\045\000\048\000\001\000\048\000\053\000\104\000\
\048\000\053\000\014\000\107\000\057\000\074\000\051\000\076\000\
\077\000\080\000\045\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\028\000\
\054\000\028\000\031\000\054\000\032\000\033\000\020\000\017\000\
\034\000\031\000\105\000\061\000\062\000\024\000\035\000\034\000\
\028\000\059\000\060\000\061\000\062\000\035\000\099\000\036\000\
\037\000\101\000\021\000\038\000\039\000\015\000\016\000\051\000\
\031\000\022\000\032\000\072\000\023\000\026\000\034\000\040\000\
\041\000\042\000\043\000\044\000\035\000\030\000\040\000\041\000\
\042\000\043\000\044\000\015\000\073\000\036\000\037\000\053\000\
\054\000\038\000\039\000\055\000\094\000\096\000\018\000\102\000\
\018\000\018\000\106\000\097\000\018\000\040\000\041\000\042\000\
\043\000\044\000\018\000\006\000\038\000\007\000\038\000\027\000\
\051\000\038\000\052\000\018\000\018\000\027\000\048\000\018\000\
\018\000\027\000\038\000\038\000\023\000\000\000\023\000\023\000\
\038\000\038\000\023\000\018\000\018\000\018\000\018\000\018\000\
\023\000\039\000\000\000\039\000\000\000\000\000\039\000\000\000\
\000\000\023\000\023\000\000\000\000\000\023\000\023\000\039\000\
\039\000\000\000\031\000\000\000\032\000\039\000\039\000\000\000\
\034\000\023\000\023\000\023\000\023\000\023\000\035\000\034\000\
\000\000\034\000\000\000\000\000\034\000\034\000\034\000\036\000\
\037\000\000\000\000\000\038\000\039\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\000\000\000\000\040\000\
\041\000\042\000\043\000\044\000\000\000\032\000\000\000\032\000\
\000\000\000\000\032\000\032\000\032\000\032\000\032\000\000\000\
\000\000\000\000\000\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\058\000\005\000\006\000\007\000\008\000\
\009\000\059\000\060\000\061\000\062\000\000\000\000\000\000\000\
\000\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\103\000\000\000\000\000\000\000\000\000\000\000\059\000\
\060\000\061\000\062\000\000\000\000\000\000\000\000\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\035\000\000\000\
\000\000\035\000\035\000\035\000\000\000\005\000\006\000\007\000\
\008\000\009\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\040\000\000\000\040\000\000\000\000\000\040\000\
\000\000\000\000\041\000\000\000\041\000\000\000\000\000\041\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\042\000\000\000\042\000\000\000\000\000\042\000\000\000\000\000\
\043\000\000\000\043\000\000\000\000\000\043\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\044\000\000\000\
\044\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\071\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\000\000\000\000\044\000\044\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\093\000\000\000\000\000\000\000\
\059\000\060\000\061\000\062\000\000\000\000\000\000\000\000\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\095\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\000\000\000\000\000\000\000\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\059\000\060\000\061\000\062\000\
\000\000\000\000\000\000\000\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\059\000\060\000\061\000\062\000\
\000\000\000\000\000\000\000\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\059\000\060\000\061\000\062\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\065\000\066\000\067\000\068\000"

let yycheck = "\031\000\
\000\000\054\000\034\000\035\000\036\000\001\001\093\000\003\001\
\095\000\002\001\006\001\001\001\001\000\003\001\003\001\102\000\
\006\001\006\001\041\001\106\000\013\001\053\000\054\000\055\000\
\056\000\057\000\022\001\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\001\001\
\003\001\003\001\002\001\006\001\004\001\005\001\041\001\016\000\
\008\001\002\001\103\000\009\001\010\001\022\000\014\001\008\001\
\025\000\007\001\008\001\009\001\010\001\014\001\094\000\023\001\
\024\001\097\000\003\001\027\001\028\001\001\001\002\001\103\000\
\002\001\006\001\004\001\005\001\004\001\041\001\008\001\039\001\
\040\001\041\001\042\001\043\001\014\001\041\001\039\001\040\001\
\041\001\042\001\043\001\001\001\001\001\023\001\024\001\002\001\
\002\001\027\001\028\001\002\001\001\001\003\001\002\001\025\001\
\004\001\005\001\003\001\006\001\008\001\039\001\040\001\041\001\
\042\001\043\001\014\001\003\001\001\001\003\001\003\001\001\001\
\003\001\006\001\003\001\023\001\024\001\003\001\032\000\027\001\
\028\001\025\000\015\001\016\001\002\001\255\255\004\001\005\001\
\021\001\022\001\008\001\039\001\040\001\041\001\042\001\043\001\
\014\001\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\255\255\023\001\024\001\255\255\255\255\027\001\028\001\015\001\
\016\001\255\255\002\001\255\255\004\001\021\001\022\001\255\255\
\008\001\039\001\040\001\041\001\042\001\043\001\014\001\001\001\
\255\255\003\001\255\255\255\255\006\001\007\001\008\001\023\001\
\024\001\255\255\255\255\027\001\028\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\039\001\
\040\001\041\001\042\001\043\001\255\255\001\001\255\255\003\001\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\001\001\029\001\030\001\031\001\032\001\
\033\001\007\001\008\001\009\001\010\001\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\001\001\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\255\255\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\029\001\030\001\031\001\
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
\255\255\255\255\021\001\022\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\007\001\008\001\009\001\010\001\007\001\
\008\001\009\001\010\001\015\001\016\001\017\001\018\001\019\001\
\020\001\017\001\018\001\019\001\020\001"

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
  SLITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 35 "parser.mly"
            ( _1 )
# 359 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                 ( ([], [])               )
# 365 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 39 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 373 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 40 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 381 "parser.ml"
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
# 396 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                  ( [] )
# 402 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 52 "parser.mly"
                  ( _1 )
# 409 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                             ( [(_1,_2)]     )
# 417 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                             ( (_3,_4) :: _1 )
# 426 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
          ( Int   )
# 432 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
          ( Bool  )
# 438 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
          ( Float )
# 444 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
            ( String  )
# 450 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
         ( Void )
# 456 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                     ( [] )
# 462 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 67 "parser.mly"
                     ( _2 :: _1 )
# 470 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "parser.mly"
               ( (_1, _2) )
# 478 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                   ( [] )
# 484 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 74 "parser.mly"
                   ( _2 :: _1 )
# 492 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                                            ( Expr _1               )
# 499 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 78 "parser.mly"
                                            ( Return _2             )
# 506 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 79 "parser.mly"
                                            ( Block(List.rev _2)    )
# 513 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 521 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 530 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 540 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "parser.mly"
                                            ( While(_3, _5)         )
# 548 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                  ( Noexpr )
# 554 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                  ( _1 )
# 561 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
                     ( Literal(_1)            )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                     ( Fliteral(_1)           )
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 93 "parser.mly"
                     ( BoolLit(_1)            )
# 582 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
                     ( Id(_1)                 )
# 589 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
                     ( Sliteral(_1)           )
# 596 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 604 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 612 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 620 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 628 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 636 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 644 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 660 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 668 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                              ( Unop(Neg, _2)  )
# 699 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                              ( Unop(Not, _2)  )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                              ( Assign(_1, _3) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 111 "parser.mly"
                              ( Call(_1, _3)   )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                              ( _2             )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                  ( [] )
# 735 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 117 "parser.mly"
               ( List.rev _1 )
# 742 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                            ( [_1] )
# 749 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                         ( _3 :: _1 )
# 757 "parser.ml"
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
