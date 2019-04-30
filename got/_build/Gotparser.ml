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
  | STR_LITERAL of (string)
  | STRUCT of (string)
  | STRUCT_LITERAL of (string)
  | GRID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "Gotparser.mly"
open Ast
# 55 "Gotparser.ml"
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
  299 (* STR_LITERAL *);
  300 (* STRUCT *);
  301 (* STRUCT_LITERAL *);
  302 (* GRID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\007\000\007\000\003\000\008\000\008\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\012\000\012\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\013\000\013\000\
\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\003\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\059\000\000\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\001\000\003\000\004\000\000\000\000\000\
\019\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\
\017\000\000\000\000\000\009\000\018\000\000\000\000\000\000\000\
\000\000\020\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\033\000\000\000\032\000\035\000\021\000\000\000\
\000\000\000\000\050\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\054\000\024\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\049\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\000\000\000\000\000\
\000\000\028\000\000\000\000\000\000\000\026\000\000\000\000\000\
\027\000"

let yydgoto = "\002\000\
\003\000\004\000\013\000\014\000\015\000\020\000\027\000\031\000\
\021\000\047\000\048\000\054\000\082\000\083\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\234\254\050\255\
\000\000\201\000\020\255\043\255\069\255\000\000\074\255\201\000\
\000\000\040\255\201\000\000\000\000\000\049\255\045\255\081\255\
\052\255\000\000\000\000\052\255\052\255\052\255\094\255\095\255\
\098\255\000\000\000\000\008\255\000\000\000\000\000\000\065\000\
\167\000\075\255\000\000\000\000\241\000\100\255\052\255\052\255\
\052\255\052\255\052\255\000\000\052\255\052\255\052\255\052\255\
\052\255\052\255\052\255\052\255\052\255\052\255\052\255\052\255\
\052\255\052\255\000\000\000\000\000\000\187\000\103\255\207\000\
\241\000\102\255\106\255\241\000\058\000\058\000\096\255\096\255\
\096\255\000\000\030\001\030\001\228\000\228\000\228\000\228\000\
\016\001\001\001\165\255\052\255\165\255\000\000\052\255\086\255\
\087\000\000\000\241\000\165\255\052\255\000\000\117\255\165\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\119\255\000\000\000\000\121\255\000\000\000\000\000\000\
\000\000\000\000\105\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\124\255\000\000\000\000\
\000\000\000\000\000\000\208\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\041\255\000\000\000\000\124\255\
\000\000\127\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\255\000\000\128\255\011\255\179\255\020\000\230\255\252\255\
\043\000\000\000\042\255\120\255\109\000\117\000\139\000\147\000\
\149\255\005\255\000\000\000\000\000\000\000\000\000\000\135\255\
\000\000\000\000\068\255\000\000\131\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\079\000\000\000\038\000\000\000\000\000\093\000\
\000\000\164\255\223\255\202\255\000\000\000\000"

let yytablesize = 562
let yytable = "\049\000\
\012\000\079\000\051\000\052\000\053\000\047\000\104\000\047\000\
\106\000\058\000\047\000\052\000\001\000\052\000\057\000\110\000\
\052\000\057\000\016\000\113\000\059\000\078\000\053\000\080\000\
\081\000\084\000\047\000\085\000\086\000\087\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\096\000\097\000\
\098\000\030\000\040\000\030\000\040\000\023\000\033\000\040\000\
\034\000\035\000\017\000\018\000\036\000\033\000\111\000\019\000\
\040\000\040\000\037\000\036\000\022\000\026\000\040\000\040\000\
\030\000\037\000\105\000\038\000\039\000\107\000\058\000\040\000\
\041\000\058\000\024\000\053\000\033\000\025\000\034\000\076\000\
\028\000\017\000\036\000\042\000\043\000\044\000\045\000\046\000\
\037\000\032\000\042\000\043\000\044\000\045\000\046\000\055\000\
\056\000\038\000\039\000\057\000\077\000\040\000\041\000\100\000\
\102\000\029\000\020\000\066\000\020\000\020\000\108\000\103\000\
\020\000\042\000\043\000\044\000\045\000\046\000\020\000\112\000\
\041\000\006\000\041\000\007\000\029\000\041\000\050\000\020\000\
\020\000\055\000\056\000\020\000\020\000\029\000\041\000\041\000\
\025\000\000\000\025\000\025\000\041\000\041\000\025\000\020\000\
\020\000\020\000\020\000\020\000\025\000\046\000\000\000\046\000\
\000\000\000\000\046\000\000\000\000\000\025\000\025\000\000\000\
\000\000\025\000\025\000\000\000\000\000\000\000\033\000\000\000\
\034\000\046\000\046\000\000\000\036\000\025\000\025\000\025\000\
\025\000\025\000\037\000\036\000\000\000\036\000\000\000\000\000\
\036\000\036\000\036\000\038\000\039\000\000\000\000\000\040\000\
\041\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\000\000\000\000\042\000\043\000\044\000\045\000\046\000\
\034\000\000\000\034\000\000\000\000\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\000\000\000\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\038\000\000\000\
\038\000\000\000\000\000\038\000\038\000\038\000\038\000\038\000\
\038\000\000\000\000\000\000\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\039\000\000\000\039\000\000\000\
\000\000\039\000\039\000\039\000\039\000\039\000\039\000\000\000\
\000\000\000\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\000\000\000\000\037\000\000\000\037\000\000\000\
\000\000\037\000\037\000\037\000\000\000\005\000\006\000\007\000\
\008\000\009\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\000\000\048\000\010\000\048\000\011\000\000\000\
\048\000\048\000\048\000\048\000\048\000\048\000\000\000\000\000\
\000\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\060\000\063\000\064\000\065\000\066\000\000\000\061\000\
\062\000\063\000\064\000\065\000\066\000\000\000\000\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\109\000\
\000\000\000\000\000\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\042\000\000\000\042\000\
\000\000\000\000\042\000\000\000\000\000\043\000\000\000\043\000\
\000\000\000\000\043\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\044\000\000\000\044\000\000\000\000\000\
\044\000\000\000\000\000\045\000\000\000\045\000\000\000\000\000\
\045\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\075\000\000\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\099\000\000\000\000\000\
\000\000\061\000\062\000\063\000\064\000\065\000\066\000\000\000\
\000\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\101\000\000\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\005\000\006\000\007\000\
\008\000\009\000\061\000\062\000\063\000\064\000\065\000\066\000\
\000\000\000\000\000\000\000\000\010\000\000\000\011\000\061\000\
\062\000\063\000\064\000\065\000\066\000\000\000\000\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\061\000\
\062\000\063\000\064\000\065\000\066\000\000\000\000\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\061\000\062\000\
\063\000\064\000\065\000\066\000\000\000\000\000\067\000\068\000\
\069\000\070\000\071\000\072\000\061\000\062\000\063\000\064\000\
\065\000\066\000\000\000\000\000\000\000\000\000\069\000\070\000\
\071\000\072\000"

let yycheck = "\033\000\
\000\000\056\000\036\000\037\000\038\000\001\001\099\000\003\001\
\101\000\002\001\006\001\001\001\001\000\003\001\003\001\108\000\
\006\001\006\001\041\001\112\000\013\001\055\000\056\000\057\000\
\058\000\059\000\022\001\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\001\001\001\001\003\001\003\001\003\001\002\001\006\001\
\004\001\005\001\001\001\002\001\008\001\002\001\109\000\018\000\
\015\001\016\001\014\001\008\001\041\001\024\000\021\001\022\001\
\027\000\014\001\100\000\023\001\024\001\103\000\003\001\027\001\
\028\001\006\001\006\001\109\000\002\001\004\001\004\001\005\001\
\041\001\001\001\008\001\039\001\040\001\041\001\042\001\043\001\
\014\001\041\001\039\001\040\001\041\001\042\001\043\001\002\001\
\002\001\023\001\024\001\002\001\001\001\027\001\028\001\001\001\
\003\001\027\000\002\001\012\001\004\001\005\001\025\001\006\001\
\008\001\039\001\040\001\041\001\042\001\043\001\014\001\003\001\
\001\001\003\001\003\001\003\001\001\001\006\001\034\000\023\001\
\024\001\003\001\003\001\027\001\028\001\003\001\015\001\016\001\
\002\001\255\255\004\001\005\001\021\001\022\001\008\001\039\001\
\040\001\041\001\042\001\043\001\014\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\023\001\024\001\255\255\
\255\255\027\001\028\001\255\255\255\255\255\255\002\001\255\255\
\004\001\021\001\022\001\255\255\008\001\039\001\040\001\041\001\
\042\001\043\001\014\001\001\001\255\255\003\001\255\255\255\255\
\006\001\007\001\008\001\023\001\024\001\255\255\255\255\027\001\
\028\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\039\001\040\001\041\001\042\001\043\001\
\001\001\255\255\003\001\255\255\255\255\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\255\255\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\255\255\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\029\001\030\001\031\001\
\032\001\033\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\001\001\044\001\003\001\046\001\255\255\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\001\001\009\001\010\001\011\001\012\001\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\001\001\
\255\255\255\255\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\001\001\255\255\003\001\
\255\255\255\255\006\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\001\001\255\255\003\001\255\255\255\255\
\006\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\003\001\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\011\001\012\001\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\029\001\030\001\031\001\
\032\001\033\001\007\001\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\255\255\044\001\255\255\046\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\007\001\008\001\
\009\001\010\001\011\001\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001"

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
  STR_LITERAL\000\
  STRUCT\000\
  STRUCT_LITERAL\000\
  GRID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 34 "Gotparser.mly"
            ( _1 )
# 395 "Gotparser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "Gotparser.mly"
                 ( ([], [])               )
# 401 "Gotparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 38 "Gotparser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 409 "Gotparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 39 "Gotparser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 417 "Gotparser.ml"
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
# 432 "Gotparser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "Gotparser.mly"
                  ( [] )
# 438 "Gotparser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 51 "Gotparser.mly"
                  ( _1 )
# 445 "Gotparser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "Gotparser.mly"
                             ( [(_1,_2)]     )
# 453 "Gotparser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "Gotparser.mly"
                             ( (_3,_4) :: _1 )
# 462 "Gotparser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "Gotparser.mly"
          ( Int   )
# 468 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "Gotparser.mly"
          ( Bool  )
# 474 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "Gotparser.mly"
          ( Float )
# 480 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "Gotparser.mly"
          ( Void  )
# 486 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "Gotparser.mly"
            ( String  )
# 492 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "Gotparser.mly"
           ( Struct )
# 499 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "Gotparser.mly"
         ( Grid )
# 506 "Gotparser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "Gotparser.mly"
                     ( [] )
# 512 "Gotparser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 68 "Gotparser.mly"
                     ( _2 :: _1 )
# 520 "Gotparser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 71 "Gotparser.mly"
                ( (_1, _2) )
# 528 "Gotparser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "Gotparser.mly"
                   ( [] )
# 534 "Gotparser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "Gotparser.mly"
                   ( _2 :: _1 )
# 542 "Gotparser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "Gotparser.mly"
                                            ( Expr _1               )
# 549 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 81 "Gotparser.mly"
                                            ( Return _2             )
# 556 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 82 "Gotparser.mly"
                                            ( Block(List.rev _2)    )
# 563 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "Gotparser.mly"
                                            ( If(_3, _5, Block([])) )
# 571 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "Gotparser.mly"
                                            ( If(_3, _5, _7)        )
# 580 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 86 "Gotparser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 590 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 87 "Gotparser.mly"
                                            ( While(_3, _5)         )
# 598 "Gotparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "Gotparser.mly"
                  ( Noexpr )
# 604 "Gotparser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "Gotparser.mly"
                  ( _1 )
# 611 "Gotparser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "Gotparser.mly"
                     ( Literal(_1)            )
# 618 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "Gotparser.mly"
                     ( Fliteral(_1)           )
# 625 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 96 "Gotparser.mly"
                     ( BoolLit(_1)            )
# 632 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "Gotparser.mly"
                     ( Id(_1)                 )
# 639 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "Gotparser.mly"
                     ( Str_literal(_1)        )
# 646 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "Gotparser.mly"
                     ( Binop(_1, Add,   _3)   )
# 654 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "Gotparser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 662 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "Gotparser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 670 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "Gotparser.mly"
                     ( Binop(_1, Div,   _3)   )
# 678 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "Gotparser.mly"
                     ( Binop(_1, Equal, _3)   )
# 686 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "Gotparser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 694 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "Gotparser.mly"
                     ( Binop(_1, Less,  _3)   )
# 702 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "Gotparser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 710 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "Gotparser.mly"
                     ( Binop(_1, Greater, _3) )
# 718 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "Gotparser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 726 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "Gotparser.mly"
                     ( Binop(_1, And,   _3)   )
# 734 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "Gotparser.mly"
                     ( Binop(_1, Or,    _3)   )
# 742 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "Gotparser.mly"
                     ( Binop(_1, Mod,   _3)   )
# 750 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "Gotparser.mly"
                     ( Binop(_1, Exp,   _3)   )
# 758 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "Gotparser.mly"
                              ( Unop(Neg, _2)  )
# 765 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "Gotparser.mly"
                              ( Unop(Not, _2)  )
# 772 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "Gotparser.mly"
                              ( Assign(_1, _3) )
# 780 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 117 "Gotparser.mly"
                              ( Call(_1, _3)   )
# 788 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 118 "Gotparser.mly"
                              ( _2             )
# 795 "Gotparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "Gotparser.mly"
                  ( [] )
# 801 "Gotparser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 122 "Gotparser.mly"
               ( List.rev _1 )
# 808 "Gotparser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "Gotparser.mly"
                            ( [_1] )
# 815 "Gotparser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "Gotparser.mly"
                         ( _3 :: _1 )
# 823 "Gotparser.ml"
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
