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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
