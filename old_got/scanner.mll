{ open Parser }

let digit = ['0' - '9']
let digits = digit+

let letter = ['A'-'Z' 'a'-'z']
let words = ['A'-'Z' 'a'-'z']* 

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/#"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '%'      { MOD }
| '^'      { EXP }
| "=="     { EQ }
| "!="     { NEQ }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| '.'      { ACCESS }
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "string" { STRING }
| "bool"   { BOOL }
| "void"   { VOID   }
| "float"  { FLOAT }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "new"    { NEW }
| "def"    { DEF }
| "self"   { SELF }
| "@step"  { AT_STEP }
| '"'      {read_string (Buffer.create 17) lexbuf}
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| letter ('_' | letter | digit)*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "#/" { token lexbuf }
| _    { comment lexbuf }


and read_string buf =
  parse
  | '"'       { SLITERAL (Buffer.contents buf) }
  | '/'       { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\'      { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | words     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | digits    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
