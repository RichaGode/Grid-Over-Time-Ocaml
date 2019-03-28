{ open GoT }

let digit = ['0' - '9']
let digits = digit+

let letter = ['A'-'Z' 'a'-'z' ]
let words = [letter '\t' '\n' '\r' '\b']* 

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
| "bool"   { BOOL }
| "string" { STRING }
| "float"  { FLOAT }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "main"   { MAIN }
| "new"    { NEW }
| "def"    { DEF }
| "self"   { SELF }
| "@step"  { AT_STEP }
| '"'      {read_string (Buffer.create 17) lexbuf}
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*     as lxm { ID(lxm) }
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
  | _         { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof       { raise (SyntaxError ("String is not terminated")) }
