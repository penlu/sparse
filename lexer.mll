{
  open Parser

  exception SyntaxError of string

  let tail s n = String.sub s n (String.length s - n)

  type lexstate = LexNormal | LexPost
}

let hex = ['0'-'9''a'-'f''A'-'F']
let dig = ['0'-'9']
let oct = ['0'-'7']
let ipv6sep = ':' ':'?

rule main = parse
| [' ' '\t'] { main lexbuf }
| "/*" { comment lexbuf }
| '"' { read_string (Buffer.create 17) lexbuf }
| '0' oct oct? oct?
  { OCTNUM (Int64.of_string ("0o"^Lexing.lexeme lexbuf)) }
| '-'? dig+
  { DECNUM (Int64.of_string (Lexing.lexeme lexbuf)) }
| "0x" hex+
  { HEXNUM (Int64.of_string (Lexing.lexeme lexbuf)) }
| dig+ '.' dig+ '.' dig+ '.' dig+
  { let s = Lexing.lexeme lexbuf in
    let field_strs = String.split_on_char '.' s in
    let fields = List.map int_of_string field_strs in
    IPV4 fields }
| "::" (hex+ (ipv6sep hex+)* as s)
  { let field_strs = String.split_on_char ':' s in
    let fields = List.map (fun s -> int_of_string ("0x"^s)) field_strs in
    let zeros = List.init (7 - List.length fields) (fun _ -> 0) in
    IPV6 (zeros @ fields) }
| (hex+ (ipv6sep hex+)+ as s)
  { let field_strs = String.split_on_char ':' s in
    let zeros = List.init (7 - List.length field_strs) (fun _ -> 0) in
    let add_zeros s =
      if s = "" then zeros
      else [int_of_string ("0x"^s)] in
    let fields = List.map add_zeros field_strs in
    IPV6 (List.concat fields) }
| "or" { ORLIT }
| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']*
  { ID (Lexing.lexeme lexbuf) }
| '\n' { Lexing.new_line lexbuf; NEWLINE }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '<' { LT }
| '>' { GT }
| '.' { DOT }
| ',' { COMMA }
| '=' { EQUAL }
| '|' { PIPE }
| "-" { DASH }
| '~' { TILDE }
| '?' { QUES }
| '*' { STAR }
| '/' { SLASH }
| '@' { AT }
| "<<" { LTLT }
| "&&" { ANDAND }
| "==" { EQUALEQUAL }
| "->" { RARROW }
| "..." { DOTDOTDOT }
| "---" { DASHDASHDASH }
| "+++" { PLUSPLUSPLUS }
| "<unfinished ...>" { UNFINISHED }
| "exited with" { EXITED }
| "<... " (['A'-'Z''a'-'z''0'-'9''_']* as s) " resumed>"
  { RESUMED s }
| "<... resuming interrupted " (['A'-'Z''a'-'z''0'-'9''_']* as s) " ...>"
  { RESUMING s }
| "+++ killed by SIGKILL +++" { KILLED }
| eof { EOF }

and read_string buf = parse
| '"' { STRING (Buffer.contents buf) }
| "\\e" { Buffer.add_char buf '\033'; read_string buf lexbuf }
| "\\f" { Buffer.add_char buf '\014'; read_string buf lexbuf }
| "\\r" { Buffer.add_char buf '\015'; read_string buf lexbuf }
| "\\n" { Buffer.add_char buf '\012'; read_string buf lexbuf }
| "\\t" { Buffer.add_char buf '\011'; read_string buf lexbuf }
| "\\v" { Buffer.add_char buf '\013'; read_string buf lexbuf }
| "\\\"" { Buffer.add_char buf '"'; read_string buf lexbuf }
| "\\\\" { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' (['0'-'3'] ['0'-'7'] ['0'-'7'] as s)
| '\\' (['0'-'7'] ['0'-'7'] as s)
| '\\' (['0'-'7'] as s)
  { Buffer.add_char buf (Char.chr (int_of_string ("0o"^s)));
    read_string buf lexbuf }
| "\\" ('x' ['0'-'9''a'-'f']['0'-'9''a'-'f'] as s)
  { Buffer.add_char buf (Char.chr (int_of_string ("0"^s)));
    read_string buf lexbuf }
| [^ '"' '\\']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string buf lexbuf }
| _ { raise (SyntaxError ("Illegal string character: "^Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError "String not terminated: got EOF") }

and comment = parse
| "*/" { main lexbuf }
| _ { comment lexbuf }
| eof { raise (SyntaxError "Comment not terminated: got EOF") }
