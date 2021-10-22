let opts = []
let file: string option ref = ref None

let usage = [
  "sparse [strace]";
  "";
  "output is s-expressions on stdout, good luck"]
  |> String.concat "\n"

let string_of_lexbuf_p lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line = p.pos_lnum
  and col = p.pos_cnum - p.pos_bol in
  string_of_int line^","^string_of_int col

let read input =
  let lexbuf = Lexing.from_channel input in
  try
    Ok (Parser.main Lexer.main lexbuf)
  with
  | Failure msg ->
    Error ("lex error at "^string_of_lexbuf_p lexbuf^": Failure "^msg)
  | Invalid_argument msg ->
    Error ("lex error at "^string_of_lexbuf_p lexbuf^": Invalid_argument "^msg)
  | Lexer.SyntaxError msg ->
    Error ("lex error at "^string_of_lexbuf_p lexbuf^": "^msg)
  | Parser.Error ->
    Error ("parse error at "^string_of_lexbuf_p lexbuf^
      " with token "^Lexing.lexeme lexbuf)

let main () =
  Arg.parse opts (fun f -> file := Some f) usage;

  let input = match !file with
    | None -> stdin
    | Some f -> open_in f in

  begin match !file with
  | None -> Printf.eprintf "input: <stdin>\n"
  | Some f -> Printf.eprintf "input: \"%s\"\n" f
  end;

  begin match read input with
  | Ok _ -> Printf.eprintf "success\n%!"
  | Error msg -> Printf.eprintf "%s\n%!" msg
  end;

  0

let _ = main () |> exit
