open Syscall
open Trace

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

let print_syscall s =
  let name = match s.name with
    | Some name -> name
    | None -> failwith "nameless syscall" in
  let pid = Int64.to_string s.pid in
  Printf.printf "(%s %s)\n" pid name

let process trace =
  let full_trace = merge_traces (split_trace trace) in
  List.iter (fun s -> Printf.printf "%s\n" (sexpr_of_syscall s)) full_trace

let dump =
  List.iter (fun s ->
    let t = string_of_stype s.stype in
    let i = Int64.to_string s.pid in
    match s.name with
    | Some s -> Printf.printf "pid=%s type=%s call=%s\n" i t s
    | None -> Printf.printf "pid=%s type=%s\n" i t)

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
  | Ok trace -> process trace
  | Error msg -> Printf.eprintf "%s\n%!" msg
  end;

  0

let _ = main () |> exit
