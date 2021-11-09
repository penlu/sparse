type value =
| ValueQues
| ValueString of string
| ValuePString of string (* "asdf"... *)
| ValueAString of string (* @"asdf" (for some reason) *)
| ValueHexnum of int64
| ValueDecnum of int64
| ValueOctnum of int64
| ValueIpv4 of int list
| ValueIpv6 of int list
| ValueId of string
| ValueCall of string * arg list
| ValueArrow of int64 * int64 (* I have no clue what this is *)
| ValueArray of value list
| ValueBitset of bool * value list (* bool: is negated? *)
| ValueStruct of arg list
| ValueFlagset of value list (* | *)
| ValueProduct of value list (* * *)
| ValueWstatus of (value * value option) list (* && *)
| ValueLshift of value * value (* << *)
| ValueNeg of value (* - *)
| ValueOr of value * value (* `or` *)

and arg =
| ArgIncomplete (* ... *)
| ArgV of value
| ArgKV of string * value (* key = value *)
| ArgIndex of string * value * value (* key = value[index] *)

type stype =
| SFull
| SPre
| SPost
| SExit
| SSig
| SPrePost
| SRestart
| SKill

(* times are in microseconds *)
type syscall = {
  stype: stype;
  pid: int64;
  timestamp: int64;
  duration: int64 option;

  name: string option;
  args: arg list option;

  retval: value option;
  errno: string option;
  comment: string option;

  exitcode: int64 option; }

(* TODO *)
let string_of_value = function
  | ValueQues -> "'?"
  | ValueString s -> Printf.sprintf "\"%s\"" s
  | ValuePString s -> Printf.sprintf "\"%s\"..." s
  | ValueAString s -> Printf.sprintf "@\"%s\"" s
  | ValueHexnum n -> Printf.sprintf "%Ld" n
  | ValueDecnum n -> Printf.sprintf "%Ld" n
  | ValueOctnum n -> Printf.sprintf "%Ld" n
  | ValueIpv4 _ -> Printf.sprintf "[ipv4]"
  | ValueIpv6 _ -> Printf.sprintf "[ipv6]"
  | ValueId s -> "'"^s
  | ValueCall _ -> "call"
  | ValueArrow _ -> "arrow"
  | ValueArray _ -> "array"
  | ValueBitset _ -> "bitset"
  | ValueStruct _ -> "struct"
  | ValueFlagset _ -> "flagset"
  | ValueProduct _ -> "product"
  | ValueWstatus _ -> "wstatus"
  | ValueLshift _ -> "lshift"
  | ValueNeg _ -> "neg"
  | ValueOr _ -> "or"

let string_of_stype = function
  | SFull -> "full"
  | SPre -> "pre"
  | SPost -> "post"
  | SExit -> "exit"
  | SSig -> "sig"
  | SPrePost -> "prepost"
  | SRestart -> "restart"
  | SKill -> "kill"

(* expect a flagset? this is your function *)
let string_of_flags = function
  | ValueDecnum d ->
    if d <> Int64.zero then
      let s = Int64.to_string d in
      let msg = Printf.sprintf "string_of_flags: got %s, expected 0" s in
      failwith msg
    else
      "'()"
  | ValueId s ->
    Printf.sprintf "(list '%s)" s
  | ValueFlagset flags ->
    let string_of_flag = function
      | ValueId s -> "'"^s
      | _ -> failwith "string_of_flags: expected ValueId in flagset" in
    let flag_strs = List.map string_of_flag flags in
    let s = String.concat " " flag_strs in
    Printf.sprintf "(list %s)" s
  | _ -> failwith "string_of_flags: unexpected value type"

let args_of_clone args =
  let string_of_arg = function
    | ArgKV ("flags", v) ->
      let s = string_of_flags v in
      [Printf.sprintf "(cons 'flags %s)" s]
    | ArgKV (_, _) -> []
    | _ -> failwith "args_of_clone: expected ArgKV" in
  let args = List.map string_of_arg args in
  List.concat args

let args_of_open = function
  | [ArgV path; ArgV flags] ->
    let path_str = string_of_value path in
    let flags_str = string_of_flags flags in
    [path_str; flags_str]
  | _ as l ->
    let len = List.length l in
    let msg = Printf.sprintf "args_of_open: expected 2 args, got %d" len in
    failwith msg

let args_of_openat = function
  | [ArgV dirfd; ArgV path; ArgV flags] ->
    assert (dirfd = ValueId "AT_FDCWD");
    let path_str = string_of_value path in
    let flags_str = string_of_flags flags in
    [path_str; flags_str]
  | [ArgV dirfd; ArgV path; ArgV flags; ArgV mode] ->
    assert (dirfd = ValueId "AT_FDCWD");
    let path_str = string_of_value path in
    let flags_str = string_of_flags flags in
    let mode_str = string_of_value mode in
    [path_str; flags_str; mode_str]
  | _ as l ->
    let len = List.length l in
    let msg = Printf.sprintf "args_of_open: expected 3 or 4 args, got %d" len in
    failwith msg

let args_of_setns = function
  | [ArgV fd; ArgV flags] ->
    let fd_str = string_of_value fd in
    let flags_str = string_of_flags flags in
    [fd_str; flags_str]
  | _ as l ->
    let len = List.length l in
    let msg = Printf.sprintf "args_of_setns: expected 2 args, got %d" len in
    failwith msg

let sexpr_of_list = function
  | [] -> "'()"
  | l -> Printf.sprintf "(list %s)" (String.concat " " l)

(* basically just dispatches to appropriate args_of_[callname] *)
let args_of_syscall name args =
  let arg_strs = match name, args with
    | "clone", Some args -> args_of_clone args
    | "open", Some args -> args_of_open args
    | "openat", Some args -> args_of_openat args
    | "setns", Some args -> args_of_setns args
    | _ -> [] in
  sexpr_of_list arg_strs

(* accept: SFull, SExit, SSig, or SKill
   produce: sexpr encoding the syscall *)
let sexpr_of_syscall s =
  let pid = Int64.to_string s.pid in
  match s.stype with
  | SFull ->
    let name = match s.name with
      | Some name -> name
      | None -> failwith "sexpr_of_syscall: missing name in SFull!" in
    let args = args_of_syscall name s.args in
    let retval = match s.retval with
      | None -> "'()"
      | Some v -> Printf.sprintf "%s" (string_of_value v) in
    Printf.sprintf "(trace-syscall %s %Ld \"%s\" %s %s)"
      pid s.timestamp name args retval
  | SExit ->
    let exitcode = match s.exitcode with
      | Some exitcode -> exitcode
      | None -> failwith "sexpr_of_syscall: missing exitcode in SExit!" in
    Printf.sprintf "(trace-exit %s %Ld %Ld)" pid s.timestamp exitcode
  | SSig ->
    let name = match s.name with
      | Some name -> name
      | None -> failwith "sexpr_of_syscall: missing name in SSig!" in
    Printf.sprintf "(trace-signal %s %Ld '%s)" pid s.timestamp name
  | SKill -> Printf.sprintf "(trace-killed %s %Ld)" pid s.timestamp
  | _ ->
    let t = string_of_stype s.stype in
    failwith (Printf.sprintf "sexpr_of_syscall: got %s" t)

(* merge SPre with SPost or SPrePost *)
(* TODO later: proper SRestart merging *)
let merge_syscalls s1 s2 =
  match s1.stype, s2.stype with
  | SPre, SPost ->
    assert (s1.pid = s2.pid);
    assert (Option.get s1.name = Option.get s2.name ||
            Option.get s2.name = "restart_syscall");
    { stype = SFull;
      pid = s1.pid;
      timestamp = s1.timestamp;
      duration = s2.duration;

      name = s1.name;
      args = Some (Option.get s1.args @ Option.get s2.args);

      retval = s2.retval;
      errno = s2.errno;
      comment = s2.comment;

      exitcode = None; }
  | SPre, SPrePost ->
    assert (s1.pid = s2.pid);
    { stype = SFull;
      pid = s1.pid;
      timestamp = s1.timestamp;
      duration = Some (Int64.sub s2.timestamp s1.timestamp);

      name = s1.name;
      args = s1.args;

      retval = s2.retval;
      errno = s2.errno;
      comment = s2.comment;

      exitcode = None; }
  | _ ->
    let t1 = string_of_stype s1.stype
    and t2 = string_of_stype s2.stype in
    failwith (Printf.sprintf "merge_syscalls: cannot merge %s and %s" t1 t2)
