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

let string_of_stype = function
  | SFull -> "full"
  | SPre -> "pre"
  | SPost -> "post"
  | SExit -> "exit"
  | SSig -> "Sig"
  | SPrePost -> "prepost"
  | SRestart -> "restart"
  | SKill -> "kill"

(* accept: SFull, SExit, SSig, or SKill *)
let sexpr_of_syscall s =
  let pid = Int64.to_string s.pid in
  match s.stype with
  | SFull ->
    let name = match s.name with
      | Some name -> name
      | None -> failwith "missing name in SFull!" in
    Printf.sprintf "'(syscall %s %Ld %s)" pid s.timestamp name
  | SExit ->
    let exitcode = match s.exitcode with
      | Some exitcode -> exitcode
      | None -> failwith "missing exitcode in SExit!" in
    Printf.sprintf "'(exit %s %Ld %Ld)" pid s.timestamp exitcode
  | SSig ->
    let name = match s.name with
      | Some name -> name
      | None -> failwith "missing name in SSig!" in
    Printf.sprintf "'(signal %s %Ld %s)" pid s.timestamp name
  | SKill -> Printf.sprintf "'(killed %s %Ld)" pid s.timestamp
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
    failwith (Printf.sprintf "cannot merge %s and %s" t1 t2)
