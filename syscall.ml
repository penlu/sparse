type value =
| ValueQues
| ValueString of string
| ValuePString of string (* elliptical *)
| ValueHexnum of int64
| ValueDecnum of int64
| ValueOctnum of int64
| ValueIpv4 of int list
| ValueIpv6 of int list
| ValueId of string
| ValueArray of value list
| ValueBitset of bool * value list (* bool: is negated? *)
| ValueFlagset of value list
| ValueProduct of value list
| ValueStruct of arg list
| ValuePStruct of arg list
| ValueCall of string * arg list
| ValueArrow of int64 * int64 (* I have no clue what this is *)
| ValueWstatus of (string * int64 option) list

and arg =
| ArgV of value
| ArgKV of string * value

type stype = SFull | SPre | SPost | SExit | SSig | SPrePost

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
