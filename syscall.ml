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
| ArgKV of string * value
| ArgIndex of string * value * value

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
