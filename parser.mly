%{
  open Syscall

  type atype = IsArray | IsBitset
%}

%token <int64> OCTNUM DECNUM HEXNUM
%token <int list> IPV4 IPV6
%token <string> STRING ID RESUMED RESUMING
%token NEWLINE EOF
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LT GT
%token DOT COMMA EQUAL PIPE DASH TILDE QUES STAR AT LTLT ORLIT SLASH
%token RARROW ANDAND EQUALEQUAL DOTDOTDOT DASHDASHDASH PLUSPLUSPLUS
%token UNFINISHED EXITED KILLED

%start <syscall list> main

%%

main:
| s=syscall NEWLINE m=main { s :: m }
| s=syscall EOF { [s] }
| EOF { [] }

syscall:
| pid=DECNUM ts=time name=ID LPAREN aa=args RPAREN
  EQUAL retval=atom err=errno c=comment d=duration
  { { stype=SFull; pid=pid; timestamp=ts;
      name=Some name; args=Some aa; duration=d;
      retval=Some retval; errno=err; comment=c;
      exitcode=None; } }
| pid=DECNUM ts=time name=ID LPAREN aa=args UNFINISHED
  { { stype=SPre; pid=pid; timestamp=ts;
      name=Some name; args=Some aa; duration=None;
      retval=None; errno=None; comment=None;
      exitcode=None; } }
| pid=DECNUM ts=time name=RESUMED aa=args RPAREN
  EQUAL retval=atom err=errno c=comment d=duration
  { { stype=SPost; pid=pid; timestamp=ts; duration=d;
      name=Some name; args=Some aa;
      retval=Some retval; errno=err; comment=c;
      exitcode=None; } }
| pid=DECNUM ts=time name=RESUMED UNFINISHED RPAREN
  EQUAL retval=atom err=errno c=comment d=duration
  { { stype=SPrePost; pid=pid; timestamp=ts; duration=d;
      name=Some name; args=None;
      retval=Some retval; errno=err; comment=c;
      exitcode=None; } }
| pid=DECNUM ts=time restart=ID LPAREN name=RESUMING UNFINISHED
  { assert (restart = "restart_syscall");
    { stype=SRestart; pid=pid; timestamp=ts;
      name=Some name; args=None; duration=None;
      retval=None; errno=None; comment=None;
      exitcode=None; } }
| pid=DECNUM ts=time PLUSPLUSPLUS EXITED code=DECNUM PLUSPLUSPLUS
  { { stype=SExit; pid=pid; timestamp=ts;
      name=None; args=None; duration=None;
      retval=None; errno=None; comment=None;
      exitcode=Some code; } }
| pid=DECNUM ts=time DASHDASHDASH signal=ID LBRACE aa=args RBRACE DASHDASHDASH
  { { stype=SSig; pid=pid; timestamp=ts;
      name=Some signal; args=Some aa; duration=None;
      retval=None; errno=None; comment=None;
      exitcode=None; } }
| pid=DECNUM ts=time KILLED
  { { stype=SKill; pid=pid; timestamp=ts;
      name=None; args=None; duration=None;
      retval=None; errno=None; comment=None;
      exitcode=None; } }

errno:
| { None }
| x=ID { Some x }

(* here's an unfortunate hack *)
comment:
| { None }
| LPAREN c=comments RPAREN { Some c }

comments:
| { "" }
| x=ID c=comments { x^" "^c }
| PIPE c=comments { "|"^c }
| DASH c=comments { "-"^c }
| ORLIT c=comments { " or "^c }
| SLASH c=comments { "/"^c }

duration:
| { None }
| LT t=time GT { Some t }

time:
| t1=DECNUM DOT t2=DECNUM { Int64.add (Int64.mul t1 1000000L) t2 }

args:
| COMMA aa=args { aa }
| a=arg COMMA aa=args { a :: aa }
| a=arg { [a] }
| { [] }

arg:
| DOTDOTDOT { ArgIncomplete }
| v=op_value { ArgV v }
| k=ID EQUAL v=op_value { ArgKV (k, v) }
| k=ID LBRACKET i=value RBRACKET EQUAL v=op_value
  { ArgIndex (k, i, v) }

atom:
| QUES { ValueQues }
| n=OCTNUM { ValueOctnum n }
| n=DECNUM { ValueDecnum n }
| n=HEXNUM { ValueHexnum n }
| v=IPV4 { ValueIpv4 v }
| v=IPV6 { ValueIpv6 v }
| x=ID { ValueId x }

value:
| v=atom { v }
| s=STRING { ValueString s }
| s=STRING DOTDOTDOT { ValuePString s }
| AT s=STRING { ValueAString s }
| x=ID LPAREN aa=args RPAREN { ValueCall (x, aa) }
| v1=DECNUM RARROW v2=DECNUM { ValueArrow (v1, v2) }
| LBRACKET v=value_array RBRACKET
  { match v with
    | (IsArray, v) -> ValueArray v
    | (IsBitset, v) -> ValueBitset (false, v) }
| TILDE LBRACKET v=value_array RBRACKET
  { let (_, v) = v in ValueBitset (true, v) }
| LBRACE aa=args RBRACE { ValueStruct aa }

op_value:
| v=lshift_value { v }
| DASH v=value { ValueNeg v }
| v1=value EQUALEQUAL v2=value { ValueWstatus [(v1, Some v2)] }
| v1=lshift_value PIPE v2=pipe_value { ValueFlagset (v1 :: v2) }
| v1=value STAR v2=star_value { ValueProduct (v1 :: v2) }
| v1=wstatus ANDAND v2=and_value { ValueWstatus (v1 :: v2) }
| v1=value ORLIT v2=value { ValueOr (v1, v2) }

lshift_value:
| v=value { v }
| v1=value LTLT v2=value { ValueLshift (v1, v2) }

pipe_value:
| v1=lshift_value PIPE v2=pipe_value { v1 :: v2 }
| v=lshift_value { [v] }

star_value:
| v1=value STAR v2=star_value { v1 :: v2 }
| v=value { [v] }

and_value:
| v1=wstatus ANDAND v2=and_value { v1 :: v2 }
| v=wstatus { [v] }

wstatus:
| v=value { (v, None) }
| v1=value EQUALEQUAL v2=value { (v1, Some v2) }

value_array:
| v=op_value COMMA vv=value_array
  { let (atype, vv) = vv in (atype, v :: vv) }
| v=op_value vv=value_array
  { let (_, vv) = vv in (IsBitset, v :: vv) }
| { (IsArray, []) }
