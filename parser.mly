%{
  open Syscall

  type atype = IsArray | IsBitset
%}

%token <int64> OCTNUM DECNUM HEXNUM
%token <int list> IPV4 IPV6
%token <string> STRING ID RESUMED
%token NEWLINE EOF
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LANGLE RANGLE
%token DOT COMMA EQUAL PIPE DASH TILDE QUES STAR RARROW ANDAND
%token DOTDOTDOT DASHDASHDASH PLUSPLUSPLUS UNFINISHED EXITED

%right PIPE STAR

%start <syscall list> main

%%

main:
| s=syscall NEWLINE m=main { s :: m }
| s=syscall EOF { [s] }
| EOF { [] }

syscall:
| pid=DECNUM ts=time name=ID LPAREN aa=args RPAREN
  EQUAL retval=value err=errno c=comment d=duration
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
  EQUAL retval=value err=errno c=comment d=duration
  { { stype=SPost; pid=pid; timestamp=ts; duration=d;
      name=Some name; args=Some aa;
      retval=Some retval; errno=err; comment=c;
      exitcode=None; } }
| pid=DECNUM ts=time name=RESUMED UNFINISHED RPAREN
  EQUAL retval=value err=errno c=comment d=duration
  { { stype=SPrePost; pid=pid; timestamp=ts; duration=d;
      name=Some name; args=None;
      retval=Some retval; errno=err; comment=c;
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

duration:
| { None }
| LANGLE t=time RANGLE { Some t }

time:
| t1=DECNUM DOT t2=DECNUM { Int64.add (Int64.mul t1 1000000L) t2 }

args:
| COMMA aa=args { aa }
| a=arg COMMA aa=args { a :: aa }
| a=arg { [a] }
| { [] }

arg:
| v=value { ArgV v }
| k=ID EQUAL v=value { ArgKV (k, v) }
| x=ID LPAREN aa=args RPAREN { ArgV (ValueCall (x, aa)) }
| k=ID EQUAL x=ID LPAREN aa=args RPAREN { ArgKV (k, ValueCall (x, aa)) }

value:
| QUES { ValueQues }
| s=STRING { ValueString s }
| s=STRING DOTDOTDOT { ValuePString s }
| n=OCTNUM { ValueOctnum n }
| n=DECNUM { ValueDecnum n }
| n=HEXNUM { ValueHexnum n }
| x=ID { ValueId x }
| LBRACKET v=value_array RBRACKET
  { match v with
    | (IsArray, v) -> ValueArray v
    | (IsBitset, v) -> ValueBitset (false, v) }
| TILDE LBRACKET v=value_array RBRACKET
  { let (_, v) = v in ValueBitset (true, v) }
| LBRACE aa=args RBRACE { ValueStruct aa }
| LBRACE aa=args DOTDOTDOT RBRACE { ValuePStruct aa }
| v1=value PIPE v2=value
  { match v2 with
    | ValueFlagset v2 -> ValueFlagset (v1 :: v2)
    | _ -> ValueFlagset (v1 :: [v2]) }
| v1=value STAR v2=value
  { match v2 with
    | ValueProduct v2 -> ValueProduct (v1 :: v2)
    | _ -> ValueProduct (v1 :: [v2]) }
| v1=DECNUM RARROW v2=DECNUM { ValueArrow (v1, v2) }
| LBRACKET LBRACE ww=wstatuses RBRACE RBRACKET { ValueWstatus ww }
| v=IPV4 { ValueIpv4 v }
| v=IPV6 { ValueIpv6 v }

value_array:
| v=value COMMA vv=value_array
  { let (atype, vv) = vv in (atype, v :: vv) }
| v=value vv=value_array
  { let (_, vv) = vv in (IsBitset, v :: vv) }
| { (IsArray, []) }

wstatuses:
| w=wstatus ANDAND ww=wstatuses { w :: ww }
| w=wstatus { [w] }

wstatus:
| w=ID LPAREN s=ID RPAREN { assert (s = "s"); (w, None) }
| w=ID LPAREN s=ID RPAREN EQUAL EQUAL v=DECNUM
  { assert (s = "s"); (w, Some v) }
