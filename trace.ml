open Syscall

module IM = Map.Make(struct type t = int64 let compare = compare end)

(* full trace for one process
   - guarantee: `calls` contains only SFull, SExit, SSig, or SKill
   - `pcall` used to store SPre, SPost, SPrePost, SRestart *)
type trace = {
  calls: syscall list;
  pcall: syscall option; }

let trace_empty: trace = {
  calls = [];
  pcall = None; }

(* add call to one-process trace
   XXX assumes traces are in reverse order!
   split_trace reverses after using this *)
let trace_add (s: syscall) ({calls; pcall}: trace): trace =
  match s.stype, pcall with
  | SFull, None -> { calls = s :: calls; pcall = None; }
  | SPre, None -> { calls = calls; pcall = Some s; }
  | SPost, Some p -> { calls = merge_syscalls p s :: calls; pcall = None; }
  | SExit, None -> { calls = s :: calls; pcall = None; }
  | SSig, None -> { calls = s :: calls; pcall = None; }
  (* TODO check: hopefully this only occurs immediately before exit *)
  | SPrePost, Some p -> { calls = merge_syscalls p s :: calls; pcall = None; }
  (* TODO later: should merge all this onto preexisting call, but whatever *)
  | SRestart, None ->
    (* for now just turn SRestart into a fake SPre *)
    let fake_pre = { s with stype = SPre; args = Some []; } in
    { calls = calls; pcall = Some fake_pre; }
  | SKill, None -> { calls = s :: calls; pcall = None; }
  | _, Some _ ->
    let t = string_of_stype s.stype in
    failwith (Printf.sprintf "trace_add: got stype=%s with pcall=Some ..." t)
  | _, None ->
    let t = string_of_stype s.stype in
    failwith (Printf.sprintf "trace_add: got stype=%s with pcall=None" t)

(* split a system trace into a map of PIDs to traces *)
let split_trace trace =
  let do_add m s =
    match IM.find_opt s.pid m with
    | Some t -> IM.add s.pid (trace_add s t) m
    | None -> IM.add s.pid (trace_add s trace_empty) m in
  let do_rev {calls; pcall} =
    assert (Option.is_none pcall);
    List.rev calls in
  let rev_traces = List.fold_left do_add IM.empty trace in
  IM.map do_rev rev_traces

(* merge traces from many processes, sorted by timestamp *)
let merge_traces traces =
  let ts_compare s1 s2 = compare s1.timestamp s2.timestamp in
  let do_merge _pid pid_trace trace =
    List.merge ts_compare pid_trace trace in
  IM.fold do_merge traces []
