(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* log files *)

(* can be set up to emit to a file later on *)
module L = Logging

let log_file = ref None

type mode = RecordTrace | ReplayTrace
let running_mode = ref None
let is_recording () = match !running_mode with | Some(RecordTrace) -> true | _ -> false
let is_replaying () = match !running_mode with | Some(ReplayTrace) -> true | _ -> false

let dump list =
  match !log_file with
  | Some(chan) ->
      Marshal.to_channel chan list [];
      Out_channel.close chan
  | _ -> ()

let logfile_path =
  ResultsDirEntryName.get_path ~results_dir:"./" TraceForML
  (* ResultsDirEntryName.get_path ~results_dir:Config.toplevel_results_dir TraceForML *)

let read ~f = 
  if is_replaying () then
    let chan = Stdlib.open_in_bin logfile_path in
    let v = f chan in
    In_channel.close chan;
    Some(v)
  else
    None

let load_dump_file () =
  let x = (Sys.file_exists logfile_path) in
  let preexisting_file = PolyVariantEqual.( = ) x `Yes in
  if preexisting_file then
    begin
      L.debug Analysis Quiet "loading traces.@\n";
      running_mode := Some(ReplayTrace);
      true
    end
  else
    begin
      running_mode := Some(RecordTrace);
      false
    end

(* create new channel from the log file, and dumps the contents of the temporary log buffer there *)
let setup_dump_file () =
  if not (load_dump_file ()) then
    match !log_file with
    | Some _ ->
        (* already set up *)
        ()
    | None ->
        L.debug Analysis Quiet "create traces.@\n";
        let chan =
          (* if invoked in a sub-dir (e.g., in Buck integrations), log inside the original log file *)
          (* assumes the results dir exists already *)
          (* let logfile_path =
           *   ResultsDirEntryName.get_path ~results_dir:Config.toplevel_results_dir TraceForML
           * in *)
          (* let preexisting_logfile = PolyVariantEqual.( = ) (Sys.file_exists logfile_path) `Yes in *)
          let chan = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 logfile_path in
          chan
        in
        log_file := Some (chan)
(* ;
       * if CLOpt.is_originator && preexisting_logfile then
       *   phase
       *     "============================================================@\n\
       *      = New infer execution begins@\n\
       *      ============================================================" *)
