(* module L = Logging *)
module F = Format
  
let trace_file = ref None
let replay_mode = ref false

let is_replaying () = !replay_mode

let finalize list =
  match !trace_file with
  | Some(chan) ->
      Marshal.to_channel chan list [];
      Out_channel.close chan
  | _ -> ()

let finalize_for_training f =
  let training_txt = ResultsDirEntryName.get_path ~results_dir:"./" TrainingDataSet in
  let chan = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 training_txt in
  let fmt = F.formatter_of_out_channel chan in
  let k_force_newline f = F.pp_force_newline f () in
  let println f = F.kfprintf k_force_newline fmt f in
  f println;
  Out_channel.close chan


let logfile_path =
  ResultsDirEntryName.get_path ~results_dir:"./" TraceForML
  (* ResultsDirEntryName.get_path ~results_dir:Config.toplevel_results_dir TraceForML *)

let read ~f = 
  if !replay_mode then
    let chan = Stdlib.open_in_bin logfile_path in
    let v = f chan in
    In_channel.close chan;
    Some(v)
  else
    None

(* let load_dump_file () =
 *   let x = Sys.file_exists logfile_path in
 *   let preexisting_file = PolyVariantEqual.( = ) x `Yes in
 *   if preexisting_file then
 *     begin
 *       L.debug Analysis Quiet "loading traces.@\n";
 *       replay_mode := true
 *     end *)

let setup_dump_file () =
  if Config.pulse_train_mode then
    begin
      (* L.debug Analysis Quiet "create traces.@\n"; *)
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
      trace_file := Some (chan)
    end
  (* else 
   *   load_dump_file () *)
