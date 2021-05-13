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

let setup_dump_file () =
  if Config.pulse_train_mode then
    trace_file := Some (Stdlib.open_out_gen [Open_append; Open_creat] 0o666 logfile_path)
  (* else 
   *   load_dump_file () *)
