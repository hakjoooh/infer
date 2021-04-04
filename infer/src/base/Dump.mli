module F = Format

val is_replaying: unit -> bool
val finalize : 'a list -> unit
val finalize_for_training : ((('a, F.formatter, unit) format -> 'a) -> unit) -> unit


val setup_dump_file : unit -> unit
(** Set up logging to go to the log file. Call this once the results directory has been set up. *)

val read : f:(In_channel.t -> 'a) -> 'a option
    
