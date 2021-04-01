
val dump : 'a list -> unit
(** log debug info *)
val is_recording: unit -> bool
val is_replaying: unit -> bool

(** log management *)

val setup_dump_file : unit -> unit
(** Set up logging to go to the log file. Call this once the results directory has been set up. *)

val read : f:(In_channel.t -> 'a) -> 'a option
    
