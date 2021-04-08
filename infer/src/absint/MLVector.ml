module L = Logging
module F = Format

type t = Vector of float list
       | LazyVector of float lazy_t list

let pp f vs =
  let vs =
    match vs with
    | Vector vs -> vs
    | LazyVector vsl -> List.map vsl ~f:Lazy.force_val
  in
  List.iter vs ~f:(fun x -> F.fprintf f "%s " (string_of_int (int_of_float x)))

let rec compute vs1 vs2 acc =
  match vs1, vs2 with
  | [], [] -> acc
  | [], _ | _, [] ->
      L.debug Analysis Quiet "The given vector size doesn't match with the number of features.@\n";
      acc
  | x::xs, y::ys ->
      let acc =
        if Float.equal x 0.0 then acc
        else acc +. x *. (Lazy.force_val y)
      in
      compute xs ys acc

let mult (vs1: t) (vs2: t) = 
  match vs1, vs2 with
  | Vector vs1, LazyVector vs2 -> compute vs1 vs2 0.
  | _ -> 0.

let vector vs = Vector vs
let lazy_vector vs = LazyVector vs

let compare e1 e2 =
  let e1 =
    match e1 with
    | Vector vs1 -> vs1
    | LazyVector vs1 -> List.map vs1 ~f:(fun x -> Lazy.force_val x)
  in
  let e2 =
    match e2 with
    | Vector vs1 -> vs1
    | LazyVector vs1 -> List.map vs1 ~f:(fun x -> Lazy.force_val x)
  in
  List.compare Float.compare e1 e2

module Key = struct
  type nonrec t = t [@@deriving compare]
end

module Set = Caml.Set.Make(Key)
