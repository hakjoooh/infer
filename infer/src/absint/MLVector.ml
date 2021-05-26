module F = Format

type t = int list

let pp f vs =
  let vs = List.map vs ~f:string_of_int
  in
  List.iter vs ~f:(fun x -> F.fprintf f "%s " x)

let concat (vs1: t) (vs2: t) = vs1 @ vs2

let equal (vs1: t) (vs2: t) = List.equal Int.equal vs1 vs2
let compare vs1 vs2 = List.compare Int.compare vs1 vs2

let vector vs = vs
let to_list (vs: t): int list = vs

module Key = struct
  type nonrec t = t [@@deriving compare]
end

module Set = Caml.Set.Make(Key)
module Map = Caml.Map.Make(Key)
