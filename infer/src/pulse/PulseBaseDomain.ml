(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
module Memory = PulseBaseMemory
module Stack = PulseBaseStack
module AddressAttributes = PulseBaseAddressAttributes

(* {2 Abstract domain description } *)

type t = {heap: Memory.t; stack: Stack.t; attrs: AddressAttributes.t}
[@@deriving compare, equal, yojson_of]

let empty =
  { heap=
      Memory.empty
      (* TODO: we could record that 0 is an invalid address at this point but this makes the
         analysis go a bit overboard with the Nullptr reports. *)
  ; stack= Stack.empty
  ; attrs= AddressAttributes.empty }


type cell = Memory.Edges.t * Attributes.t

let find_cell_opt addr {heap; attrs} =
  match (Memory.find_opt addr heap, AddressAttributes.find_opt addr attrs) with
  | None, None ->
      None
  | edges_opt, attrs_opt ->
      let edges = Option.value edges_opt ~default:Memory.Edges.empty in
      let attrs = Option.value attrs_opt ~default:Attributes.empty in
      Some (edges, attrs)

(** comparison between two elements of the domain to determine the [<=] relation

    Given two states [lhs] and [rhs], try to find a bijection [lhs_to_rhs] (with inverse
    [rhs_to_lhs]) between the addresses of [lhs] and [rhs] such that
    [lhs_to_rhs(reachable(lhs)) = reachable(rhs)] (where addresses are reachable if they are
    reachable from stack variables). *)

module GraphComparison = struct
  module AddressMap = AbstractValue.Map

  (** translation between the abstract values on the LHS and the ones on the RHS *)
  type mapping =
    { rhs_to_lhs: AbstractValue.t AddressMap.t  (** map from RHS values to LHS *)
    ; lhs_to_rhs: AbstractValue.t AddressMap.t  (** inverse map from [rhs_to_lhs] *) }

  let empty_mapping = {rhs_to_lhs= AddressMap.empty; lhs_to_rhs= AddressMap.empty}

  let pp_mapping fmt {rhs_to_lhs; lhs_to_rhs} =
    F.fprintf fmt "@[<v>{ rhs_to_lhs=@[<hv2>%a@];@,lhs_to_rhs=@[<hv2>%a@];@,}@]"
      (AddressMap.pp ~pp_value:AbstractValue.pp)
      rhs_to_lhs
      (AddressMap.pp ~pp_value:AbstractValue.pp)
      lhs_to_rhs


  (** try to add the fact that [addr_lhs] corresponds to [addr_rhs] to the [mapping] *)
  let record_equal ~addr_lhs ~addr_rhs mapping =
    (* have we seen [addr_lhs] before?.. *)
    (* let _ =
     *   L.d_printfln "* mapping";
     *   AddressMap.iter (fun k v ->
     *       L.d_printfln "%a -> %a" AbstractValue.pp k AbstractValue.pp v) mapping.lhs_to_rhs;
     *   L.d_printfln "* want to find %a" AbstractValue.pp addr_lhs in *)
    match AddressMap.find_opt addr_lhs mapping.lhs_to_rhs with
    | Some addr_rhs' when not (AbstractValue.equal addr_rhs addr_rhs') ->
        (* ...yes, but it was bound to another address *)
        L.d_printfln
          "Aliasing in LHS not in RHS: LHS address %a in current already bound to %a, not %a@\n\
           State=%a"
          AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs' AbstractValue.pp addr_rhs pp_mapping
          mapping ;
        `AliasingLHS
    | Some _addr_rhs (* [_addr_rhs = addr_rhs] *) ->
          (* L.d_printfln "* already visited %a->%a" AbstractValue.pp addr_lhs AbstractValue.pp _addr_rhs; *)
        `AlreadyVisited
    | None -> (
      (* ...and have we seen [addr_rhs] before?.. *)
        match AddressMap.find_opt addr_rhs mapping.rhs_to_lhs with
      | Some addr_lhs' ->
          (* ...yes, but it was bound to another address: [addr_lhs' != addr_lhs] otherwise we would
             have found [addr_lhs] in the [lhs_to_rhs] map above *)
          L.d_printfln
            "Aliasing in RHS not in LHS: RHS address %a in current already bound to %a, not %a@\n\
             State=%a"
            AbstractValue.pp addr_rhs AbstractValue.pp addr_lhs' AbstractValue.pp addr_lhs
            pp_mapping mapping ;
          `AliasingRHS
      | None ->
          (* L.d_printfln "* record %a<->%a" AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs; *)
          (* [addr_rhs] and [addr_lhs] are both new, record that they correspond to each other *)
          let mapping' =
            { rhs_to_lhs= AddressMap.add addr_rhs addr_lhs mapping.rhs_to_lhs
            ; lhs_to_rhs= AddressMap.add addr_lhs addr_rhs mapping.lhs_to_rhs }
          in
          `NotAlreadyVisited mapping' )


  type isograph_relation =
    | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
    | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

  (** can we extend [mapping] so that the subgraph of [lhs] rooted at [addr_lhs] is isomorphic to
      the subgraph of [rhs] rooted at [addr_rhs]? *)
  let rec isograph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping =
    L.d_printfln "%a<->%a@\n" AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs ;
    match record_equal mapping ~addr_lhs ~addr_rhs with
    | `AlreadyVisited -> (
        L.d_printfln "already visited";
        let get_non_empty_cell addr astate =
          find_cell_opt addr astate
          |> Option.filter ~f:(fun (edges, attrs) ->
                 not (Memory.Edges.is_empty edges && Attributes.is_empty attrs)
                 (* this can happen because of [register_address] or because we don't care to delete empty
                    edges when removing edges *) )
        in
        let lhs_cell_opt = get_non_empty_cell addr_lhs lhs in
        let rhs_cell_opt = get_non_empty_cell addr_rhs rhs in
        match (lhs_cell_opt, rhs_cell_opt) with
        | None, None ->
            L.d_printfln "keep";
            IsomorphicUpTo mapping
        | Some _, None ->
            L.d_printfln "notiso1 %a %a" AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs;
            NotIsomorphic
        | None, Some _ ->
            L.d_printfln "notiso2 %a %a" AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs;
            NotIsomorphic
        | Some (edges_lhs, attrs_lhs), Some (edges_rhs, attrs_rhs) ->
            (* continue the comparison recursively on all edges and attributes *)
            L.d_printfln "continue to edges";
            if Attributes.equal attrs_lhs attrs_rhs then
begin
              let bindings_lhs = Memory.Edges.bindings edges_lhs in
              let bindings_rhs = Memory.Edges.bindings edges_rhs in
              L.d_printfln "check attr left";
              List.iter bindings_lhs ~f:(fun (acc,trace) ->
                  L.d_printfln "- %a - %a" Memory.Access.pp acc Memory.AddrTrace.pp trace
                ) ;
              L.d_printfln "check attr right";
              List.iter bindings_rhs ~f:(fun (acc,trace) ->
                  L.d_printfln "- %a - %a" Memory.Access.pp acc Memory.AddrTrace.pp trace
                ) ;
              isograph_map_edges ~lhs ~edges_lhs:bindings_lhs ~rhs ~edges_rhs:bindings_rhs mapping
end
            else
begin
              L.d_printfln "notiso";
              NotIsomorphic 
end)
        (* IsomorphicUpTo mapping *)
    | `AliasingRHS | `AliasingLHS ->
        NotIsomorphic
    | `NotAlreadyVisited mapping -> (
        L.d_printfln "new visited with recorded";
        let get_non_empty_cell addr astate =
          find_cell_opt addr astate
          |> Option.filter ~f:(fun (edges, attrs) ->
                 not (Memory.Edges.is_empty edges && Attributes.is_empty attrs)
                 (* this can happen because of [register_address] or because we don't care to delete empty
                    edges when removing edges *) )
        in
        let lhs_cell_opt = get_non_empty_cell addr_lhs lhs in
        let rhs_cell_opt = get_non_empty_cell addr_rhs rhs in
        match (lhs_cell_opt, rhs_cell_opt) with
        | None, None ->
            L.d_printfln "keep";
            IsomorphicUpTo mapping
        | Some _, None ->
            L.d_printfln "notiso1 %a %a" AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs;
            NotIsomorphic
        | None, Some _ ->
            L.d_printfln "notiso2 %a %a" AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs;
            NotIsomorphic
        | Some (edges_lhs, attrs_lhs), Some (edges_rhs, attrs_rhs) ->
            (* continue the comparison recursively on all edges and attributes *)
            L.d_printfln "continue to edges";
            if Attributes.equal attrs_lhs attrs_rhs then
begin
              let bindings_lhs = Memory.Edges.bindings edges_lhs in
              let bindings_rhs = Memory.Edges.bindings edges_rhs in
              L.d_printfln "check attr left";
              List.iter bindings_lhs ~f:(fun (acc,trace) ->
                  L.d_printfln "- %a - %a" Memory.Access.pp acc Memory.AddrTrace.pp trace
                ) ;
              L.d_printfln "check attr right";
              List.iter bindings_rhs ~f:(fun (acc,trace) ->
                  L.d_printfln "- %a - %a" Memory.Access.pp acc Memory.AddrTrace.pp trace
                ) ;
              isograph_map_edges ~lhs ~edges_lhs:bindings_lhs ~rhs ~edges_rhs:bindings_rhs mapping
end
            else
begin
              L.d_printfln "notiso";
              NotIsomorphic 
end
)
            

  (** check that the isograph relation can be extended for all edges *)
  and isograph_map_edges ~lhs ~edges_lhs ~rhs ~edges_rhs mapping =
    match (edges_lhs, edges_rhs) with
    | [], [] ->
        (* L.d_printfln "done with edges"; *)
        IsomorphicUpTo mapping
    | (a_lhs, (addr_lhs, _trace_lhs)) :: edges_lhs, (a_rhs, (addr_rhs, _trace_rhs)) :: edges_rhs
      when Memory.Access.equal a_lhs a_rhs -> (
      (* check isograph relation from the destination addresses *)
        L.d_printfln "continue to address %a %a" AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs;
      match isograph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping with
      | IsomorphicUpTo mapping ->
          (* ok: continue with the other edges *)
          isograph_map_edges ~lhs ~edges_lhs ~rhs ~edges_rhs mapping
      | NotIsomorphic ->
          NotIsomorphic )
    | _ :: _, _ :: _ | [], _ :: _ | _ :: _, [] ->
        NotIsomorphic


  (** check that the memory graph induced by the addresses in [lhs] reachable from the variables in
      [stack_lhs] is a isograph of the same graph in [rhs] starting from [stack_rhs], up to some
      [mapping] *)
  let rec isograph_map_from_stack ~lhs ~stack_lhs ~rhs ~stack_rhs mapping =
    L.d_printfln "start stack elem";
    match (stack_lhs, stack_rhs) with
    | [], [] ->
        (* L.d_printfln "continue stack elem"; *)
        IsomorphicUpTo mapping
    | (var_lhs, (addr_lhs, _trace_lhs)) :: stack_lhs, (var_rhs, (addr_rhs, _trace_rhs)) :: stack_rhs
      when Var.equal var_lhs var_rhs -> (
        L.d_printfln "from stack %a <-> %a: %a<-%a@\n" Var.pp var_lhs Var.pp var_rhs AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs ;
        (* L.d_printfln "<->"; *)
      match isograph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping with
      | IsomorphicUpTo mapping ->
          (* L.d_printfln "go recursive"; *)
          isograph_map_from_stack ~lhs ~stack_lhs ~rhs ~stack_rhs mapping
      | NotIsomorphic ->
        (* L.d_printfln "not iso"; *)
          NotIsomorphic )
    | _ :: _, _ :: _ | [], _ :: _ | _ :: _, [] ->
        (* L.d_printfln "not isomorphic"; *)
        NotIsomorphic


  let isograph_map ~lhs ~rhs mapping =
    L.d_printfln "stack checks";
    let stack_lhs = Stack.bindings lhs.stack in
    let stack_rhs = Stack.bindings rhs.stack in
    (* let rec temp lhs rhs =
     *   match lhs, rhs with
     *   | [], [] -> ()
     *   | (var_lhs,_)::lhs, (var_rhs,_)::rhs ->
     *       L.d_printfln "stack check  %a <-> %a@\n" Var.pp var_lhs Var.pp var_rhs;
     *       temp lhs rhs
     *   | _ -> ()
     * in *)
    (* L.d_printfln "stack checks";
     * temp stack_lhs stack_rhs; *)
    let r = isograph_map_from_stack ~lhs ~rhs ~stack_lhs ~stack_rhs mapping in
    begin
      match r with
      | IsomorphicUpTo _ -> L.d_printfln "found a mapping"
      | NotIsomorphic -> L.d_printfln "failed to find a mapping"
    end;
    r


  let is_isograph ~lhs ~rhs mapping =
    match isograph_map ~lhs ~rhs mapping with IsomorphicUpTo _ -> true | NotIsomorphic -> false

  let rhs_to_lhs map = map.rhs_to_lhs
  let lhs_to_rhs map = map.lhs_to_rhs
  let rev_mapping map =
    { lhs_to_rhs = map.rhs_to_lhs
    ; rhs_to_lhs = map.lhs_to_rhs }
end

let pp fmt {heap; stack; attrs} =
  F.fprintf fmt "{@[<v1> roots=@[<hv>%a@];@;mem  =@[<hv>%a@];@;attrs=@[<hv>%a@];@]}" Stack.pp stack
    Memory.pp heap AddressAttributes.pp attrs


module GraphVisit : sig
  val fold :
       var_filter:(Var.t -> bool)
    -> t
    -> init:'accum
    -> f:
         (   Var.t
          -> 'accum
          -> AbstractValue.t
          -> Memory.Access.t list
          -> ('accum, 'final) Base.Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> AbstractValue.Set.t * 'final
  (** Generic graph traversal of the memory starting from each variable in the stack that pass
      [var_filter], in order. Returns the result of folding over every address in the graph and the
      set of addresses that have been visited before [f] returned [Stop] or all reachable addresses
      were seen. [f] is passed each address together with the variable from which the address was
      reached and the access path from that variable to the address. *)

  val fold_from_addresses :
       AbstractValue.t list
    -> t
    -> init:'accum
    -> f:
         (   'accum
          -> AbstractValue.t
          -> Memory.Access.t list
          -> ('accum, 'final) Base.Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> AbstractValue.Set.t * 'final
  (** Similar to [fold], but start from given addresses, instead of stack variables. *)
end = struct
  open Base.Continue_or_stop

  let visit address visited =
    if AbstractValue.Set.mem address visited then `AlreadyVisited
    else
      let visited = AbstractValue.Set.add address visited in
      `NotAlreadyVisited visited


  let rec visit_address address ~f rev_accesses astate ((visited, accum) as visited_accum) =
    match visit address visited with
    | `AlreadyVisited ->
        Continue visited_accum
    | `NotAlreadyVisited visited -> (
      match f accum address rev_accesses with
      | Continue accum -> (
        match Memory.find_opt address astate.heap with
        | None ->
            Continue (visited, accum)
        | Some edges ->
            visit_edges edges ~f rev_accesses astate (visited, accum) )
      | Stop fin ->
          Stop (visited, fin) )


  and visit_edges edges ~f rev_accesses astate visited_accum =
    let finish visited_accum = Continue visited_accum in
    Container.fold_until edges ~fold:Memory.Edges.fold ~finish ~init:visited_accum
      ~f:(fun visited_accum (access, (address, _trace)) ->
        match visit_access ~f access astate visited_accum with
        | Stop fin ->
            Stop (Stop fin)
        | Continue visited_accum -> (
          match visit_address address ~f (access :: rev_accesses) astate visited_accum with
          | Continue _ as cont ->
              cont
          | Stop fin ->
              Stop (Stop fin) ) )


  and visit_access ~f (access : Memory.Access.t) astate visited_accum =
    match access with
    | ArrayAccess (_, addr) ->
        visit_address addr ~f [] astate visited_accum
    | FieldAccess _ | TakeAddress | Dereference ->
        Continue visited_accum


  let visit_address_from_var (orig_var, (address, _loc)) ~f rev_accesses astate visited_accum =
    visit_address address ~f:(f orig_var) rev_accesses astate visited_accum


  let fold_common x astate ~fold ~filter ~visit ~init ~f ~finish =
    let finish (visited, accum) = (visited, finish accum) in
    let init = (AbstractValue.Set.empty, init) in
    Container.fold_until x ~fold ~init ~finish ~f:(fun visited_accum elem ->
        if filter elem then visit elem ~f [] astate visited_accum else Continue visited_accum )


  let fold ~var_filter astate =
    fold_common astate.stack astate
      ~fold:(IContainer.fold_of_pervasives_map_fold Stack.fold)
      ~filter:(fun (var, _) -> var_filter var)
      ~visit:visit_address_from_var


  let fold_from_addresses from astate =
    fold_common from astate ~fold:List.fold ~filter:(fun _ -> true) ~visit:visit_address
end

include GraphComparison

let reachable_addresses astate =
  GraphVisit.fold astate
    ~var_filter:(fun _ -> true)
    ~init:() ~finish:Fn.id
    ~f:(fun _ () _ _ -> Continue ())
  |> fst


let reachable_addresses_from addresses astate =
  GraphVisit.fold_from_addresses addresses astate ~init:() ~finish:Fn.id ~f:(fun () _ _ ->
      Continue () )
  |> fst


let subst_var subst ({heap; stack; attrs} as astate) =
  let open SatUnsat.Import in
  let* stack' = Stack.subst_var subst stack in
  let+ heap' = Memory.subst_var subst heap in
  let attrs' = AddressAttributes.subst_var subst attrs in
  if phys_equal heap heap' && phys_equal stack stack' && phys_equal attrs attrs' then astate
  else {heap= heap'; stack= stack'; attrs= attrs'}

(** for ML *)
let num_of_must_be_valid base =
  AddressAttributes.fold (fun _ attrs i ->
      if Attributes.get_must_be_valid attrs
         |> Option.is_some
      then i + 1
      else i)
    base.attrs 0

let num_of_written_to base =
  AddressAttributes.fold (fun _ attrs i ->
      if Attributes.get_written_to attrs
         |> Option.is_some
      then i + 1
      else i)
    base.attrs 0

let num_of_must_be_initialized base =
  AddressAttributes.fold (fun _ attrs i ->
      if Attributes.get_must_be_initialized attrs
         |> Option.is_some
      then i + 1
      else i)
    base.attrs 0

let num_of_invalids base =
  AddressAttributes.fold (fun _ attrs i ->
      if Attributes.get_invalid attrs
         |> Option.is_some
      then i + 1
      else i)
    base.attrs 0

let num_of_allocated base =
  AddressAttributes.fold (fun _ attrs i ->
      if Attributes.get_allocation attrs
         |> Option.is_some
      then i + 1
      else i)
    base.attrs 0

(* type t = {heap: Memory.t; stack: Stack.t; attrs: AddressAttributes.t} *)
let feature_vector t =
  let v1 = lazy (Memory.cardinal t.heap) in
  let v2 = lazy (num_of_must_be_valid t) in
  let v3 = lazy (num_of_written_to t) in
  let v4 = lazy (num_of_must_be_initialized t) in
  let v5 = lazy (num_of_invalids t) in
  let v6 = lazy (num_of_allocated t) in
  let v7 = lazy (Stack.cardinal t.stack) in
  [v1; v2; v3; v4; v5; v6; v7]

