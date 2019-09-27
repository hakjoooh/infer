(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for the checks called by Eradicate. *)

(* TODO(T54088319) get rid of annotation_deprecated:
  - move all usages related to nullability to AnnotatedNullability.
  - introduce "field flags" and move all other usages to this dedicated datatype
  *)
type field_type = {annotation_deprecated: Annot.Item.t; annotated_type: AnnotatedType.t}

let get_field_annotation tenv fn typ =
  let lookup = Tenv.lookup tenv in
  let type_and_annotation_to_field_type (typ, annotation) =
    { annotation_deprecated= annotation
    ; annotated_type=
        AnnotatedType.{nullability= AnnotatedNullability.of_annot_item annotation; typ} }
  in
  Option.map
    (Typ.Struct.get_field_type_and_annotation ~lookup fn typ)
    ~f:type_and_annotation_to_field_type


let report_error tenv = TypeErr.report_error tenv (EradicateCheckers.report_error tenv)

let explain_expr tenv node e =
  match Errdesc.exp_rv_dexp tenv node e with
  | Some de ->
      Some (DecompiledExp.to_string de)
  | None ->
      None


let is_virtual = function
  | AnnotatedSignature.{mangled} :: _ when Mangled.is_this mangled ->
      true
  | _ ->
      false


(** Check an access (read or write) to a field. *)
let check_field_access tenv find_canonical_duplicate curr_pname node instr_ref exp fname
    inferred_nullability loc : unit =
  if InferredNullability.is_nullable inferred_nullability then
    let origin_descr = InferredNullability.descr_origin inferred_nullability in
    report_error tenv find_canonical_duplicate
      (TypeErr.Null_field_access (explain_expr tenv node exp, fname, origin_descr, false))
      (Some instr_ref) loc curr_pname


(** Check an access to an array *)
let check_array_access tenv find_canonical_duplicate curr_pname node instr_ref array_exp fname
    inferred_nullability loc indexed =
  if InferredNullability.is_nullable inferred_nullability then
    let origin_descr = InferredNullability.descr_origin inferred_nullability in
    report_error tenv find_canonical_duplicate
      (TypeErr.Null_field_access (explain_expr tenv node array_exp, fname, origin_descr, indexed))
      (Some instr_ref) loc curr_pname


(** Where the condition is coming from *)
type from_call =
  | From_condition  (** Direct condition *)
  | From_instanceof  (** x instanceof C *)
  | From_is_false_on_null  (** returns false on null *)
  | From_is_true_on_null  (** returns true on null *)
  | From_containsKey  (** x.containsKey *)
[@@deriving compare]

let equal_from_call = [%compare.equal: from_call]

(** Check the normalized "is zero" or "is not zero" condition of a prune instruction. *)
let check_condition tenv case_zero find_canonical_duplicate curr_pdesc node e typ
    inferred_nullability true_branch from_call idenv linereader loc instr_ref : unit =
  let contains_instanceof_throwable pdesc node =
    (* Check if the current procedure has a catch Throwable. *)
    (* That always happens in the bytecode generated by try-with-resources. *)
    let loc = Procdesc.Node.get_loc node in
    let throwable_found = ref false in
    let typ_is_throwable {Typ.desc} =
      match desc with
      | Typ.Tstruct (Typ.JavaClass _ as name) ->
          String.equal (Typ.Name.name name) "java.lang.Throwable"
      | _ ->
          false
    in
    let do_instr = function
      | Sil.Call (_, Exp.Const (Const.Cfun pn), [_; (Exp.Sizeof {typ}, _)], _, _)
        when Typ.Procname.equal pn BuiltinDecl.__instanceof && typ_is_throwable typ ->
          throwable_found := true
      | _ ->
          ()
    in
    let do_node n =
      if Location.equal loc (Procdesc.Node.get_loc n) then
        Instrs.iter ~f:do_instr (Procdesc.Node.get_instrs n)
    in
    Procdesc.iter_nodes do_node pdesc ; !throwable_found
  in
  let from_try_with_resources () : bool =
    (* heuristic to check if the condition is the translation of try-with-resources *)
    match Printer.LineReader.from_loc linereader loc with
    | Some line ->
        (not (String.is_substring ~substring:"==" line || String.is_substring ~substring:"!=" line))
        && String.is_substring ~substring:"}" line
        && contains_instanceof_throwable curr_pdesc node
    | None ->
        false
  in
  let is_temp = Idenv.exp_is_temp idenv e in
  let should_report =
    (not (InferredNullability.is_nullable inferred_nullability))
    && Config.eradicate_condition_redundant && true_branch && (not is_temp)
    && PatternMatch.type_is_class typ
    && (not (from_try_with_resources ()))
    && equal_from_call from_call From_condition
    && not (InferredNullability.origin_is_fun_library inferred_nullability)
  in
  let is_always_true = not case_zero in
  if should_report then
    report_error tenv find_canonical_duplicate
      (TypeErr.Condition_redundant (is_always_true, explain_expr tenv node e))
      (Some instr_ref) loc curr_pdesc


(** Check an "is zero" condition. *)
let check_zero tenv find_canonical_duplicate = check_condition tenv true find_canonical_duplicate

(** Check an "is not zero" condition. *)
let check_nonzero tenv find_canonical_duplicate =
  check_condition tenv false find_canonical_duplicate


(** Check an assignment to a field. *)
let check_field_assignment tenv find_canonical_duplicate curr_pdesc node instr_ref typestate
    exp_lhs exp_rhs typ loc fname field_type_opt typecheck_expr : unit =
  let curr_pname = Procdesc.get_proc_name curr_pdesc in
  let curr_pattrs = Procdesc.get_attributes curr_pdesc in
  let t_lhs, inferred_nullability_lhs, _ =
    typecheck_expr node instr_ref curr_pdesc typestate exp_lhs
      (typ, InferredNullability.create ~is_nullable:false TypeOrigin.ONone, [loc])
      loc
  in
  let _, inferred_nullability_rhs, _ =
    typecheck_expr node instr_ref curr_pdesc typestate exp_rhs
      (typ, InferredNullability.create ~is_nullable:false TypeOrigin.ONone, [loc])
      loc
  in
  let field_is_injector_readwrite () =
    match field_type_opt with
    | Some {annotation_deprecated} ->
        Annotations.ia_is_field_injector_readwrite annotation_deprecated
    | _ ->
        false
  in
  let field_is_in_cleanup_context () =
    let AnnotatedSignature.{ret_annotation_deprecated} =
      (Models.get_modelled_annotated_signature curr_pattrs).ret
    in
    Annotations.ia_is_cleanup ret_annotation_deprecated
  in
  let should_report_nullable =
    (not
       (NullsafeRules.passes_assignment_rule_for_inferred_nullability ~lhs:inferred_nullability_lhs
          ~rhs:inferred_nullability_rhs))
    && (not (AndroidFramework.is_destroy_method curr_pname))
    && PatternMatch.type_is_class t_lhs
    && (not (Typ.Fieldname.Java.is_outer_instance fname))
    && (not (field_is_injector_readwrite ()))
    && not (field_is_in_cleanup_context ())
  in
  if should_report_nullable then
    let origin_descr = InferredNullability.descr_origin inferred_nullability_rhs in
    report_error tenv find_canonical_duplicate
      (TypeErr.Field_annotation_inconsistent (fname, origin_descr))
      (Some instr_ref) loc curr_pdesc


let is_nullable {annotated_type} =
  match annotated_type.nullability with
  | AnnotatedNullability.Nullable _ ->
      true
  | AnnotatedNullability.Nonnull _ ->
      false


let is_nonnull {annotated_type} =
  match annotated_type.nullability with
  | AnnotatedNullability.Nullable _ ->
      false
  | AnnotatedNullability.Nonnull _ ->
      true


(* Do we have evidence that the field is annotated as nullable? *)
let is_field_annotated_as_nullable annotated_field_opt =
  (* If the field is not present, we optimistically assume it is not nullable.
    TODO(T54687014) investigate if this leads to unsoundness issues in practice
  *)
  Option.exists annotated_field_opt ~f:is_nullable


(* Do we have evidence that the field is annotated as non-nullable? *)
let is_field_annotated_as_nonnull annotated_field_opt =
  (* If the field is not present, we optimistically assume it is not nullable.
    TODO(T54687014) investigate if this leads to unsoundness issues in practice
  *)
  Option.exists annotated_field_opt ~f:is_nonnull


(** Check that nonnullable fields are initialized in constructors. *)
let check_constructor_initialization tenv find_canonical_duplicate curr_pname curr_pdesc start_node
    final_initializer_typestates final_constructor_typestates loc : unit =
  State.set_node start_node ;
  if Typ.Procname.is_constructor curr_pname then
    match PatternMatch.get_this_type (Procdesc.get_attributes curr_pdesc) with
    | Some {desc= Tptr (({desc= Tstruct name} as ts), _)} -> (
      match Tenv.lookup tenv name with
      | Some {fields} ->
          let do_field (fn, ft, _) =
            let annotated_field = get_field_annotation tenv fn ts in
            let is_injector_readonly_annotated =
              match annotated_field with
              | None ->
                  false
              | Some {annotation_deprecated} ->
                  Annotations.ia_is_field_injector_readonly annotation_deprecated
            in
            let final_type_annotation_with unknown list f =
              let filter_range_opt = function Some (_, ta, _) -> f ta | None -> unknown in
              List.exists
                ~f:(function
                  | pname, typestate ->
                      let pvar =
                        Pvar.mk (Mangled.from_string (Typ.Fieldname.to_string fn)) pname
                      in
                      filter_range_opt (TypeState.lookup_pvar pvar typestate) )
                list
            in
            let may_be_assigned_in_final_typestate =
              let origin_is_initialized = function
                | TypeOrigin.Undef ->
                    false
                | TypeOrigin.Field (TypeOrigin.Formal name, _, _) ->
                    let circular = Mangled.is_this name in
                    not circular
                | _ ->
                    true
              in
              final_type_annotation_with false (Lazy.force final_initializer_typestates)
                (fun nullability ->
                  origin_is_initialized (InferredNullability.get_origin nullability) )
            in
            let may_be_nullable_in_final_typestate () =
              final_type_annotation_with true (Lazy.force final_constructor_typestates) (fun ta ->
                  InferredNullability.is_nullable ta )
            in
            let should_check_field_initialization =
              let in_current_class =
                let fld_cname = Typ.Fieldname.Java.get_class fn in
                String.equal (Typ.Name.name name) fld_cname
              in
              (not is_injector_readonly_annotated)
              && PatternMatch.type_is_class ft && in_current_class
              && not (Typ.Fieldname.Java.is_outer_instance fn)
            in
            if should_check_field_initialization then (
              (* Check if non-null field is not initialized. *)
              if
                is_field_annotated_as_nonnull annotated_field
                && not may_be_assigned_in_final_typestate
              then
                report_error tenv find_canonical_duplicate
                  (TypeErr.Field_not_initialized (fn, curr_pname))
                  None loc curr_pdesc ;
              (* Check if field is over-annotated. *)
              if
                Config.eradicate_field_over_annotated
                && is_field_annotated_as_nullable annotated_field
                && not (may_be_nullable_in_final_typestate ())
              then
                report_error tenv find_canonical_duplicate
                  (TypeErr.Field_over_annotated (fn, curr_pname))
                  None loc curr_pdesc )
          in
          List.iter ~f:do_field fields
      | None ->
          () )
    | _ ->
        ()


let check_return_not_nullable tenv find_canonical_duplicate loc curr_pname curr_pdesc
    (ret_signature : AnnotatedSignature.ret_signature) ret_inferred_nullability =
  if
    not
      (NullsafeRules.passes_assignment_rule_for_annotated_nullability
         ~lhs:ret_signature.ret_annotated_type.nullability ~rhs:ret_inferred_nullability)
  then
    report_error tenv find_canonical_duplicate
      (TypeErr.Return_annotation_inconsistent
         (curr_pname, InferredNullability.descr_origin ret_inferred_nullability))
      None loc curr_pdesc


(* TODO(T54308240) Consolidate return over annotated checks in NullsafeRules *)
let check_return_overrannotated tenv find_canonical_duplicate loc curr_pname curr_pdesc
    (ret_signature : AnnotatedSignature.ret_signature) ret_inferred_nullability =
  (* TODO(T54308240) this needs to be changed when we introduce Unknown nullability *)
  let is_ret_inferred_nonnull = not (InferredNullability.is_nullable ret_inferred_nullability) in
  let is_ret_annotated_nullable =
    match ret_signature.ret_annotated_type.nullability with
    | AnnotatedNullability.Nonnull _ ->
        false
    | AnnotatedNullability.Nullable _ ->
        true
  in
  if is_ret_inferred_nonnull && is_ret_annotated_nullable then
    report_error tenv find_canonical_duplicate (TypeErr.Return_over_annotated curr_pname) None loc
      curr_pdesc


(** Check the annotations when returning from a method. *)
let check_return_annotation tenv find_canonical_duplicate curr_pdesc ret_range
    (annotated_signature : AnnotatedSignature.t) ret_implicitly_nullable loc : unit =
  let curr_pname = Procdesc.get_proc_name curr_pdesc in
  match ret_range with
  (* Disables the warnings since it is not clear how to annotate the return value of lambdas *)
  | Some _
    when match curr_pname with
         | Typ.Procname.Java java_pname ->
             Typ.Procname.Java.is_lambda java_pname
         | _ ->
             false ->
      ()
  | Some (_, ret_inferred_nullability, _) ->
      (* TODO(T54308240) Model ret_implicitly_nullable in AnnotatedNullability *)
      if not ret_implicitly_nullable then
        check_return_not_nullable tenv find_canonical_duplicate loc curr_pname curr_pdesc
          annotated_signature.ret ret_inferred_nullability ;
      if Config.eradicate_return_over_annotated then
        check_return_overrannotated tenv find_canonical_duplicate loc curr_pname curr_pdesc
          annotated_signature.ret ret_inferred_nullability
  | None ->
      ()


(** Check the receiver of a virtual call. *)
let check_call_receiver tenv find_canonical_duplicate curr_pdesc node typestate call_params
    callee_pname (instr_ref : TypeErr.InstrRef.t) loc typecheck_expr : unit =
  match call_params with
  | ((original_this_e, this_e), typ) :: _ ->
      let _, this_inferred_nullability, _ =
        typecheck_expr tenv node instr_ref curr_pdesc typestate this_e
          (typ, InferredNullability.create ~is_nullable:false TypeOrigin.ONone, [])
          loc
      in
      let null_method_call = InferredNullability.is_nullable this_inferred_nullability in
      if null_method_call then
        let descr = explain_expr tenv node original_this_e in
        let origin_descr = InferredNullability.descr_origin this_inferred_nullability in
        report_error tenv find_canonical_duplicate
          (TypeErr.Call_receiver_annotation_inconsistent (descr, callee_pname, origin_descr))
          (Some instr_ref) loc curr_pdesc
  | [] ->
      ()


type resolved_param =
  { num: int
  ; formal: AnnotatedSignature.param_signature
  ; actual: Exp.t * InferredNullability.t
  ; is_formal_propagates_nullable: bool }

(** Check the parameters of a call. *)
let check_call_parameters tenv find_canonical_duplicate curr_pdesc node callee_attributes
    resolved_params loc instr_ref : unit =
  let callee_pname = callee_attributes.ProcAttributes.proc_name in
  let check {num= param_num; formal; actual= orig_e2, nullability_actual} =
    let report () =
      let description =
        match explain_expr tenv node orig_e2 with
        | Some descr ->
            descr
        | None ->
            "formal parameter " ^ Mangled.to_string formal.mangled
      in
      let origin_descr = InferredNullability.descr_origin nullability_actual in
      let callee_loc = callee_attributes.ProcAttributes.loc in
      report_error tenv find_canonical_duplicate
        (TypeErr.Parameter_annotation_inconsistent
           (description, param_num, callee_pname, callee_loc, origin_descr))
        (Some instr_ref) loc curr_pdesc
    in
    if PatternMatch.type_is_class formal.param_annotated_type.typ then
      if
        not
          (NullsafeRules.passes_assignment_rule_for_annotated_nullability
             ~lhs:formal.param_annotated_type.nullability ~rhs:nullability_actual)
      then report ()
  in
  let should_check_parameters =
    match callee_pname with
    | Typ.Procname.Java java_pname ->
        (* TODO(T52947663) model is_external as unknown nullability and move the logic out of this check  *)
        (* If method is external, we don't check it, unless it is explicitly modelled *)
        (not (Typ.Procname.Java.is_external java_pname))
        || Models.is_modelled_for_nullability callee_pname
    | _ ->
        false
  in
  if should_check_parameters then
    (* left to right to avoid guessing the different lengths *)
    List.iter ~f:check resolved_params


(** Checks if the annotations are consistent with the inherited class or with the
    implemented interfaces *)
let check_overridden_annotations find_canonical_duplicate tenv proc_name proc_desc
    annotated_signature =
  let start_node = Procdesc.get_start_node proc_desc in
  let loc = Procdesc.Node.get_loc start_node in
  let check_return overriden_proc_name overriden_signature =
    let ret_is_nullable =
      Annotations.ia_is_nullable
        annotated_signature.AnnotatedSignature.ret.ret_annotation_deprecated
    and ret_overridden_nullable =
      Annotations.ia_is_nullable
        overriden_signature.AnnotatedSignature.ret.ret_annotation_deprecated
    in
    if ret_is_nullable && not ret_overridden_nullable then
      report_error tenv find_canonical_duplicate
        (TypeErr.Inconsistent_subclass_return_annotation (proc_name, overriden_proc_name))
        None loc proc_desc
  and check_params overriden_proc_name overriden_signature =
    let compare pos
        AnnotatedSignature.{mangled= current_name; param_annotation_deprecated= current_ia}
        AnnotatedSignature.{param_annotation_deprecated= overriden_ia} =
      let () =
        if (not (Annotations.ia_is_nullable current_ia)) && Annotations.ia_is_nullable overriden_ia
        then
          report_error tenv find_canonical_duplicate
            (TypeErr.Inconsistent_subclass_parameter_annotation
               (Mangled.to_string current_name, pos, proc_name, overriden_proc_name))
            None loc proc_desc
      in
      pos + 1
    in
    (* TODO (#5280249): investigate why argument lists can be of different length *)
    let current_params = annotated_signature.AnnotatedSignature.params
    and overridden_params = overriden_signature.AnnotatedSignature.params in
    let initial_pos = if is_virtual current_params then 0 else 1 in
    if Int.equal (List.length current_params) (List.length overridden_params) then
      ignore (List.fold2_exn ~f:compare ~init:initial_pos current_params overridden_params)
  in
  let check overriden_proc_name =
    match PatternMatch.lookup_attributes tenv overriden_proc_name with
    | Some attributes -> (
        let overridden_signature = Models.get_modelled_annotated_signature attributes in
        check_params overriden_proc_name overridden_signature ;
        (* the analysis should not report return type inconsistencies with external code *)
        match overriden_proc_name with
        | Typ.Procname.Java java_pname when not (Typ.Procname.Java.is_external java_pname) ->
            check_return overriden_proc_name overridden_signature
        | _ ->
            () )
    | None ->
        ()
  in
  PatternMatch.override_iter check tenv proc_name
