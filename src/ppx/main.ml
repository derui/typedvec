open Ppxlib
open Ast_helper

exception Error of Location.t * string

let make_size_type ~loc list =
  let rec succ' typ = function [] -> typ | _ :: rest -> succ' [%expr Typedvec.Size.succ [%e typ]] rest in
  succ' [%expr Typedvec.Size.zero] list

(* Expand [%vec] extension to an expression what make vector. *)
let expand_vec ~loc = function
  | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> (
      match e with
      | { pexp_desc = Pexp_construct ({ txt = Lident "::"; _ }, _); _ } ->
          let list = Ast_util.tup_to_list e in
          let hd = List.hd list in
          let size = make_size_type ~loc list in
          let indexed_list = List.mapi (fun index v -> (Exp.constant (Const.int index), v)) list in
          let set_value =
            List.map
              (fun v ->
                let index = fst v and v_ = snd v in
                [%expr Typedvec.Algebra.Vec.set vec ~index:[%e index] ~v:[%e v_]])
              indexed_list
          in
          let set_value =
            match set_value with
            | []           -> failwith "No any element of list %vec given?"
            | base :: rest -> List.fold_left (fun memo i -> Exp.sequence memo i) base rest
          in

          (* Expand expr using metaquot. *)
          [%expr
            let vec = Typedvec.Algebra.Vec.make [%e size] [%e hd] in
            [%e set_value];
            vec]
      | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, _); _ } ->
          Location.raise_errorf ~loc "%%vec have to apply a list is contained least one element"
      | _ -> Location.raise_errorf ~loc "%%vec have to be applied with a list of any element." )
  | _ -> Location.raise_errorf ~loc "Invalid extension point"

let vec_extension =
  Extension.V3.declare "vec" Extension.Context.expression Ast_pattern.__ (fun ~ctxt payload ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      expand_vec ~loc payload)

(* Mapper to reconstruct for assertions that are change to some assertion method *)
let rule = Context_free.Rule.extension vec_extension

let () = Driver.register_transformation ~rules:[ rule ] "vec"
