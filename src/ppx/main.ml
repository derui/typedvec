open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Location

exception Error of Location.t * string

let () =
  Location.register_error_of_exn (fun exn ->
    match exn with
    | Error (loc, message) ->
       Some (error ~loc message)
    | _ -> None)

let make_size_type list =
  let rec succ' typ = function
    | [] -> typ
    | v :: rest -> succ' ([%expr Typedvec.Std.Size.succ [%e typ]]) rest
  in
  succ' [%expr Typedvec.Std.Size.zero] list

(* Expand [%vec] extension to an expression what make vector. *)
let expand_vec ~loc = function
  | PStr [{pstr_desc = Pstr_eval (e,_);_}] -> begin
    match e with
    | {pexp_desc = Pexp_construct ({txt = Lident "::";_}, _);_} ->
       let list = Ast_util.tup_to_list e in
       let hd = List.hd list in
       let size = make_size_type list in
       let indexed_list = List.mapi (fun index v ->
          (Exp.constant (Const_int (index)), v)
       ) list in
       let set_value = List.map (fun v ->
         let index = fst v
         and v_ = snd v in
         [%expr Typedvec.Std.Algebra.Vec.set vec ~index:[%e index] ~v:[%e v_];]) indexed_list in
       let set_value = match set_value with
         | [] -> failwith "No any element of list %vec given?"
         | base :: rest -> List.fold_left (fun memo i ->
           Exp.sequence memo i
         ) base rest
       in
            
       (* Expand expr using metaquot. *)
       Some [%expr
             let vec = Typedvec.Std.Algebra.Vec.make [%e size] [%e hd] in
             [%e set_value];
             vec
            ]
    | {pexp_desc = Pexp_construct ({txt = Lident "[]";_},_);_} ->
       failwith "%vec have to apply a list is contained least one element"
    | _ -> failwith "%vec have to be applied with a list of any element."
  end
  | _ -> None

(* Mapper to reconstruct for assertions that are change to some assertion method *)
let rec extension_mapper argv = {default_mapper with
  expr = fun mapper expr -> (
    match expr with
    | {pexp_desc = Pexp_extension ({txt="vec";loc}, payload);_} -> begin
      match expand_vec ~loc payload with
      | Some e -> e
      | None -> default_mapper.expr mapper expr
    end
    | _ -> default_mapper.expr mapper expr
  );
}

let () = run_main extension_mapper
