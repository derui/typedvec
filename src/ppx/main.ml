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

(* Expand [%vec] extension to an expression what make vector. *)
let expand_vec ~loc = function
  | PStr [{pstr_desc = Pstr_eval (e,_);_}] -> begin
    match e with
    | {pexp_desc = Pexp_construct ({txt = Lident "::";_}, _);_} ->
       let list = Ast_util.tup_to_list e in
       let tupled_list = Ast_util.list list in
       let hd = List.hd list in
       let len = Exp.constant (Const_int (List.length list)) in
       Some [%expr
             let module S = (val Typedvec.Std.Size.of_int [%e len]) in
                 let v = Typedvec.Std.Vec.make S.size [%e hd] in
                 List.iteri (fun index v -> Typedvec.Std.Vec.set v ~index ~v) [%e tupled_list];
                 v
            ]
    | {pexp_desc = Pexp_construct ({txt = Lident "[]";_},_);_} ->
       failwith "%vec have to apply a list is contained least one element"
    | _ -> failwith "%vec have to be applied with a list of any element."
  end
  | _ -> None

(* Convert attributes to program to generate to make vector or matrix. *)
let convert_extensions ~loc vbs =
  List.fold_left (fun memo vb ->
    match vb with
    | {pvb_expr = {pexp_desc = Pexp_extension ({txt="vec";loc}, payload);_};_} -> begin
      match expand_vec ~loc payload with
      | Some e -> {vb with pvb_expr = e} :: memo
      | None -> memo
    end
    | _ -> memo
  ) [] vbs |> List.rev

(* Mapper to reconstruct for assertions that are change to some assertion method *)
let rec assertion_mapper argv = {default_mapper with
  expr = fun mapper expr ->
    match expr with
    | {pexp_desc = Pexp_let (rec_flag, vbs, e);pexp_loc = loc;pexp_attributes = attrs} ->
       let expanded = convert_extensions ~loc vbs in
       Exp.let_ ~loc ~attrs rec_flag expanded e
    | _ -> default_mapper.expr mapper expr
}

let () = run_main assertion_mapper
