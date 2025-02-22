(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* ======= binary ops ======= *)

(* int operators *)
val int_ty_op : Typedtree.ty
val op_mul : string
val op_div : string
val op_plus : string
val op_minus : string

(* comparison operators *)
val bool_ty_op : Typedtree.ty
val op_less_eq : string
val op_more_eq : string
val op_less : string
val op_more : string
val op_eq : string
val op_2eq : string
val op_not_eq : string

(* bool operators *)
val comp_ty_op : int -> Typedtree.ty
val op_and : string
val op_or : string

(* ======= unary ops ======= *)
val sig_un_op_minus : Typedtree.ty
val un_op_minus : string
val sig_un_op_not : Typedtree.ty
val un_op_not : string

(** Example: [let (~-) a = a + 1;;] *)
val un_op_prefix : string

val sig_func_print_int : Typedtree.ty
val func_print_int : string

(* ALL *)
val std_lib_names : string list
val converte_infix : string -> string
val is_binop : string -> bool

(* INTERN LIB *)

open Typedtree

(**  *)
val ty_get_by_idx : binder -> binder -> ty

val func_get_by_idx : string

(**  *)
val ty_get_list_len_plus_one : binder -> ty

val func_get_list_len_plus_one : string

(**  *)
val ty_fail_pt_match : binder -> ty

val func_fail_pt_match : string
