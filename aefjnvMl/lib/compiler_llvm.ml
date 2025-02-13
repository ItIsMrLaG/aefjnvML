(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast

(* let rec fuc a = if a = 1 then a else a * fuc (a - 1) *)
let todo =
  { vb_pat = Pat_var "fuc"
  ; vb_expr =
      Exp_function
        ( Pat_var "a"
        , Exp_ifthenelse
            ( Exp_apply
                (Exp_apply (Exp_ident "=", Exp_ident "a"), Exp_constant (Const_int 1))
            , Exp_ident "a"
            , Exp_apply
                ( Exp_apply (Exp_ident "*", Exp_ident "a")
                , Exp_apply
                    ( Exp_ident "fuc"
                    , Exp_apply
                        ( Exp_apply (Exp_ident "-", Exp_ident "a")
                        , Exp_constant (Const_int 1) ) ) ) ) )
  }
;;
