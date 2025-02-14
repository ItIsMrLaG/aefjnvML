(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base.Result

let ( let* ) = ( >>= )

let alpha_test s =
  let ast'_t =
    let* ast = Parser.parse s in
    let* _ = Inferencer.check_program ast in
    let* ast' = Alpha_converter.rename_ast_with_uniq ast in
    let* _ = Inferencer.check_program ast' in
    Ok ast'
  in
  match ast'_t with
  | Ok ast' -> Stdlib.Format.printf "%a\n" Common.Ast_pp.program_pp ast'
  | Error err ->
    (match err with
     | Parser e -> Parser.PP.pp_error Format.std_formatter e
     | Infer e -> Inferencer.PP.pp_error Format.std_formatter e
     | Alpha_converter (Illegal_state_error s) -> Format.print_string s)
;;

let%expect_test "" =
  let () = alpha_test {|
   let a = 1;;
   let b = 1;;
   let c = a + b;;
  |} in
  [%expect {|
    let a = 1;;
    let b = 1;;
    let c = ((((+)) a) b);; |}]
;;

let%expect_test "" =
  let () =
    alpha_test
      {|
   let a = 1;;
   let b = a;;
   let c = 
   let a = b in
   a
   ;;
  |}
  in
  [%expect {|
    let a = 1;;
    let b = a;;
    let c = let ac0_a = b in ac0_a;; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
   let rt_a = 1
   let ll_b = rt_a
   let main = 1
   ;;
  |} in
  [%expect {|
    let ac_rt_a = 1;;
    let ac_ll_b = ac_rt_a;;
    let ac0_main = 1;; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
   let rec (+) b c = c + b
   ;;
  |} in
  [%expect {|
    let rec op_plus = fun b -> fun c -> ((op_plus c) b);; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
   let (+) b c = c + b
   let (+) b c = c + b
   ;;
  |} in
  [%expect
    {|
    let op_plus = fun b -> fun c -> ((((+)) c) b);;
    let ac2_op_plus = fun ac0_b -> fun ac1_c -> ((op_plus ac1_c) ac0_b);; |}]
;;

let%expect_test "" =
  let () =
    alpha_test
      {|
   let rec even n =
   match n with
      | 0 -> true
      | x -> odd (x-1)
   and odd n =
   match n with
      | 0 -> false
      | x -> even (x-1);;
  |}
  in
  [%expect
    {|
    let rec even = fun n -> match n with
    | 0 -> true
    | x -> (odd ((((-)) x) 1))
    and odd = fun ac0_n -> match ac0_n with
    | 0 -> false
    | ac1_x -> (even ((((-)) ac1_x) 1));; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
   let a = 10;;
   let a, b = 1, 2;;
  |} in
  [%expect {|
    let a = 10;;
    let (ac0_a, b) = (1, 2);; |}]
;;
