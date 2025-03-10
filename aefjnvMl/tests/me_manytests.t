  $ ./me_runner.exe < manytests/do_not_type/001.ml
  Unbound value 'fac'

  $ ./me_runner.exe < manytests/do_not_type/002if.ml
  This expression has type bool but an expression was expected of type int

  $ ./me_runner.exe < manytests/do_not_type/003occurs.ml
  The type variable 'a occurs inside 'a -> 'b

  $ ./me_runner.exe < manytests/do_not_type/004let_poly.ml
  This expression has type int but an expression was expected of type bool

  $ ./me_runner.exe < manytests/do_not_type/015tuples.ml
  Only variables are allowed as left-side of 'let rec'

  $ ./me_runner.exe < manytests/typed/001fac.ml
  let rec fac n =
    (if (( <= ) n) 1
    then
      1
    else
      (( * ) n) (fac ((( - ) n) 1)))
  ;;
  
  let cc_ac0_main = let () = print_int (fac 4) in
      0
  ;;
  

  $ ./me_runner.exe < manytests/typed/002fac.ml
  let rec fac_cps n k =
    (if (( = ) n) 1
    then
      k 1
    else
      (fac_cps ((( - ) n) 1)) (((fun cc0_n cc1_k p -> cc1_k ((( * ) p) cc0_n)) n) k))
  ;;
  
  let cc_ac1_main = let () = print_int ((fac_cps 4) (fun cc_ac0_print_int -> cc_ac0_print_int)) in
      0
  ;;
  

  $ ./me_runner.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n =
    (if (( = ) n) 1
    then
      b
    else
      let n1 cc0_n =
        (( - ) cc0_n) 1 in
        let ab cc1_a cc2_b =
          (( + ) cc1_a) cc2_b in
          ((fib_acc b) ((ab b) a)) (n1 n))
  ;;
  
  let rec fib cc_ac0_n =
    (if (( < ) cc_ac0_n) 2
    then
      cc_ac0_n
    else
      (( + ) fib ((( - ) cc_ac0_n) 1)) (fib ((( - ) cc_ac0_n) 2)))
  ;;
  
  let cc_ac1_main = let () = print_int (((fib_acc 0) 1) 4) in
      let () = print_int (fib 4) in
        0
  ;;
  

  $ ./me_runner.exe < manytests/typed/004manyargs.ml
  let wrap f =
    (if (( = ) 1) 1
    then
      f
    else
      f)
  ;;
  
  let test3 c b a =
    let cc_ac0_a cc0_a =
      print_int cc0_a in
      let cc_ac1_b cc1_b =
        print_int cc1_b in
        let cc_ac2_c cc2_c =
          print_int cc2_c in
          0
  ;;
  
  let test10 j i h g cc_ac6_f e d cc_ac5_c cc_ac4_b cc_ac3_a =
    (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) cc_ac3_a) cc_ac4_b) cc_ac5_c) d) e) cc_ac6_f) g) h) i) j
  ;;
  
  let cc_ac7_main = let rez = ((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000 in
      let () = print_int rez in
        let temp2 = (((wrap test3) 1) 10) 100 in
          0
  ;;
  

  $ ./me_runner.exe < manytests/typed/005fix.ml
  This expression has type int but an expression was expected of type 'a -> 'b

  $ ./me_runner.exe < manytests/typed/006partial.ml
  let cc_ac1_foo b =
    (if b
    then
      (fun foo -> (( + ) foo) 2)
    else
      (fun cc_ac0_foo -> (( * ) cc_ac0_foo) 10))
  ;;
  
  let cc_ac2_foo x =
    (cc_ac1_foo true) ((cc_ac1_foo false) ((cc_ac1_foo true) ((cc_ac1_foo false) x)))
  ;;
  
  let cc_ac3_main = let () = print_int (cc_ac2_foo 11) in
      0
  ;;
  

  $ ./me_runner.exe < manytests/typed/006partial2.ml
  This expression has type ((int -> int -> int) -> int -> 'a) -> 'a but an expression was expected of type int

  $ ./me_runner.exe < manytests/typed/006partial3.ml
  let foo a =
    let () = print_int a in
      (fun b -> let () = print_int b in
        (fun c -> print_int c))
  ;;
  
  let cc_ac0_main = let () = ((foo 4) 8) 9 in
      0
  ;;
  

  $ ./me_runner.exe < manytests/typed/007order.ml
  This expression has type unit but an expression was expected of type int

  $ ./me_runner.exe < manytests/typed/008ascription.ml
  This expression has type int but an expression was expected of type int -> bool -> int

  $ ./me_runner.exe < manytests/typed/009let_poly.ml
  let temp = let f x =
      x in
      (f 1, f true)
  ;;
  

  $ ./me_runner.exe < manytests/typed/015tuples.ml
  This expression has type ('a -> 'b -> 'c) -> 'b -> 'c but an expression was expected of type 'a * 'a

  $ ./me_runner.exe < manytests/typed/016lists.ml
  let rec length xs =
    (if (( = ) []) xs
    then
      0
    else
      (if (( >= ) get_list_len xs) 2
      then
        let tl = (get_list_tail xs) 1 in
          let h = (get_by_idx xs) 0 in
            (( + ) 1) (length tl)
      else
        part_match_fail ()))
  ;;
  
  let length_tail = let rec helper cc_ac_acc cc_ac0_xs =
      (if (( = ) []) cc_ac0_xs
      then
        cc_ac_acc
      else
        (if (( >= ) get_list_len cc_ac0_xs) 2
        then
          let cc_ac2_tl = (get_list_tail cc_ac0_xs) 1 in
            let cc_ac1_h = (get_by_idx cc_ac0_xs) 0 in
              (helper ((( + ) cc_ac_acc) 1)) cc_ac2_tl
        else
          part_match_fail ())) in
      helper 0
  ;;
  
  let rec map f cc_ac3_xs =
    (if (( = ) []) cc_ac3_xs
    then
      []
    else
      (if (( = ) 1) (get_list_len cc_ac3_xs)
      then
        let a = (get_by_idx cc_ac3_xs) 0 in
          (f a :: [])
      else
        (if (( = ) 2) (get_list_len cc_ac3_xs)
        then
          let cc_ac4_a = (get_by_idx cc_ac3_xs) 0 in
            let b = (get_by_idx cc_ac3_xs) 1 in
              (f cc_ac4_a :: (f b :: []))
        else
          (if (( = ) 3) (get_list_len cc_ac3_xs)
          then
            let cc_ac5_a = (get_by_idx cc_ac3_xs) 0 in
              let cc_ac6_b = (get_by_idx cc_ac3_xs) 1 in
                let c = (get_by_idx cc_ac3_xs) 2 in
                  (f cc_ac5_a :: (f cc_ac6_b :: (f c :: [])))
          else
            (if (( >= ) get_list_len cc_ac3_xs) 5
            then
              let cc_ac10_tl = (get_list_tail cc_ac3_xs) 4 in
                let cc_ac7_a = (get_by_idx cc_ac3_xs) 0 in
                  let cc_ac8_b = (get_by_idx cc_ac3_xs) 1 in
                    let cc_ac9_c = (get_by_idx cc_ac3_xs) 2 in
                      let d = (get_by_idx cc_ac3_xs) 3 in
                        (f cc_ac7_a :: (f cc_ac8_b :: (f cc_ac9_c :: (f d :: (map f) cc_ac10_tl))))
            else
              part_match_fail ())))))
  ;;
  
  let rec append cc_ac11_xs ys =
    (if (( = ) []) cc_ac11_xs
    then
      ys
    else
      (if (( >= ) get_list_len cc_ac11_xs) 2
      then
        let cc_ac12_xs = (get_list_tail cc_ac11_xs) 1 in
          let x = (get_by_idx cc_ac11_xs) 0 in
            (x :: (append cc_ac12_xs) ys)
      else
        part_match_fail ()))
  ;;
  
  let concat = let rec cc_ac13_helper cc_ac14_xs =
      (if (( = ) []) cc_ac14_xs
      then
        []
      else
        (if (( >= ) get_list_len cc_ac14_xs) 2
        then
          let cc_ac16_tl = (get_list_tail cc_ac14_xs) 1 in
            let cc_ac15_h = (get_by_idx cc_ac14_xs) 0 in
              (append cc_ac15_h) (cc_ac13_helper cc_ac16_tl)
        else
          part_match_fail ())) in
      cc_ac13_helper
  ;;
  
  let rec iter cc_ac17_f cc_ac18_xs =
    (if (( = ) []) cc_ac18_xs
    then
      ()
    else
      (if (( >= ) get_list_len cc_ac18_xs) 2
      then
        let cc_ac20_tl = (get_list_tail cc_ac18_xs) 1 in
          let cc_ac19_h = (get_by_idx cc_ac18_xs) 0 in
            let () = cc_ac17_f cc_ac19_h in
              (iter cc_ac17_f) cc_ac20_tl
      else
        part_match_fail ()))
  ;;
  
  let rec cartesian cc_ac21_xs cc_ac22_ys =
    (if (( = ) []) cc_ac21_xs
    then
      []
    else
      (if (( >= ) get_list_len cc_ac21_xs) 2
      then
        let cc_ac24_tl = (get_list_tail cc_ac21_xs) 1 in
          let cc_ac23_h = (get_by_idx cc_ac21_xs) 0 in
            (append ((map ((fun cc0_cc_ac23_h cc_ac25_a -> (cc0_cc_ac23_h, cc_ac25_a)) cc_ac23_h)) cc_ac22_ys)) ((cartesian cc_ac24_tl) cc_ac22_ys)
      else
        part_match_fail ()))
  ;;
  
  let cc_ac26_main = let () = (iter print_int) (1 :: (2 :: (3 :: []))) in
      let () = print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: [])))))) in
        0
  ;;
  
