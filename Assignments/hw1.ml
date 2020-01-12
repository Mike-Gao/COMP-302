(* Q1 TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let double int = match int with
  | 0 -> 0
  | n -> 2 * n


(* Q1 TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
  (0, 1.0);
  (1, 1.0);
  (3, 6.0);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> float_of_int(n * int_of_float(fact(n - 1)));;

(* Q2 TODO: Write your own tests for the mysqrt function.
         You should NOT test cases for n < 0.
*)
let mysqrt_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (9.0, 3.0);
]

(* Q2 TODO: Implement mysqrt. *)
let rec mysqrthelper (base, x) = 
  if close(square(base),x) then base 
  else mysqrthelper ((x /. base +. base) /. 2.0, x) 
let mysqrt (x:float) = mysqrthelper(x /. 3.0, x)


(* Q3 TODO: Write your own tests for the cube_root function.
            You should NOT test cases for n < 0.
*)
let cube_root_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (8.0, 2.0);
]

(* Q3 TODO: Implement cube_root. *)
let rec mycuberoothelper (base, x) = 
  if close(cube(base),x) then base 
  else mycuberoothelper ((x /. base /. base +. base +. base) /. 3.0, x) 
let cube_root (x:float) = mycuberoothelper(x /. 3.0, x)

(* Q4 TODO: Write your own tests for the fast_exp function.
            You should NOT test cases for negative bases or powers.
*)
let fast_exp_tests = [
  (* Your test cases go here. *)
  (0, 0), 0;
  (1, 0), 1;
  (3, 2), 9;
]

(* Q4 TODO: Implement tail recursive helper fast_exp_aux. *)
let rec fast_exp_aux (base, power, acc) = 
  if base = 0 then base
  else if power = 0 then acc
  else fast_exp_aux (base, power - 1, acc * base)

(* Q4 TODO: Implement fast_exp using fast_exp_aux. *)
let fast_exp (base, power) = fast_exp_aux (base, power, 1)
                                         

                           

