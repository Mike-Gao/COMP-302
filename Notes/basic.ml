let inc = fun n -> n + 1;;

let foo = inc 5;;

let rec fact n =
  if n = 0 then
    1
  else
    n * fact(n-1);;

fact 5;;

let fastfact n =
  let rec helper(n,m) =
    if n = 0 then m
    else helper(n-1, n * m)
  in
  helper(n,1);;

fastfact 20;;

let even n = (n mod 2) = 0;;
let odd n = (n mod 2) = 1;;

(* Russian peasant exponentiation. *)

let rec rpe base power =
  if base = 0 then 0
  else
    if power = 0 then 1
    else
      if (odd power) then
        base * (rpe base (power - 1))
      else
        let tmp = (rpe base (power/2)) in
        tmp * tmp;;
        
(* Fibonacci *)

let rec fib n =
  if n = 0 then 0
  else
    if n = 1 then 1
    else (fib (n - 1)) + (fib (n - 2));;

let tailfib n =
  let rec helper (n,a,b) =
    if n = 0 then a
    else
      if n = 1 then b
      else helper (n-1, b, a+b)
  in
  helper (n, 0, 1);;

let rec sumnums lo hi =
  if lo > hi then 0
  else lo + (sumnums (lo + 1) hi);;

let tailsum lo hi =
  let rec helper lo hi tally =
    if lo > hi then tally
    else helper (lo + 1) hi (lo + tally)
  in
  helper lo hi 0;;

let rec sum2 (lo, hi) =
  if lo > hi then 0
  else lo + sum2(lo+1, hi);;

let silly str =
  if str = "Prakash" then str ^ " is awesome."
  else if str = "Prof." then str ^ " Prakash, is incorrect."
  else "Who cares?";;

let less_silly str =
  match str with
  | "Prakash" -> str ^ " is awesome."
  | "prof." -> str ^ " Prakash, is incorrect."
  | _ -> "Who cares?";;

let myadd(n,m) =
  match (n,m) with
  | (0,_) -> m
  | (_,0) -> n
  | _ -> n + m;;


           
    

                           
