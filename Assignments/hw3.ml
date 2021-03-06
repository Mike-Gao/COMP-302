(* Question 1. *)

let common_tests = [
  ([1; 3; 2; 4; 1; 5; 6; 3], [3; 9; 8; 2; 11; 21; 3]),[3; 2];
  ([], []),[];
  ([2], [3]),[];
]

let rec memberof pair =
  match pair with
  | (x,[]) -> false
  | (x,y::ys) -> if x = y then true else memberof(x,ys)
          
let rec common twolists =
  match twolists with 
  | ([], []) -> []
  | (x, []) -> []
  | ([], y) -> []
  | (x::xs, ys) -> if memberof(x,ys) then x::common(xs,remove(x,ys))
      else common(xs,ys)
  
(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)
let split_tests = [
  [1; 3; 2; 4; 5; 6; 9; 11; 17; 13; 12], ([1; 2; 5; 9; 17; 12], [3; 4; 6; 11; 13]);
  [1;2], ([1],[2]); 
]

let rec split l =
  match l with
  | [] -> ([],[])
  | x::[] -> ([x],[])
  | e1::e2::elems -> let (l1,l2)= split elems in 
      (e1::l1,e2::l2)
  

(* Question 3 Here you implement merge. *)

let merge_tests = [
  ([1; 3; 5; 7; 9],[2; 4; 6; 8]), [1; 2; 3; 4; 5; 6; 7; 8; 9];
  ([1],[1]),[1;1];
  ([6],[5]),[5;6];
  ([],[]),[];
  ([3],[]),[3];
]

let rec merge twolists =
  match twolists with
  | ([], []) -> []
  | (x, []) -> x
  | ([], y) -> y
  | (x::xs,y::ys) -> if x < y then x::(merge(xs,y::ys))
      else y::merge(x::xs,ys)

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  [1; 3; 2; 4; 1; 5; 6; 3],[1; 1; 2; 3; 3; 4; 5; 6];
  [1],[1];
  [],[];
]

let rec mergesort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | xs -> let (l1, l2) = split xs in merge(mergesort(l1),mergesort(l2))
