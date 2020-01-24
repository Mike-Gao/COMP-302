(* Question 1. *)

let common_tests = [
  ([1; 3; 2; 4; 1; 5; 6; 3], [3; 9; 8; 2; 11; 21; 3]),[3; 2];
  ([], []),[];
  ([2], [3]),[];
]

let rec common twolists =
  match twolists with 
  | ([],[]) -> []

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)
let split_tests = []

let rec split l =
  raise NotImplemented
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  ([1; 3; 5; 7; 9],[2; 4; 6; 8]), [1; 2; 3; 4; 5; 6; 7; 8; 9];
  ([1],[1]),[1;1];
  ([6],[5]),[5;6];
  ([],[]),[];
  ([3],[]),[3];
]

let rec merge twolists =
  raise NotImplemented
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  [1; 3; 2; 4; 1; 5; 6; 3],[1; 1; 2; 3; 3; 4; 5; 6];
  [1],[1];
  [],[];
  
]

let rec mergesort l =
  raise NotImplemented
;;
