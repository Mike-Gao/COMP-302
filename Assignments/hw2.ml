(* Q1a TODO: Write your own tests for the pairlists function.
         You should NOT test lists of different lengths.
*)
let pairlists_tests = [
  (([], []), []);
  (([1;2;3],[1;2;3]), [(1, 1); (2, 2); (3, 3)]); 
  (([3;2;0],[1;2;3]), [(3, 1); (2, 2); (0, 3)]); 
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists twolists = 
  match twolists with 
  | ([],[]) -> []
  | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)
    
  

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)
let w_mean_tests = [ 
]

(* Q1b TODO: Implement w_mean. *)
let w_mean weights data =
  raise NotImplemented;;


(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  ((3,[3]),true);
  ((3,[]),false)
]

(* Q2 TODO: Implement memberof. *)
let rec memberof pair =
  match pair with
  | (x,[]) -> false
  | (x,y::ys) -> if x = y then true else memberof(x,ys)

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  ((3, [1; 6; 3; 2; 6; 1; 7; 2; 3; 5]),([1; 6; 2; 6; 1; 7; 2; 5]));
  ((0, [1; 6; 2; 6; 1; 7; 2; 5]),([1; 6; 2; 6; 1; 7; 2; 5]));
  ((0,[]),([]));
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) =
  match lst with
  |[] -> [] 
  |h::t -> if h = item then remove(item,t)
      else h :: (remove (item,t)) 

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  (([1;6;3;2;6;1;7;2;3;5]), 7);
  (([-1;0]), 0);
]

(* Q3 TODO: Implement find_max. *)
let find_max l =
  match l with 
  |[] -> failwith "None"
  |h::t -> 
      let rec helper (seen,rest) =
        match rest with 
        |[] -> seen
        |h'::t' -> let seen' = if h' > seen then h' else seen in 
            let rest' = t'
            in helper (seen',rest')
      in helper (h,t) 

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  ([1;2;3;4;5;6],[6;5;4;3;2;1]);
  ([],[]);
  ([6;-1;3;2],[6;3;2;-1])
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l =
  match l with
  | [] -> []
  | _ -> find_max(l) :: (selsort(remove(find_max(l), l)))