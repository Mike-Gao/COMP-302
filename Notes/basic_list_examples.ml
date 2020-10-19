(* The basic list operations are "cons" written as :: which adds an element
to the front of a list and "append", written @ which combines two lists.
Lists in OCaml are homogenous and immutable.  List items are separated by semi-colons.
*)

let vowels = ['a';'e';'i';'o';'u']

let vowels2 = 'y' :: vowels

(* What if we want to add y at the end? vowels :: 'y' gives a type error. *)

let vowels3 = vowels @ ['y']
(* We had to make 'y' into a list (of length 1) by writing ['y']. *)

(* How do we take lists apart?  OCaml provides "destructors" to take a list apart.  
They are called "hd" and "tl" for "head" and "tail". *)
let liszt = [1; 2; 3; 4; 5]

let liszt2 = [11; 12; 13; 14; 15]

(* These are discouraged.  Use pattern-matching instead.  *)
let v = List.hd liszt

let t = List.tl liszt

let rec badzip (l1,l2) =
  if l1 = [] then l2
  else
    (List.hd(l1))::(badzip (l2,List.tl(l1)))

let foo = badzip(liszt,liszt2);;
    

(* The pattern matching version. *)

let rec zip (l1, l2) =
  match l1 with
  | [] -> l2
  | x:: xs -> x :: zip(l2, xs);;

zip([1;3;5],[2;4;6]);;

(* Our own append function. It is O(n), unlike cons which is O(1). *)
let rec myappend l1 l2 = 
  match l1 with
  | [] -> l2
  | x :: xs -> x :: (myappend xs l2);;


(* Reverse done naively.  This is O(n^2). *)
let rec reverse l = 
  (match l with    
  | [] -> []
  | x::xs -> (reverse xs) @ [x]);;


(* Better reverse using an accumulating parameter. This is O(n). *)
let rev l = 
  let rec helper(l,acc) = 
    match l with
    | [] -> acc
    | x :: xs -> helper(xs, x::acc)
  in
  helper(l,[]);;

(* Some functions in the List module *)
open List;;

let foo = init 20 (fun n -> n);;

concat [[1;2;3];[4;5;6];[7;8;9]];;

nth foo 7;;

(* A string to list function; not provided in OCaml. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


(*  Below are the results of running the above definitions and expressions through the interpreter.

val vowels : char list = ['a'; 'e'; 'i'; 'o'; 'u']
val vowels2 : char list = ['y'; 'a'; 'e'; 'i'; 'o'; 'u']
val vowels3 : char list = ['a'; 'e'; 'i'; 'o'; 'u'; 'y']
val liszt : int list = [1; 2; 3; 4; 5]
val liszt2 : int list = [11; 12; 13; 14; 15]
val v : int = 1
val t : int list = [2; 3; 4; 5]
val badzip : 'a list * 'a list -> 'a list = <fun>
val foo : int list = [1; 11; 2; 12; 3; 13; 4; 14; 5; 15]
val zip : 'a list * 'a list -> 'a list = <fun>
#   - : int list = [1; 2; 3; 4; 5; 6]
val myappend : 'a list -> 'a list -> 'a list = <fun>
val reverse : 'a list -> 'a list = <fun>
val rev : 'a list -> 'a list = <fun>
#   val foo : int list =
  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19]
#   - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
#   - : int = 7
val explode : string -> char list = <fun>

 *)


