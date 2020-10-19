let rec mymap f l =
  match l with
  | [] -> []
  | item :: rest -> (f item) :: (mymap f rest);;

mymap (fun n -> n + 1) [1;2;3];;

let odd n = (n mod 2) = 1;;

let l1 = List.init 9 (fun n -> n * n);;
          
let rec myfilter test l =
  match l with
  | [] -> []
  | x :: xs -> if (test x) then x :: (myfilter test xs)
               else (myfilter test xs);;

let l2 = myfilter odd l1;;

(* map and filter are built in.  There is a special library (such libraries are called 
"modules" in OCaml.)  To use the built-in functions write List.map or List.filter.  Or you can write 
"open List" as I have done below and the List library functions will be available.  *)

open List;;

filter odd l1;;
map (fun n -> n * n * n) (init 10 (fun n -> n));;

(* The most useful function in the List module. *)

(* First I will write it myself *)
let rec my_fold_left (f : 'a -> 'b ->'a) (acc : 'a) (l : 'b list): 'a =
  match l with
  |  [] -> acc
  | x :: xs -> my_fold_left f (f acc x) xs

let sum lst =
  fold_left (+) 0 lst;;

let myconcat lstlst = fold_left (@) [] lstlst;;

let stringmash strlst = fold_left (fun (s1:string) -> fun (s2:string) -> s1 ^ s2) "" strlst;;

(* Almost equally useful: fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)

let rec my_fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (acc : 'b) : 'b =
  match l with
  |  [] -> acc
  | x :: xs -> f x (my_fold_right f xs acc);;

let newstringmash strlst = fold_right (fun s1 -> fun s2 -> s1 ^ s2) strlst "";;

(* We can write many things in terms of folds. *)
let len l = fold_left (fun a _ -> a + 1) 0 l;;
let rev l = fold_left (fun a x -> x :: a) [] l;;
let newmap f l = fold_right (fun x a -> (f x) :: a) l [];;
let newfilter f l = fold_right (fun x a -> if f x then x :: a else a) l [];;

                 
                                  
