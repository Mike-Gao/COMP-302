let rec insert(n, l) =
  match l with
  | [] ->  [n]
  | x::xs ->
     if (n < x)
     then
       n::(x::xs)
     else
       x::(insert(n,xs));;

insert (3,[1;2;4;5]);;

let rec isort l =
  match l with
  | [] -> []
  | x::xs -> insert(x, isort(xs));;
           
isort [8;2;5;1;3;2;6;9;7];;

