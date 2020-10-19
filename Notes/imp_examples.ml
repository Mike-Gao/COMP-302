let x = ref 1;;

let mash (n:int) =
  let m = ref n in
  (Printf.printf "m is %i " !m);(m := !m + 1); (Printf.printf "n is %i " n); 
  (Printf.printf "m is %i" !m);;


 let foo n =
  let x = ref n in     
  while (!x < 10) do
    (Printf.printf "x is %i\n " !x);(x := !x + 1)
  done;;

type person = { name : string ; birthday : int * int; title : string };;

let prakash = { name = "Prakash"; birthday = (3,11); 
                title = "Bane of while loops"};;

