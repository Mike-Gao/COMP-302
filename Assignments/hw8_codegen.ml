let tempstore = ref 0;;
  
type exptree = Var of char | Expr of char * exptree * exptree;;

let codegen (e: exptree) = 
  let rec helper((e: exptree), (tag: char)) =
    match e with
      | Var c ->
        if (tag = '=') then
          Printf.printf "LOAD  %c\n" c
        else
          if (tag = '+')
          then
            Printf.printf "ADD  %c\n" c
          else (* tag = '*' *)
            Printf.printf "MUL  %c\n" c
      | Expr(op,l,r) -> 
         if (tag = '=') then
           (helper (l,'=');
           helper (r, op))
         else
           begin
             tempstore := !tempstore + 1;
             Printf.printf "STORE %i\n" !tempstore;
             helper(l,'=');
             helper(r,op);
             (if (tag = '+')
             then
               Printf.printf "ADD %i\n" !tempstore
             else
               Printf.printf "MUL %i\n" !tempstore);
             tempstore := !tempstore - 1
           end
  in
  helper(e,'=');;

(*
         
let exptree = Expr ('+',Var 'a',Expr ('*',Var 'b',Expr ('+',Var 'c',Var 'd')));;
# codegen exptree;;
LOAD  a
STORE 1
LOAD  b
STORE 2
LOAD  c
ADD  d
MUL 2
ADD 1
- : unit = ()

 *)
