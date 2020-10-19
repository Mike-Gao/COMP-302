  OCaml version 4.07.1

Findlib has been successfully loaded. Additional directives:
  #require "package";;      to load a package
  #list;;                   to list the available packages
  #camlp4o;;                to load camlp4 (standard syntax)
  #camlp4r;;                to load camlp4 (revised syntax)
  #predicates "p,q,...";;   to set these predicates
  Topfind.reset();;         to force that packages will be reloaded
  #thread;;                 to enable threads

# type lterm = 
  |  Num of int 
  |  Var of string  
  |  Apply of lterm *lterm 
  |  Plus of lterm *lterm 
  |  Lambda of string * lterm 
  |  Let of (( string * lterm) list) *lterm;;
            type lterm =
    Num of int
  | Var of string
  | Apply of lterm * lterm
  | Plus of lterm * lterm
  | Lambda of string * lterm
  | Let of (string * lterm) list * lterm
# type results  = 
  | Int of int  
  | Closure of (lterm * envs)
  | Unbound
and 
  layer = Layer of (lterm * results) list
and
  envs = Env of layer list;;
              type results = Int of int | Closure of (lterm * envs) | Unbound
and layer = Layer of (lterm * results) list
and envs = Env of layer list
# let first_part (x,y) = x;;
val first_part : 'a * 'b -> 'a = <fun>
# let sec_part (x,y) = y;;
val sec_part : 'a * 'b -> 'b = <fun>
# let toplayer env =
  match env with
    | Env (top :: rest) -> top
    | _ -> failwith "Not a proper environment";;
      val toplayer : envs -> layer = <fun>
      
# let firstpair layer =
  match layer with
    | Layer (first:: rest) -> first
    | _ -> failwith "Improper layer";;
      val firstpair : layer -> lterm * results = <fun>

# let rec bound_in (layer, var) =
  match var with
    | Var name -> 
        (match layer with
          | [] ->  (false, Unbound) 
          | (x,y)::rest -> 
            if x = (Var name) then (true, y)
            else bound_in (rest,var) )
    | _ -> failwith "Searching for a non-variable";;
                val bound_in : (lterm * results) list * lterm -> bool *
  results = <fun>
  
# let rec binding (bare_env, var) =
  match var with
    | Var name -> 
        (match bare_env with
          | []  -> Unbound
          | x :: rest -> 
               let (p,q) = search_layer(x, var) in
               if p then q else binding(rest, var) )
    | _ -> failwith "Searching for a non-variable";;
                val binding : layer list * lterm -> results = <fun>
# let newenv (x, Env n) =  Env ((Layer x)::n);;
val newenv : (lterm * results) list * envs -> envs = <fun>
(* The following functions build closures and take them apart. *)

# let is_closure arg =
  match arg with
    | Closure _ -> true 
    | _ -> false;;
      val is_closure : results -> bool = <fun>
# let lambda_body (Lambda (x,y)) = y;;
Characters 16-34:
  let lambda_body (Lambda (x,y)) = y;;
                  ^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Num _|Var _|Apply (_, _)|Plus (_, _)|Let (_, _))
val lambda_body : lterm -> lterm = <fun>
# let boundvar (Lambda (x,y))   = x;;
Characters 13-33:
  let boundvar (Lambda (x,y))   = x;;
               ^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Num _|Var _|Apply (_, _)|Plus (_, _)|Let (_, _))
val boundvar : lterm -> string = <fun>
# let get_body (Closure (exp, env))= lambda_body exp;;
Characters 13-50:
  let get_body (Closure (exp, env))= lambda_body exp;;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Unbound|Int _)
val get_body : results -> lterm = <fun>
# let get_var (Closure (exp, env))= Var (boundvar exp);;
Characters 12-52:
  let get_var (Closure (exp, env))= Var (boundvar exp);;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Unbound|Int _)
val get_var : results -> lterm = <fun>
# let get_env (Closure (exp, env)) = env;;
Characters 12-38:
  let get_env (Closure (exp, env)) = env;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Unbound|Int _)
val get_env : results -> envs = <fun>
# let make_closure (Lambda (n,exp), Env e) = Closure (Lambda (n,exp), Env  e);;
Characters 17-75:
  let make_closure (Lambda (n,exp), Env e) = Closure (Lambda (n,exp), Env  e);;
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
((Num _|Var _|Apply (_, _)|Plus (_, _)|Let (_, _)), Env _)
val make_closure : lterm * envs -> results = <fun>
# let myadd (Int x, Int  y) = Int (x+y);;
Characters 10-37:
  let myadd (Int x, Int  y) = Int (x+y);;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
((Int _, Unbound)|(Unbound, _))
val myadd : results * results -> results = <fun>
# let rec make_new_layer (list_of_pairs, env) =
  match list_of_pairs with
    | []  -> []
    | (name, expr)::rest -> 
        (Var name, eval(expr, env))::make_new_layer(rest, env) 
and apply (closure, arg) = eval((get_body(closure)),
                                (newenv([((get_var closure), arg)],
                                        (get_env  closure))))
and eval (term, env) = 
  match term with
    | Var name ->  search_env (env, Var name)
    | Num y ->  Int y 
    | Plus (exp1,exp2) ->  myadd(eval (exp1,env),
                                 eval(exp2,env)) 
    | Lambda (param, body) -> make_closure (Lambda (param, body) , env)
    | Apply (func, arg) -> apply(eval(func, env), eval(arg, env))
    | Let (binding_list, let_body) ->
        eval (let_body, 
              newenv((make_new_layer(binding_list, env),env)));;
                                    val make_new_layer : (string * lterm) list * envs -> (lterm * results) list =
  <fun>
val apply : results * results -> results = <fun>
val eval : lterm * envs -> results = <fun>
# let p = Let ( [("b", Num 2);("d",Num 4)], Apply(Lambda ("c", Plus(Plus(Var "c", Var "b"), Var "d")), Num 3));;
val p : lterm =
  Let ([("b", Num 2); ("d", Num 4)],
   Apply (Lambda ("c", Plus (Plus (Var "c", Var "b"), Var "d")), Num 3))
# let fresh = Env [ (Layer []) ];;
val fresh : envs = Env [Layer []]
# eval (p, fresh);;
- : results = Int 9
# let q = Let ( [("b", Num 2)], Apply(Lambda ("c",Plus(Var "c", Var
"b")),Num 3));;
  val q : lterm =
  Let ([("b", Num 2)], Apply (Lambda ("c", Plus (Var "c", Var "b")), Num 3))
# let x = Lambda ("y",Plus (Var "y", Var "y"));;
val x : lterm = Lambda ("y", Plus (Var "y", Var "y"))
# let z = Apply (x, Num 2);;
val z : lterm = Apply (Lambda ("y", Plus (Var "y", Var "y")), Num 2)
# let l1 = [(Var "a",Int 2);(Var "b",Int 3)];;
val l1 : (lterm * results) list = [(Var "a", Int 2); (Var "b", Int 3)]
# let l2 = [(Var "c", Int 3);(Var "d", Int 4)];;
val l2 : (lterm * results) list = [(Var "c", Int 3); (Var "d", Int 4)]
# let e1 =Env ( [Layer l1;Layer l2]);;
val e1 : envs =
  Env
   [Layer [(Var "a", Int 2); (Var "b", Int 3)];
    Layer [(Var "c", Int 3); (Var "d", Int 4)]]
# eval (z, e1);;
- : results = Int 4
# let f = Apply(Lambda ("x", Var "x"), Lambda ("u", Var "u"));;
val f : lterm = Apply (Lambda ("x", Var "x"), Lambda ("u", Var "u"))
# eval (f, fresh);;
- : results = Closure (Lambda ("u", Var "u"), Env [Layer []])
# let k1 = Let ([("x", Num 1)], Let ([("f", Lambda ("u", Plus(Var "u", Var "x")))], Let ([("x", Num 2)], Apply(Var "f", Var "x"))));;
val k1 : lterm =
  Let ([("x", Num 1)],
   Let ([("f", Lambda ("u", Plus (Var "u", Var "x")))],
    Let ([("x", Num 2)], Apply (Var "f", Var "x"))))
# eval (k1, fresh);;
- : results = Int 3
# let k2 = Apply (Lambda ("x", Var "x"), (Lambda ("y", Plus (Var "y", Num 1))));;
val k2 : lterm =
  Apply (Lambda ("x", Var "x"), Lambda ("y", Plus (Var "y", Num 1)))
# eval (k2, fresh);;
- : results = Closure (Lambda ("y", Plus (Var "y", Num 1)), Env [Layer []])
# let k3 = Apply (Lambda ("x", Plus (Var "x", Var "a")), Var "c");;
val k3 : lterm = Apply (Lambda ("x", Plus (Var "x", Var "a")), Var "c")
# eval (k3, e1);;
- : results = Int 5
# let d1 = Apply(Lambda ("f", Lambda ("x", Apply (Var "f", Var "x"))),
               Lambda ("n", Plus(Var "n", Num 1)));;
  val d1 : lterm =
  Apply (Lambda ("f", Lambda ("x", Apply (Var "f", Var "x"))),
   Lambda ("n", Plus (Var "n", Num 1)))
# eval(d1, fresh);;
- : results =
Closure
 (Lambda ("x", Apply (Var "f", Var "x")),
  Env
   [Layer
     [(Var "f",
       Closure (Lambda ("n", Plus (Var "n", Num 1)), Env [Layer []]))];
    Layer []])
# let d2 = Apply(d1, Num 3);;
val d2 : lterm =
  Apply
   (Apply (Lambda ("f", Lambda ("x", Apply (Var "f", Var "x"))),
     Lambda ("n", Plus (Var "n", Num 1))),
   Num 3)
# eval (d2, fresh);;
- : results = Int 4
# 