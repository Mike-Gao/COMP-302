(* Q1 Polynomials TODO: Implement the following four functions *)

let multiplyPolyByTerm (Term(c,e), Poly p) =
  let list = List.map (fun (f, i) -> (f *. c, e + i)) p in
  match List.filter (fun (c, _) -> c != 0.) list with
  | [] -> Poly [0., 0]
  | l -> Poly l
;;

let addTermToPoly (Term(c,e), Poly p) =
  let rec helper(list) =
    match list with
    |[] -> [(c,e)]
    |x::xs -> match x with
      |(c1,e2) -> if e = e2 then (c1 +. c, e2)::xs
          else if e > e2 then (c,e)::x::xs
          else x::(helper(xs))
  in
  if c = 0. then Poly p
  else Poly (helper(p))
;;

let rec addPolys (Poly p1, Poly p2) = 
  match p1 with 
  | [] -> Poly p2
  | (c,e)::xs -> addPolys( Poly xs,  addTermToPoly(Term(c,e), Poly p2))
;;


let multPolys (Poly p1, Poly p2) =
  let rec multPolyHelper(Poly p1, Poly p2, Poly acc) =
    match p1 with
    | [] -> Poly acc
    | (c,e)::xs -> let a = multiplyPolyByTerm( Term (c,e), Poly p2) 
        in
        multPolyHelper(Poly xs, Poly p2, addPolys(Poly acc, a))
  in
  multPolyHelper(Poly p1,Poly p2,Poly [(0.,0)])
;;

(* Q2 References TODO: implement the `insert` function *)

let rec insert comp (item: int) (list: rlist) =
  match !list with
  | None -> list := Some {data = item; next = ref None} 
  | Some {data = d; next = n} -> 
      if comp (item,d)
      then (list := Some {data = item; next = ref (Some {data = d; next = n})})
      else insert comp item n 
