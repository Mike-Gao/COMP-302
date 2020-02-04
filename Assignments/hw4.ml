(* Question 1 *)
let t1 = Node(Empty, 0, (Node(Empty, 1, Empty)));;
let t2 = Node(Node(Empty,5,Empty),6,Empty);;
let t3 = Node(Node(t2,2,Node(Empty,3,Empty)),4,t1);;
let mapTree_tests =
  [
    ((fun x -> x + 1), t3), 
    Node ( Node ( Node ( Node ( Empty, 6, Empty), 7, Empty) , 3, 
                  Node (Empty, 4, Empty)), 5, Node (Empty, 1, Node (Empty, 2, Empty))
         ); 
    ((fun x -> x + 1), Empty), Empty;
  ]

let rec mapTree (f, (t: 'a tree)) = 
  match t with
  | Empty -> Empty
  | Node(l,n,r) -> Node(mapTree(f,l),f n,mapTree(f,r))
(* Question 2. *)

let halfint_tests =
  [
    (sin, 3.0, -2.0, 0.1),0.03125;

  ]

let rec halfint ((f: float -> float), (posValue : float), (negValue : float), (epsilon : float)) =
  let mid = ( posValue +. negValue ) /. 2.0 in
  if abs_float (f mid) < epsilon then mid
  else if (f mid) < 0.0 then halfint (f, posValue, mid, epsilon)
  else halfint (f, mid, negValue, epsilon) 

(* Question 3. *)
let make_cubic((a:float),(b:float),(c:float)) =
  fun x -> (x*.x*.x +. a *. x*.x +. b*.x +. c)
let newton_tests =
  [
    ((sin,5.0,0.0001,0.0001),9.42477); 
  ]

let rec newton ((f: float -> float),  (guess:float), (epsilon:float), (dx:float)) =
  let close((x:float), (y:float), (epsilon:float)) = abs_float(x-.y) < epsilon in 
  let improve((guess:float),f,(dx:float)) = (guess -. (f guess)/. (deriv(f,dx) guess)) in
  if close((f guess), 0.0, epsilon)
  then
    guess
  else 
    newton(f, improve(guess, f, dx), epsilon , dx ) 

(* Question 4. *)

let integralhelper (f,lo,hi,dx) =
  let delta x = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta);;

let indIntegral (f, (dx:float)) =
  fun x -> integralhelper (f,0.0,x,dx) 
  
      


