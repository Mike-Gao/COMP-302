let deriv(f, dx) = fun x -> ((f(x +. dx) -. f(x))/.dx);;

let iterSum(f, lo, hi, inc) =
  let rec helper(x, result) =
    if (x > hi) then result
    else helper((inc x), (f x) +. result)
  in
  helper(lo,0.0);;

let integral(f,lo,hi,dx) =
  let delta x = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta);;

let r_sq x = x *. x;;

integral(r_sq,0.0,1.0,0.001);;

integral(sin,0.0, 3.14159, 0.001);;
