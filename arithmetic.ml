(* gcd *)

let rec gcd x y = 
  if y = 0
  then x
  else
    gcd y (x mod y)

let coprime x y =
  gcd x y = 1


let rec factors x =
  
