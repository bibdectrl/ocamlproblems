(* Write a function last : 'a list -> 'a option that returns the last element of a list. *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | x::xs -> last xs

(* Find the last but one (last and penultimate) elements of a list. *)

let rec last_two = function 
  | [] -> None
  | [x] -> None
  | [x;y;] -> Some x
  | x::xs -> last xs

(* Find the k'th element of a list. *)

let rec kth(list, k) =
  match (list, k = 1) with
    (x::xs, true) -> Some (x)
  | (x::xs, false) -> kth (xs, k-1)
  | ([], _) -> None

   
 (* Find the number of elements of a list. *)

let rec length = function
  | [] -> 0
  | x::xs -> 1 + length xs



let length_tail_rec(list) = 
  let rec aux x = function
    | [] -> x
    | _::xs -> aux (x+1) xs
  in    
  aux 0 list;;
 


(* Reverse a list. *)
(*not actually correct
let rec reverse = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> reverse (List.tl xs) @ [x]
*)

