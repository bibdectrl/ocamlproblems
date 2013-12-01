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

let rec reverse = function
  | [] -> []
  | x::xs -> reverse xs @ [x]

(* Find out whether a list is a palindrome *)

let is_palindrome a b =
  reverse a = b



(* There is no nested list type in OCaml, so we need to define one first. A node of a nested list is either an element, or a list of nodes. *)

type 'a node =
| One of 'a 
| Many of 'a node list;;

type 'a node = One of 'a | Many of 'a node list

(*
let rec flatten = function
  |
*)
