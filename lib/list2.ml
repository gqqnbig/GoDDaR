open Types

(* Tail recursive map *)
let map f l =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] l

(* Tail recursive rev *)
let rev l =
  let rec rev l r = match l with
      | [] -> r
      | (h::t) -> rev t (h::r)
  in
  rev l []

(* Tail recursive array append *)
let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

(* Tail recursive flatten *)
let flatten2 ll =
  let rec go acc = function
    | [] -> rev acc
    | l :: r -> go (List.rev_append l acc) r
  in
    go [] ll

(* Enumerates the elements of a list by transforming them into pairs *)
let rec enumerate list i =
  match list with
  | [] -> []
  | hd::tl -> (i, hd)::(enumerate tl (i+1))

(* Finds the index of elem in list *)
let rec find list elem =
  match list with
  | [] -> -1 (* Not found *)
  | hd::tl -> if hd = elem then 0 else 1 + find tl elem 

(* Removes the first occurrence of elem in list *)
let rec filter_first list elem = 
  match list with
  | [] -> []
  | hd::tl ->
    if hd != elem then
      hd::(filter_first tl elem)
    else
      tl

(* Substitutes the first element in a list equal to replaced with replacer *)
let rec subst_first list replaced replacer =
  match list with
  | [] -> []
  | hd::tl ->
    if hd = replaced then
      replacer::tl
    else
      hd::(subst_first tl replaced replacer)

let rec subst_at list replacer curr_i i_at =
  match list with
  | [] -> []
  | hd::tl ->
    if curr_i = i_at then
      replacer::tl
    else
      hd::(subst_at tl replacer (curr_i+1) i_at)