open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts x string to x character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)

let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt

let rec fold_right f xs a = match xs with
| [] -> a
| x :: xt -> f x (fold_right f xt a)

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  List.fold_left (fun accum tuple -> 
    match tuple with 
    | (x,y,z) -> if (y = s && List.mem x qs) &&
    ((List.mem z accum) = false) then z::accum else accum) [] nfa.delta

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec clos_help nfa qs lst = 
    match qs with 
    | [] -> lst
    | h::t -> 
      if (List.mem h lst) then (clos_help nfa t lst)
      else clos_help nfa (union t (move nfa [h] None)) (insert h lst)
  in clos_help nfa qs []

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let rec acc_help nfa current lst =
    match lst with
    |[] -> current
    |h::t -> acc_help nfa (e_closure nfa (move nfa current (Some h))) t
  in let curr_accept = acc_help nfa (e_closure nfa [nfa.q0]) (explode s) in
    let res = intersection curr_accept nfa.fs in if res = [] then false else true

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  map (fun a -> e_closure nfa (move nfa qs (Some a))) nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold_left (fun a b -> union [(qs, Some b, e_closure nfa (move nfa qs (Some b)))] a) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let res = intersection qs nfa.fs in if res = [] then [] else [qs]

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with 
  | [] -> dfa
  | h::t -> let new_dfa = 
  {
    sigma = dfa.sigma;
    qs = insert h dfa.qs;
    q0 = dfa.q0;
    fs = union (new_finals nfa h) dfa.fs;
    delta = union dfa.delta (new_trans nfa h);
  }
  in nfa_to_dfa_step nfa new_dfa (diff (union (new_states nfa h) t) new_dfa.qs)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let new_dfa = 
  { 
  sigma = nfa.sigma; 
  qs = [(e_closure nfa [nfa.q0])]; 
  q0 = (e_closure nfa [nfa.q0]); 
  fs = []; 
  delta = [];
  } 
  in nfa_to_dfa_step nfa new_dfa [e_closure nfa [nfa.q0]]
