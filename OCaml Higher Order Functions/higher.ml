open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a b -> a || b = e) false lst

let is_present lst x = map (fun a -> if a = x then 1 else 0) lst

let count_occ lst target = fold (fun count elem -> if elem = target then count + 1 else count) 0 lst

let uniq lst = fold (fun a b -> if (contains_elem a b) = false then b::a else a) [] lst

let assoc_list lst = let new_list = uniq lst in fold (fun a b -> (b, count_occ lst b)::a) [] new_list

let ap fns args = fold (fun a b -> a @ (map b args)) [] fns
