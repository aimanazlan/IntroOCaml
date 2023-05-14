open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_mem x t =
  match t with
    | IntLeaf -> false
    | IntNode ( value, option , tree_one , tree_two, tree_three)-> 
      if value = x || (match option with None -> false | Some q -> q = x) 
      then true else int_mem x tree_one || int_mem x tree_two || int_mem x tree_three


let rec int_insert x t = 
  if (int_mem x t) = true then t 
  else match t with
  | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (value, None, tree_one, tree_two, tree_three)-> if x > value 
      then IntNode (value, Some x, tree_one, tree_two, tree_three) else IntNode (x, Some value, tree_one, tree_two, tree_three)
  | IntNode (value, Some option, tree_one, tree_two, tree_three) -> if x < value 
      then IntNode (value, Some option, int_insert x tree_one, tree_two, tree_three)
      else if x > option then IntNode (value, Some option, tree_one, tree_two, int_insert x tree_three)
      else IntNode (value, Some option, tree_one, int_insert x tree_two, tree_three)

let rec int_size t =
  match t with 
  | IntLeaf -> 0
  | IntNode(value, option, tree_one, tree_two, tree_three) ->
    if option = None then 1 else 2 + int_size tree_one + int_size tree_two + int_size tree_three

let convert_int x = 
  match x with
  | None -> failwith "Unable to convert to int"
  | Some i -> i

let rec int_max t =
  match t with 
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode (value, option, tree_one, tree_two, tree_three) -> if option = None 
      then value 
      else if tree_three = IntLeaf then convert_int option 
      else int_max tree_three

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_contains k t = 
  match t with
  | MapLeaf -> false
  | MapNode ((key, value), option, tree_one, tree_two, tree_three) -> if k = key || match option with
    | None -> false
    | Some (key_some, value_some) -> k = key_some then true else map_contains k tree_one || map_contains k tree_two || map_contains k tree_three

let rec map_put k v t = 
  if ((map_contains k t) = true) then raise(Invalid_argument("map_put")) 
  else match t with
    | MapLeaf -> MapNode ((k,v), None, MapLeaf,MapLeaf,MapLeaf)
    | MapNode ((key,value), option, tree_one, tree_two, tree_three) -> match option with
      | None -> if k < key then MapNode ((k, v), Some (key, value), tree_one, tree_two, tree_three) else MapNode ((key,value),Some (k,v), tree_one, tree_two, tree_three)
	    | Some (key_some, value_some) -> if k < key then MapNode ((key, value),Some (key_some, value_some), map_put k v tree_one, tree_two, tree_three)
          else if k > key_some then MapNode ((key, value), Some(key_some, value_some), tree_one, tree_two, map_put k v tree_three)
          else MapNode ((key, value), Some(key_some, value_some), tree_one, map_put k v tree_two, tree_three)


let rec map_get k t =
  match t with 
  | MapLeaf -> raise(Invalid_argument("map_get"))
  | MapNode ((key, value), None, tree_one, tree_two, tree_three) -> if key = k then value else raise(Invalid_argument("map_get"))
  | MapNode ((key, value), Some (key_some, value_some), tree_one, tree_two, tree_three) -> if key = k then value 
    else if key_some = k then value_some 
    else if k < key then map_get k tree_one 
    else if k > key_some then map_get k tree_three
    else map_get k  tree_two
    

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (string  * int) list list

let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = []::table

let pop_scope (table : lookup_table) : lookup_table =
  match table with
  | [] -> failwith "No scopes remain!"
  | _::t -> t

let rec dupli lst name value =
  match lst with 
  | [] -> false
  | h::t -> let (curr_name, curr_val) = h in if curr_name = name then true 
    else if curr_val = value 
    then true else (dupli t name value)

let add_var name value (table : lookup_table) : lookup_table =
  match table with 
  | [] -> failwith "There are no scopes to add a variable to!"
  | h::t -> if (dupli h name value) then failwith "Duplicate variable binding in scope!" 
    else match h with
    | [] -> [(name, value)]::t
    | a::b -> ((name, value)::a::b)::t  

let rec lookup name (table : lookup_table) =
  match table with
  | [] -> failwith "Variable not found!"
  | h::t -> let rec help lst n = 
    match lst with 
    | [] -> failwith "Variable not found!"
    | h::t -> let (n, var) = h in if n = name then var else help t name in help h name
  