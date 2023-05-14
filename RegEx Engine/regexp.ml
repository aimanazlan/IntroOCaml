open List
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  match regexp with 
  | Empty_String -> let reg = fresh() in 
    {
      q0 = reg;
      qs = [reg];
      sigma = [];
      fs = [reg];
      delta = [(reg, None, reg)];
    }

  | Star(exp) -> let first = regexp_to_nfa exp in
    let reg = fresh() in
    let f_reg = fresh() in
    {
      q0 = reg;
      qs = Sets.union first.qs [reg;f_reg];
      sigma = first.sigma;
      fs = [f_reg];
      delta = Sets.union first.delta [((List.hd first.fs), None, f_reg); (reg, None, first.q0); (reg, None, f_reg); (f_reg, None, reg)];
    }

  | Union (exp1, exp2) -> let f_q = fresh() in
    let s_q = fresh() in
    let left_nfa = regexp_to_nfa exp1 in
    let right_nfa = regexp_to_nfa exp2 in
    let d_temp = Sets.union left_nfa.delta right_nfa.delta in
    {
      q0 = f_q;
      qs = Sets.union left_nfa.qs (Sets.union right_nfa.qs [f_q; s_q]);
      sigma = Sets.union left_nfa.sigma right_nfa.sigma;
      fs = [s_q];
      delta = Sets.union d_temp [(f_q, None, left_nfa.q0); (f_q, None, right_nfa.q0); ((List.hd left_nfa.fs), None, s_q); ((List.hd right_nfa.fs), None, s_q)];
    }

  | Concat (exp1, exp2) -> let left_nfa = regexp_to_nfa exp1 in
    let right_nfa = regexp_to_nfa exp2 in
    {
      q0 = left_nfa.q0;
      qs = Sets.union left_nfa.qs right_nfa.qs;
      sigma = Sets.union left_nfa.sigma right_nfa.sigma;
      fs = right_nfa.fs;
      delta = Sets.union (Sets.union left_nfa.delta right_nfa.delta) [((List.hd left_nfa.fs), None, right_nfa.q0)];
    }

  | Char (exp) -> let reg = fresh() in
    let fresh_reg = fresh() in
    {
      q0 = reg;
      qs = [reg; fresh_reg];
      sigma = [exp];
      fs = [fresh_reg];
      delta = [(reg, Some exp, fresh_reg)];
    }

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
