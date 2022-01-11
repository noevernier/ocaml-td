type afd = {
  initial : int;
  delta : int -> char -> int;
  est_final : int -> bool;
};;

(*Question 1*)
let delta_star n c =
  match (n, c) with
  |(1, 'a') -> 1
  |(1, 'b') -> -1
  |(_, _) -> -1;;

let est_final_star n = n=1;;

let star = {initial = 1; delta = delta_star; est_final = est_final_star};;


(*Question 2*)
let delta_epsilon n c =
  match (n, c) with
  |(1,' ') -> 1
  |(_, _) -> -1;;

let est_final_epsilon n = n=1;;

let epsilon = {initial = 1; delta = delta_epsilon; est_final = est_final_epsilon};;

(*Question 3*)
let lang_vide = {initial = -1; delta = (fun n c -> -1); est_final = (fun n -> true)};;

(*Question 4*)
let estReconnu a s =
  let n = String.length s in
  let state = ref a.initial in
  for i=0 to n-1 do
    state := (a.delta !state s.[i]);
  done;
  a.est_final !state;;

estReconnu star "aabaa";;

(*Question 5*)

let delta_5 n c =
  match (n, c) with
  |(1, 'a') -> 2
  |(2, 'a') -> 3
  |(3, 'a') -> 3
  |(3, 'b') -> 4
  |(4, 'a') -> 3
  |(_, _) -> -1;;

let a_5 = {initial = 1; delta = delta_5; est_final = fun n -> n=4};;

estReconnu a_5 "aabababaaaab";;
estReconnu a_5 "aababaaaaa";;


(*Question 6*)
let int2bin n =
  let rec aux p = 
    match p with
    |0 -> [0]
    |p -> (p mod 2)::(aux (p/2));
  in let l = aux n in
  let rec list_to_string t = 
    match t with
    |[] -> ""
    |e::q -> (string_of_int e)^(list_to_string q);
  in list_to_string l;;

int2bin 36;;

let delta_6 n c =
  match (n, c) with
  |(0, '0') -> 0
  |(0, '1') -> 1
  |(1, '0') -> 2
  |(1, '1') -> 0
  |(2, '0') -> 1
  |(2, '1') -> 2
  |(_, _) -> -1;;

let a_6 = {initial = 0; delta = delta_6; est_final = fun n -> n=0};;

estReconnu a_6 (int2bin 21);;

let divisiblePar3 n = estReconnu a_6 (int2bin n);;
(*_________Partie 2___________*)
(*Exercice 7*)
let rec insere (e : 'a) (l : 'a list) =
  match l with
  |[] -> e::[]
  |x::l -> if(e < x) then e::x::l else x::(insere e l);;

let rec egal l p =
  match (l, p) with
  |([], []) -> true
  |([], _) -> false
  |(_, []) -> false
  |(t1::q1, t2::q2) -> (t1=t2)&&(egal q1 q2);;

let rec est_dans a l = 
  match l with
  |[] -> false
  |t::q -> (a=t)||(est_dans a q);;

type afnd = {
  etats : int list;
  initiaux : int list;
  finaux : int list;
  transitions : (int * char * int) list;
};;

let star_nd = {
  etats = [1];
  initiaux = [1]; 
  finaux = [1]; 
  transitions = [(1,'a',1)];
};;
let epsilon_nd = {
  etats = [1]; 
  initiaux = [1]; 
  finaux = [1]; 
  transitions = [(1,' ',1)];
};;

(*Question 9*)
let states (a : afnd) = a.etats;;
(*Question 10*)
let alphabet (a : afnd) =
  let alph = ref [] in
  let rec aux l =
    match l with
    |[] -> ()
    |(_, c, _)::q ->  if(not(est_dans c !alph))then alph:= c::(!alph); aux q;
  in aux (a.transitions); alph;;

alphabet star_nd;;
