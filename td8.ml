type expr_rat =
  |Vide
  |Epsilon
  |Lettre of char
  |Concat of expr_rat * expr_rat
  |Plus of expr_rat * expr_rat
  |Etoile of expr_rat;;

type expr_lin = 
  |Empty
  |Epsi
  |Pos of int
  |Prod of expr_lin * expr_lin
  |Sum of expr_lin * expr_lin
  |Star of expr_lin;;

let e = Concat(Etoile(Plus(Lettre('a'), Concat(Lettre('a'), Lettre('b')))), Concat(Lettre('a'), Lettre('b')));;


let linearise (e : expr_rat) =
  let n = ref 0 in
  let chars = ref [] in
  let rec aux e = 
    match e with
    |Vide -> Empty
    |Epsilon -> Epsi
    |Lettre(l) -> chars := l::(!chars); n := !n+1; Pos(!n)
    |Concat(e1, e2) -> Prod(aux e1, aux e2)
    |Plus(e1, e2) -> Sum(aux e1, aux e2)
    |Etoile(ee) -> Star(aux ee);
  in let res = aux e
  in res,Array.of_list !chars;;

let le = fst (linearise e);;

let rec accepteMotVide (e : expr_lin) =
  match e with
  |Empty -> false
  |Epsi -> true
  |Pos(n) -> false
  |Prod(e1, e2) -> (accepteMotVide e1)&&(accepteMotVide e2)
  |Sum(e1, e2) -> (accepteMotVide e1)||(accepteMotVide e2)
  |Star(e1) -> true;;

accepteMotVide(le);;

let prefixes (e : expr_lin) = 
  let rec aux e =
    match e with
    |Empty -> []
    |Epsi -> []
    |Pos(n) -> [n]
    |Prod(e1, e2) -> if(accepteMotVide e1) then (aux e2)@(aux e1) else aux e1
    |Sum(e1, e2) -> (aux e2)@(aux e1)
    |Star(e1) -> aux e1;
  in aux e;;
le;;
prefixes le;;

let suffixes (e : expr_lin) =
  let rec aux e =
    match e with
    |Empty -> []
    |Epsi -> []
    |Pos(n) -> [n]
    |Prod(e1, e2) -> if(accepteMotVide e2) then (aux e2)@(aux e1) else aux e2
    |Sum(e1, e2) -> (aux e2)@(aux e1)
    |Star(e1) -> aux e1;
  in aux e;;

suffixes le;;

let rec prod e l =
  match l with
  |[] -> []
  |t::q -> (e, t)::(prod e q);;

let rec prod_carte l1 l2 = 
  match(l1, l2) with
  |([], []) -> []
  |([], _) -> []
  |(_, []) -> []
  |(t::q1, l2) -> (prod t l2)@(prod_carte q1 l2);;


let facteurs (e : expr_lin) =
  let rec aux e =
    match e with
    |Empty -> []
    |Epsi -> []
    |Pos(n) -> []
    |Prod(e1, e2) -> (aux e1)@(aux e2)@(prod_carte (suffixes e1) (prefixes e2))
    |Sum(e1, e2) -> (aux e1)@(aux e2)
    |Star(e1) -> (aux e1)@(prod_carte (suffixes e1) (prefixes e1))
  in aux e;;

facteurs le;;

let suivant (e : expr_lin) (i : int) =
  let fact = facteurs e in
  let rec aux l =
    match l with
    |[] -> []
    |t::q -> if (fst t = i) then (snd t)::(aux q) else aux q
  in aux fact;;

suivant le 2;;

type automate = {
  nb_etat : int;
  finaux : int list;
  transitions : ((char * int) list) array;
}
(*
let glushkov (e : expr_rat) =
    let le, chars = linearise e in
    let nb_etat = Array.length chars in
    let finaux = suffixes le in
    let p = ref [] in
    for i=0 to nb_etat do
        let s = suivant l2 (i+1) in
        let l = ref [] in
        for j=0 to (List.length s)-1 do
            l := (chars.(i), get_elem s j)::(!l)
        p := l::(!p) *)