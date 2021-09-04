(************ Remise en Route ************)
(*EXERICE 1*)
let rec longueur (l : 'a list) =
  match l with
  |[] -> 0
  |t::q -> 1 + (longueur q);;
(*_______________________________________________*)
let rec est_membre (x : 'a) (l : 'a list)  =
  match l with
  |[] -> false
  |t::q -> if t = x then true else (est_membre x q);;
(*_______________________________________________*)
let rec minimum (l : 'a list) =
  match l with
  |[] -> failwith "Liste Vide"
  |[e] -> e
  |t::q -> min t (minimum q);;
(*_______________________________________________*)
let rec inserer (e : 'a) (l : 'a list) =
  match l with
  |[] -> e::[]
  |x::l -> if(e < x) then e::x::l else x::(inserer e l);;
(*_______________________________________________*)
let rec tri_insertion (l : 'a list) = 
  match l with
  |[] -> []
  |x::l -> inserer x (tri_insertion l);;
(*_______________________________________________*)
let rec separe (l : 'a list) =
  match l with
  |[] -> ([], [])
  |[e] -> ([e], [])
  |t1::t2::q -> let g = separe q in (t1::(fst g), t2::(snd g));;
(*_______________________________________________*)
let rec fusion (e : 'a list) (f : 'a list) =
  match (e,f) with
  |([],[]) -> []
  |(e, []) -> e
  |([], f) -> f
  |(t1::q1, t2::q2) -> if(t1 < t2) then t1::t2::(fusion q1 q2) else t2::t1::(fusion q1 q2);;
(*_______________________________________________*)
let rec tri_fusion (l : 'a list) =
  match l with
  |[] -> []
  |l -> let (l1, l2) = separe l in (fusion (tri_fusion l1) (tri_fusion l2));;
(*_______________________________________________*)   
let rec union (e : 'a list) (f : 'a list) =
  match (e,f) with
  |(e, []) -> e
  |([], f) -> f
  |(x::e, f) -> let g = union e f in if (est_membre x g) then g else x::g;;
(*_______________________________________________*)
let rec intersection (e : 'a list) (f : 'a list) =
  match (e, f) with
  |(_, []) -> []
  |([], _) -> []
  |(x::e, f) -> let g = intersection e f in if (est_membre x f) then x::g else g;;
(*_______________________________________________*)
let miroir l =
  let rec aux a l =
    match l with
    |[] -> a
    |x::l -> (aux (x::a) l);
  in aux [] l;;
(*_______________________________________________*)
let rec accolle (l1 : 'a list) (l2 : 'a list) =
  match (l1, l2) with
  |(l1, []) -> l1
  |([], l2) -> l2
  |(x::l1, l2) -> x::(accolle l1 l2);;
(*_______________________________________________*)
let rec parties l =
  match l with
  |[] -> [[]]
  |[e] -> [[]; [e]]
  |x::l -> let g = parties l in (g)@(List.map (fun t -> x::t) g);;
(*_______________________________________________*)

(*EXERICE 2*)
type 'a arbre =
  |Nil
  |N of ('a arbre * 'a * 'a arbre);;
(*_______________________________________________*)
let rec hauteur (a : 'a arbre) =
  match a with
  |Nil -> 0
  |N(g, _, d) -> 1 + max (hauteur g) (hauteur d);;
(*_______________________________________________*)
let rec feuilles (a : 'a arbre) =
  match a with
  |Nil -> 0
  |N(Nil, _, Nil) -> 1
  |N(g, _, d) -> (feuilles g) + (feuilles d);;
(*_______________________________________________*)
let rec bornes (a : 'a arbre) =
  match a with
  |Nil -> failwith "Arbre Vide"
  |N(Nil, a, Nil) -> (a, a)
  |N(g, a, d) -> 
    let b1 = bornes g in
    let b2 = bornes d in
    let max_b = max (fst b1) (fst b2) in
    let min_b = max (snd b1) (snd b2) in
    (max a max_b, min a min_b);;
(*_______________________________________________*)
let rec desequilibre (a : 'a arbre) =
    match a with
    |Nil -> Nil
    |N(g, _, d) -> N(g, (hauteur g) - (hauteur d), d);;
(*_______________________________________________*)