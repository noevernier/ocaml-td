type graphe1 = (int * int list) list;;
let (g : graphe1) = [1, [2;5]; 2, [1;3;5]; 3, [2;4]; 4, [3;5;6]; 5, [1;2;4]; 6, [4]; 7, [8]; 8, [7]];;

type graphe2 = {mutable sommets : int list; mutable arcs : (int * int) list};;

type graphe3 = int list array;;
type graphe4 = bool array array;;

let sommet_max (g : graphe2)=
  let rec aux l =
    match l with
    |[] -> failwith "Sommet Vide"
    |[e] -> e
    |t::q -> aux q
  in (aux g.sommets);;

let convertit12 (g : graphe1) =
  let rec get_sommets g = 
    match g with
    |[] -> []
    |t::q -> (fst t)::(get_sommets q);
  in
  let rec get_arcs g =
    match g with
    |[] -> []
    |t::q -> let rec aux p s =
               match s with
               |[] -> []
               |t::q -> (p, t)::(aux p q) in
      (aux (fst t) (snd t))@(get_arcs q);
  in {sommets = get_sommets g; arcs = get_arcs g};;

convertit12 g;;

let rec get_elem (l : 'a list) (i : int) =
  match (l, i) with
  |([], i) -> failwith "Liste Vide"
  |(t::q, i) -> if(i = 0) then t else (get_elem q (i-1));;

let convertit13 (g : graphe1) =
  let n = sommet_max (convertit12 g) in
  let graphe3 = Array.make (n+1) [] in
  for i=0 to (List.length g)-1 do
    let (s, a) = get_elem g i in
    graphe3.(s) <- a;
  done;(graphe3 : graphe3);;

convertit13 g;;

let convertit24 (g : graphe2) =
  let n = (sommet_max g) in
  let g3 = Array.make_matrix n n false in
  let rec aux a =
    match a with
    |[] -> ()
    |(i,j)::q -> g3.(i-1).(j-1) <- true; aux q;
  in
  aux g.arcs; g3;;

convertit24 (convertit12 g);;



let add_sommet1 (g : graphe1) (s : int) = g@[(s, [])];;
let add_sommet2 (g : graphe2) (s : int) = {sommets = g.sommets@[s]; arcs = g.arcs};;

let del_sommet (g : graphe3) (n : int) = g.(n) <- [];;
let add_vertex (g : graphe4) i j = g.(i).(j) <- true;;
let del_vertex (g : graphe4) i j = g.(i).(j) <- false;;

let profondeur (g : graphe3) (k) =
  let l = ref [k] in
  let visit = Array.make (Array.length g) false in
  visit.(k)<-true;
  let rec aux (g : graphe3) (i) = 
    let voisins = g.(i) in
    let n = List.length voisins in
    visit.(i) <- true;
    for k=0 to n-1 do
      let s = get_elem voisins k in
      if(visit.(s) = false)then( (l := !l@[s]);(aux g s);)
    done;
  in
  aux g k; !l;;

profondeur (convertit13 g) 1;;

(*
let largeur (g : graphe3) (s) = 
  let f = file_vide() in
  let visit = Array.make (Array.length g) false in
  visit.(s) <- true;
  enfile f s
    while !(is_empty(f))do
      let x = defile f in
      print_int x;
      let voisins = g.(x) in
      let n = List.length voisins in
      for k=0 to n-1 do   
        let z = get_elem voisins k in
        if(visit.(z) = false)then(
          visit.(z) <- true;
          enfile f z;
        )
      done;
    done;;

largeur (convertit13 g) 1;;*)

