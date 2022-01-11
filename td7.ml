(*---------------EXERCICE 1---------------*)
type 'a file = {mutable debut : 'a list; mutable fin : 'a list}

let file_vide () = {debut = []; fin = []};;

let est_vide (f : 'a file) = (f.debut = [])&&(f.fin = []);;

(*O(n)*)
let list_rev l =
  let rec aux a l =
    match l with
    |[] -> a
    |x::l -> (aux (x::a) l);
  in aux [] l;;

(*O(n)*)
let premier (f : 'a file) =
  match f with
  |{debut = []; fin = []} -> failwith "File Vide";
  |{debut = []; fin = _} -> f.debut <- (list_rev f.fin); List.hd f.debut;
  |_ -> List.hd f.debut;;

let enfile (x : 'a) (f : 'a file) = {debut = f.debut; fin = x::(f.fin)};;

(*O(n)*)
let defile (f : 'a file) = 
  match f with
  |{debut = []; fin = []} -> {debut = []; fin = []};
  |{debut = []; fin = _} -> f.debut <- (list_rev f.fin); {debut = (List.tl f.debut); fin = []};
  |_ -> {debut = (List.tl f.debut); fin = []};;

let f = {debut = []; fin = [6;4;3]};;

defile f;;

(*---------------EXERCICE 2---------------*)
let size () = 10000;;
type 'a filec ={donnees : 'a array; 
                est_vide : bool; 
                mutable debut : int; 
                mutable suivant : int};;

let filec_vide x = {donnees = Array.make (size()) x; est_vide = true; debut = 0; suivant = 0};;

let est_vide (f : 'a filec) = f.est_vide;;

let premier (f : 'a filec) = 
  match f with
  |{donnees = _; est_vide = true; debut = _; suivant = _} -> failwith "File Vide"
  |_ -> f.donnees.(f.debut);;

let enfile (x : 'a) (f : 'a filec) = ((f.donnees).(f.suivant) <- x); {donnees = f.donnees; 
                                                                      est_vide = false; 
                                                                      debut = f.debut; 
                                                                      suivant = (f.suivant +1)mod(size())};;

let defile (f : 'a filec) =  {donnees = f.donnees; 
                              est_vide = false; 
                              debut = (f.debut+1)mod(size()); 
                              suivant = f.suivant};;

(*---------------EXERCICE 3---------------*)
type tas = Vide | N of int*tas*tas;;

let min_tas (t : tas) =
  match t with
  |Vide -> failwith "Tas Vide"
  |N(x,_,_) -> x;;

let rec fusion (t1 : tas) (t2 : tas) =
  match (t1, t2) with
  |(Vide, t2) -> t2
  |(t1, Vide) -> t1
  |(N(x1, g1, d1), N(x2, g2, d2)) ->  if(x1 < x2)then N(x1, N(x2, g2, d2), fusion d1 d2) else N(x2, fusion g1 g2, N(x1, g1, d1));;

let enleve_min (t : tas) =
  match t with
  |Vide -> failwith "tas Vide"
  |N(x, g, d) -> fusion g d;;

let rec ajouter (t : tas) (e : int) =
  match t with
  |Vide -> N(e, Vide, Vide)
  |N(x, g, d) when(x>e)-> N(e, d, ajouter g x)
  |N(x, g, d) -> N(x, ajouter d e, g);;

let rec ajouter_liste (l : int list) =
  match l with
  |[] -> Vide
  |t::q -> ajouter (ajouter_liste q) t;;

let rec vider (t : tas) =
  match t with
  |Vide -> []
  |N(x, g, d) -> x::(vider (enleve_min t));;

let tri_par_tas (l : int list) = vider (ajouter_liste l);;

(*------------------EXERCICE 4------------------ *)

let indice c = if c=' ' then 0 else (int_of_char c)-96;;

let rec fr (s : string) = 
  let l = (String.length s) in
  let f = Array.make 27 0 in
  for i = 0 to l-1 do
    f.(indice s.[i]) <- f.(indice s.[i])+1;
  done;
  f;;

let nb_char (s : string) =
  let count = ref 0 in
  let f = fr s in
  for i = 0 to 26 do
    if(f.(i) <> 0) then
      count:=!count+1;
  done;!count;;

nb_char "abbgff";;

let letter_tab (s : string) = 
  let t = Array.make (nb_char s) ' '  in
  let f = fr s in
  let j = ref 1 in
  if f.(0) <> 0 then t.(0) <- ' ';
  for i=1 to 26 do
    if(f.(i) <> 0) then (
      t.(!j) <- char_of_int(i+96); 
      j := !j+1;
    )
  done; t;;

let occur_tab (s : string) = 
  let t = Array.make (nb_char s) 0 in
  let f = fr s in
  let j = ref 0 in
  for i=0 to 26 do
    if(f.(i)<>0) then(
      t.(!j) <- f.(i);
      j:=!j+1;)
  done; t;;


let frequence (s : string) = (nb_char s, letter_tab s, occur_tab s);;

frequence "je code et je decode";;

let deux_plus_petits v n =
  let mini = ref (0,1) in
  let mini_value = ref (v.(0), v.(1)) in
  for k=0 to (n-1) do
    if(v.(k) < (fst !mini_value) && k <> (snd !mini)) then(
      mini := (k, snd !mini);
      mini_value := (v.(k), snd !mini_value);
    );
    if(v.(k) > (fst !mini_value) && v.(k) < (snd !mini_value) && k <> (fst !mini))then(
      mini := (fst !mini, k);
      mini_value := (fst !mini_value, v.(k));
    );
  done;!mini;;

deux_plus_petits [|7;2;4;1;2|] 5;;

type arbre_huffman = F of char | N of arbre_huffman * arbre_huffman;;

let rec affiche_arbre (a : arbre_huffman) =
  match a with
  |F(c) -> print_char c
  |N(g, d) -> print_char '(';(affiche_arbre g); print_char '.'; (affiche_arbre d);print_char ')';;


let b = N( N(F(' '), N(F('j'), F('o'))) , N(N(F('d'), N(F('t'), F('c'))),F('e')) );;
affiche_arbre b;;

let rem_arr a j = 
  let b = Array.make (Array.length a - 1) a.(0) in
  for i=0 to Array.length b -1 do
    if(i < j) then (
      b.(i) <- a.(i);
    ) else (
      b.(i) <- a.(i+1);
    )
  done;b;;

let construit_arbre (s : string) = 
  let (n,u,v) = frequence s in
  let w = ref (Array.make n (F(' '))) in
  for i=0 to n-1 do
    !w.(i) <- F(u.(i));
  done;
  let step w_s v_s = 
    let (i,j) = deux_plus_petits v_s (Array.length v_s) in
    let v_out = ref v_s in
    let w_out = ref w_s in
    !v_out.(i) <- !v_out.(i)+ !v_out.(j);
    !w_out.(i) <- N(w_s.(i), w_s.(j));
    v_out := rem_arr !v_out j;
    w_out := rem_arr !w_out j;
    (!w_out, !v_out);
  in 
  let rec aux w_s v_s =
    match(w_s, v_s) with
    |([|a|], [|b|]) -> w := w_s
    |(w_s, v_s) -> let (w_o, v_o) = step w_s v_s in aux w_o v_o;
  in aux !w v; !w.(0);;


construit_arbre "je code et je decode";;

let table_code (a : arbre_huffman) =
  let t = Array.make 27 "" in
  let rec aux a chem =
    match a with
    |F(c) -> t.(indice c) <- chem
    |N(g, d) -> (aux g (chem^"0")); (aux d (chem^"1"));
  in aux a "";t;;

table_code (construit_arbre "je code et je decode");;

