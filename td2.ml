(*--------------------EXERCICE 1--------------------*)
type 'a arbre = Vide | N of ('a arbre)*('a)*('a arbre);;

let rec imprime_infixe (a : 'a arbre) =
    match a with
    |Vide -> ()
    |N(g, e, d) -> (imprime_infixe g); (print_int e); (imprime_infixe d);;

imprime_infixe (N(N(Vide, 2, Vide), 1, N(N(Vide, 6, Vide), 4, Vide)));;

let rec imprime_prefixe (a : 'a arbre) =
    match a with
    |Vide -> ()
    |N(g, e, d) -> (print_int e); (imprime_infixe g); (imprime_infixe d);;

imprime_prefixe (N(N(Vide, 2, Vide), 1, N(N(Vide, 6, Vide), 4, Vide)));;

let rec imprime_postfixe (a : 'a arbre) =
    match a with
    |Vide -> ()
    |N(g, e, d) -> (imprime_infixe g); (imprime_infixe d); (print_int e);;

imprime_postfixe (N(N(Vide, 2, Vide), 1, N(N(Vide, 6, Vide), 4, Vide)));;

let rec profondeur (a : 'a arbre) = 
    match a with
    |Vide -> 0
    |N(g, e, d) -> 1 + (max (profondeur g) (profondeur d));;

(*--------------------EXERCICE 2--------------------*)
let rec liste_noeuds (a : 'a arbre) (n : int) =
    match (n, a) with
    |(0, a) ->  let rec get_node_list (a : 'a arbre) = 
                match a with
                |Vide -> []
                |N(g, e, d) -> e::((get_node_list g)@(get_node_list d));
            in (get_node_list a)
    |(n, Vide) -> []
    |(n, N(g, e, d)) ->  (liste_noeuds g (n-1))@(liste_noeuds d (n-1));;

liste_noeuds (N(N(Vide, 2, Vide), 1, N(N(Vide, 6, Vide), 4, Vide))) 0;;

let largeur (a : 'a arbre) = liste_noeuds a 0;;
(*--------------------EXERCICE 3--------------------*)
type abr = Vide | N of abr*(int)*abr;;


let rec list_infixe (a : abr) =
    match a with
    |Vide -> []
    |N(g, e, d) -> (list_infixe g)@[e]@(list_infixe d);;

let rec sort_test (l : int list) = 
    match l with
    |[] -> true
    |[e] -> true
    |e::f::l -> (e <= f)&&(sort_test (f::l));;

let rec test_abr (a : abr) = sort_test (list_infixe a);;

let rec trouve (a : abr) (x : int) =
    match a with
    |Vide -> false
    |N(g, e, d) -> if(e = x) then true else if(e < x) then (trouve d x) else (trouve g x);;

let rec insere_feuille (a : abr) (x : int) = 
    match a with
    |Vide -> N(Vide, x, Vide)
    |N(g, e, d) -> if(x <= e) then N((insere_feuille g x), e, d) else N(g, e,(insere_feuille d x));;

let rec min_arbre (a : abr) =
    match a with 
    |Vide -> min_int
    |N(Vide, e, d) -> e
    |N(g, e, d) -> (min_arbre g);;

let rec supprime (a : abr) (x : int) =
    match a with
    |Vide -> Vide
    |N(_,_,_) when( not(trouve a x)) -> a
    |N(Vide, e, Vide) when(e=x) -> Vide
    |N(g, e, d) -> if(x < e) then (N((supprime g x),e,d))
                    else if(x > e) then (N(g, e, (supprime d x)))
                    else let m = (min_arbre d) in N(g, m, (supprime d m));;

let rec creer_abr (l : int list) = 
    match l with
    |[] -> Vide
    |t::q -> insere_feuille (creer_abr q) t;;

let insere_racine_bis (a : abr) (x : int) = creer_abr(x::(list_infixe a));;

let rec infos (a : abr) = 
    match a with
    |Vide -> failwith "Arbre vide"
    |N(Vide, x, Vide) -> [|x;x;1|]
    |N(g,x,Vide) -> let i = infos g in  if(x >= i.(1)) then [|i.(0); x;1|] else [|i.(0); x; 1|] 
    |N(g,x,d) -> let ig = infos g and id = infos d in
                if((ig.(2)==1) && (id.(2)==1) && (x >= ig.(1)) && (x <= id.(0))) then [|ig.(0); id.(1); 1|] 
                else [|ig.(0); id.(1); 0|];;

infos (creer_abr [1;4;3;5;8;23;3]);;


(*--------------------EXERCICE 4--------------------*)
type operation = Plus | Mult | Div | Minus;;

type abr_expr = Zero | F of int | N of operation * abr_expr * abr_expr;;

let rec evalue (a : abr_expr) = 
    match a with
    |Zero -> 0
    |F(e) -> e
    |N(Plus, g, d) -> (evalue g) + (evalue d)
    |N(Mult, g, d) -> (evalue g) * (evalue d)
    |N(Div, g, d) -> (evalue g) / (evalue d)
    |N(Minus, g, d) -> (evalue g) - (evalue d);;

let rec imprime_infixe (a : abr_expr) =
    match a with
    |Zero -> ()
    |F(e) -> print_int(e);
    |N(Plus, g, d) -> (print_string "("); (imprime_infixe g); (print_string "+"); (imprime_infixe d);(print_string ")"); 
    |N(Mult, g, d) -> (print_string "("); (imprime_infixe g); (print_string "*"); (imprime_infixe d);(print_string ")"); 
    |N(Div, g, d) -> (print_string "("); (imprime_infixe g); (print_string "/"); (imprime_infixe d);(print_string ")"); 
    |N(Minus, g, d) -> (print_string "("); (imprime_infixe g); (print_string "-"); (imprime_infixe d);(print_string ")");;

imprime_infixe (N(Minus, N(Mult,N(Plus, F(4), F(3)), N(Div, N(Minus, Zero, F(12)), F(2))), F(5)));;

let rec imprime_postfixe (a : abr_expr) =
    match a with
    |Zero -> ()
    |F(e) -> print_int(e);
    |N(Plus, g, d) -> (print_string "("); (imprime_infixe g); (imprime_infixe d); (print_string "+");(print_string ")"); 
    |N(Mult, g, d) -> (print_string "("); (imprime_infixe g);  (imprime_infixe d);(print_string "*");(print_string ")"); 
    |N(Div, g, d) -> (print_string "("); (imprime_infixe g);  (imprime_infixe d);(print_string "/");(print_string ")"); 
    |N(Minus, g, d) -> (print_string "("); (imprime_infixe g);  (imprime_infixe d);(print_string "-");(print_string ")");;

imprime_postfixe (N(Minus, N(Mult,N(Plus, F(4), F(3)), N(Div, N(Minus, Zero, F(12)), F(2))), F(5)));;
