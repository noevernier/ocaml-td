let donne_A (n : int) = 
    let i = ref 0 in
    let j = ref 0 in
    while ((!i + !j) <= n+1) do
        let a = 2*(!i)+1 in
        let b = 3*(!j)+1 in
        if(a < b) then ((print_int a); (i := !i+1);)
        else if(a > b) then ((print_int b); (j := !j+1);)
        else ((print_int b); i := !i +1; j := !j+1);
    done;;
        

(donne_A 10);;

let rec get_elem (l : 'a list) (i : int) =
    match (l, i) with
    |([], i) -> failwith "Liste Vide"
    |(t::q, i) -> if(i = 0) then t else (get_elem q (i-1));;

let rec affiche_list (l : int list) = 
    match l with
    |[] -> ()
    |t::q -> (print_int t); (print_string " "); (affiche_list q);;

let donne_B (n : int) = 
    let b = ref [1] in
    while List.length !b < n do
        let i = ref 0 in
        let j = ref 0 in
        while ((!i + !j) <= List.length !b) do
            let x = 2*(get_elem !b !i)+1 in
            let y = 3*(get_elem !b !j)+1 in
            if(x < y) then (b := x::!b; i := !i +1;)
            else if (y < x) then (b := y::!b; j := !j+1;)
            else (b := x::!b; j := !j+1; i := !i +1;)
        done;
    done;
    (affiche_list (!b));;


let list_reverse l =
  let rec aux a l =
    match l with
    |[] -> a
    |x::l -> (aux (x::a) l);
  in aux [] l;;

let rec int_to_list (n : int) =
    match n with
    |0 -> []
    |n -> let r = (n mod 10) in r::(int_to_list ((n-r)/10));;

int_to_list 113;;

let est_palindrome (n : int) = (list_reverse (int_to_list n)) = (int_to_list n);;

est_palindrome 11311;;

let f_spirale (p : int) (q : int) =
    let pos = ref [] in
    for i = 0 to (p-1) do
        pos := (q+i, q+0)::(!pos);
    done;
    for i = 1 to (p-1) do
        pos := (q+p-1,q+ i)::(!pos);
    done;
    for i = p-2 downto 0 do
        pos := (q+i, q+p-1)::(!pos);
    done;
    for i = p-2 downto 1 do
        pos := (q+0, q+i)::(!pos);
    done;
    list_reverse(!pos);;

f_spirale 5 0;;
f_spirale 3 1;;
f_spirale 1 2;;

let matrice_spirale (n : int) =
    let tab = (Array.make_matrix n n 0) in
    let trace = ref (f_spirale n 0) in
    for i = 1 to n/2  do
        trace := !trace@(f_spirale (n-2*i) i);
    done;
    for j=0 to (n*n-1) do
        let pos = (get_elem !trace j) in
        let x = fst pos in
        let y = snd pos in
        tab.(y).(x) <- j;
    done;
    tab;;

matrice_spirale 5;;
