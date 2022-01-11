type 'a tas = {mutable taille : int; tab : 'a array};;
let nmax = 1000;;

let creerTasVide (valeur) = {taille = 0; tab = Array.make nmax valeur};;

let echange tas i j =
  let stock = tas.tab.(i) in
  tas.tab.(i) <- tas.tab.(j);
  tas.tab.(j) <- stock;;

let rec remonte tas i =
  match i with
  |0 -> failwith "Valeur Inconnu"
  |1 -> ()
  |i -> if(tas.tab.(i) <= tas.tab.(i/2)) then ()
    else (
      (echange tas i (i/2)); (remonte tas (i/2))
    );;

let rec descend tas i = 
  match i with
  |0 -> failwith "Valeur inconnu"
  |i -> if(tas.taille < 2*i) then () else (
      if(tas.taille < 2*i+1) then (
        if(tas.tab.(i) < tas.tab.(2*i)) then (
          (echange tas i (2*i));
          (descend tas (2*i));
        )
      ) else (
        if(tas.tab.(2*i) > tas.tab.(2*i+1)) then(
          if(tas.tab.(i) < tas.tab.(2*i)) then(
            (echange tas i (2*i));
            (descend tas (2*i));
          )
        ) else (
          if(tas.tab.(i) < tas.tab.(2*i+1))then(
            (echange tas i (2*i+1));
            (descend tas (2*i+1));
          )
        )
      )
    );;

let ajoute tas x =
  tas.tab.(tas.taille+1) <- x;
  tas.taille <- tas.taille+1;
  remonte tas tas.taille;;

let creerTas1 t = 
  let tas = creerTasVide (t.(0)) in
  for i=0 to (Array.length t -1) do
    (ajoute tas (t.(i)));
  done;tas;;

creerTas1 [|1;2;3;4;5;6;7;8;9|];;

let creerTas2 t =
  let tas = creerTasVide (t.(0)) in
  tas.taille <- (Array.length t);
  for i=0 to (Array.length t -1) do
    tas.tab.(i+1) <- t.(i);
  done;
  for i=((Array.length t)/2) to 1 do
    descend tas i;
  done;tas;;

