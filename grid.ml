(** Definition des types **)

type nature = Mur  
            | Moins 
            | Plus  
            | S 
            | E  
            | Vide 
            | Solution 
            
type case = { 
  coord : int * int ;
  n : nature ;
}

type grille = {
  g : case array array  ;
  n : int ; (*Dimension de la grille (n x m)*)
  m : int 
}



(** Definition des fonctions de grille **)

(*Renvoie un booléen en fonction de si les coordonnées sont valides ou non*)
let coordValide a gr=
  (fst a >= 0) && (snd a >= 0) && (fst a < gr.n) && (snd a < gr.m)


(*renvoie un booléen en fonction de si la case a une nature Mur ou non*)
let estBord gr c = 
  (fst c.coord = 0) || (snd c.coord = 0) || (fst c.coord = gr.n - 1) || (snd c.coord = gr.m - 1)

(**)
let gvalide gr =
  let l1 = Array.length (gr.g.(0)) in (*vérifie que toutes les lignes sont de même taille*)
  let rec read_line gr i (tmp : int*int) =
    if i < gr.m then
      let l2 = Array.length (gr.g.(i)) in
      if l1 != l2 then 
          failwith "les lignes ne sont pas toutes de même taille"
      else
        let n = Array.fold_left (fun (accE, accS) (c : case) -> (*on vérifie qu'il existe bien une case départ et une case arrivée*)
                                  if c.n = E then (accE + 1, accS)
                                  else if c.n = S then (accE, accS + 1)
                                  else (accE, accS)) (0, 0) gr.g.(i) in
                                  
      read_line gr (i+1) (fst tmp + fst n, snd tmp + snd n)
    else
      tmp
  in
    let r = read_line gr 1 (0, 0) in
    if r != (1, 1) then failwith "il n'y a pas une case départ et une case d'arrivée";
;;

let char_to_nature c = 
  match c with
    '|' -> Mur
  | '-' -> Moins
  | '+' -> Plus
  | 'S' -> S
  | 'E' -> E
  | '*' -> Solution
  | ' ' -> Vide 
  
  | _ -> failwith "Nature inexistante"
;;

let cree_case i j nat =
  {coord = (i, j) ; n = nat} 

let rev_array_array (a : case array array) = 
  let temp = Array.to_list (Array.map (fun ligne -> Array.to_list ligne) a) in
  let res = List.rev temp in
  Array.of_list (List.map (fun ligne -> Array.of_list ligne) res) 

let load file = 
  let c_in = open_in file in 

  let rec read_file temp_res temp_tab i j  =
    match input_char c_in with 
        exception End_of_file -> Array.of_list temp_res
      | ' '  -> read_file temp_res ((cree_case i j Vide)     ::temp_tab) i (j+1)
      | '|'  -> read_file temp_res ((cree_case i j Mur)      ::temp_tab) i (j+1)
      | '+'  -> read_file temp_res ((cree_case i j Plus)     ::temp_tab) i (j+1)
      | '-'  -> read_file temp_res ((cree_case i j Moins)    ::temp_tab) i (j+1)
      | 'E'  -> read_file temp_res ((cree_case i j E)        ::temp_tab) i (j+1)
      | 'S'  -> read_file temp_res ((cree_case i j S)        ::temp_tab) i (j+1)
      | '*'  -> read_file temp_res ((cree_case i j Solution) ::temp_tab) i (j+1)
      | '\n' ->
          (* En remettant dans l'ordre les elements *)
          let t = Array.of_list (List.rev temp_tab) in 
          read_file  (t::temp_res) [] (i+1) 0
      
      | _ -> failwith "Caractere invalide"
  in
  (* On remet dans l'ordre les sous-tableaux *)
  let arr = rev_array_array (read_file [] [] 0 0) in 
  let () = close_in c_in in
  let res = {g = arr ; n = Array.length arr ; m = Array.length arr.(1)} in
  (* let () = gvalide res in *)
  res



let case_nature_to_char(c : case) =
  match c.n with 
      Mur      -> '|'
    | Moins    -> '-'
    | Plus     -> '+'
    | S        -> 'S'
    | E        -> 'E'
    | Solution -> '*'
    | Vide     -> ' '


let grille_of_gr gr =
  gr.g

let coord_of_case c =
  c.coord 

let pr_coord (c : case) : unit = 
    Printf.printf "(%d, %d), " (fst c.coord) (snd c.coord)

let pr_case_array_coord c_list =
    Array.iter pr_coord c_list 

let print a =
    let affiche_case c = Printf.printf "%c" (case_nature_to_char c) in
    let affiche_ligne ligne = Array.iter affiche_case ligne;
                              Printf.printf "\n"
    in 
    Array.iter affiche_ligne a
 

let print_pretty arr =
    let case_to_string (c : case) =
      match c.n with
      | Mur          -> "▉" 
      | Moins | Plus -> "▉"
      | S            -> "\x1b[32m▓\x1b[0m"  (* Vert *)
      | E            -> "\x1b[31m▓\x1b[0m"  (* Rouge *)
      | Solution     -> "\x1b[32m▒\x1b[0m"  (* Rouge *)
      | Vide         -> "░"
    in
  
    let affiche_case (c : case) =
      Printf.printf "%s" (case_to_string c)
    in
  
    let affiche_ligne ligne =
      Printf.printf "│";
      Array.iter affiche_case ligne;
      Printf.printf "│\n"
    in

    Printf.printf "┌";
  Array.iter (fun _ -> Printf.printf "─") arr.(0);
    Printf.printf "┐\n";
  
    Array.iter affiche_ligne arr;
  
    Printf.printf "└";
    Array.iter (fun _ -> Printf.printf "─") arr.(0);
    Printf.printf "┘\n"


let get gr i j = 
  if i >= gr.n || j >= gr.m || i < 0 || j < 0 then
      failwith "Coordonnees non valides !"
  else 
    gr.g.(i).(j)

  
    (*Partie resolution*) 

let find_index f a =
  let len = Array.length a in
  let rec find i =
    if i >= len then
        -1
    else if f (a.(i)) then
        i
    else 
        find (i + 1)
  in find 0


let find_S (gr : grille) =
    (* On considere negliger les labyrinthes non valides *)
    (*let () = gvalide gr in*)

    (*On cree un array contenant des entiers tels qu'un seul sera different
     de -1 (l'indice de S (element unique) dans un des sous-array)*)
    let array_j = Array.map (fun l -> (find_index (fun (c : case) -> c.n = S) l)) gr.g in
    (*On trouve l'indice de ce sous-tableau contenant S*)
    let i = find_index (fun x -> x <> -1) array_j in
    let j = array_j.(i) in 
    (i, j)


let cases_voisines (gr : grille) c : case list = 
    let c1 = (get gr (fst c.coord-1) (snd c.coord))   in (*Nord*)
    let c2 = (get gr (fst c.coord)   (snd c.coord+1)) in (*Est*)
    let c3 = (get gr (fst c.coord+1) (snd c.coord))   in (*Sud*)
    let c4 = (get gr (fst c.coord)   (snd c.coord-1)) in (*Ouest*)
    c1::c2::c3::c4::[]

let cases_visitees (c_liste : case list) case =   
    List.exists (fun c -> c = case) c_liste 

let solve fichier = 
    let gr = load fichier in 
    let coord_S = find_S gr in 
    let arr = Array.make_matrix (gr.n) (gr.m) [] in

    let rec loop i j ch_courant c_visitee = 
      let c_curr = (get gr i j) in 
    
      (* Sortie trouvée *)
      if c_curr.n = E then 
        let () = arr.(i).(j) <- ch_courant in  (* On stocke le chemin *)
        Some ch_courant (* .. et indique qu'on a trouve un chemin*)
  
      (* Case non visitée et non au bord du labyrinthe *)
      else if not (cases_visitees c_visitee c_curr) && not (estBord gr c_curr) then 
        let c_visitee_new = (c_curr :: c_visitee) in 

        (* On etudie les voisins autour de la case courante *)
        let rec loop_voisin l_case =
          match l_case with 
          | [] -> None  (* Aucun chemin trouvé parmi les voisins courants *)
          | c::ll -> 
            (* Si c est vide (un passage) ou bien notre point d'arrivee *)
            if (c : case).n = Vide || (c : case).n = E then 
              (* On cherche récursivement à partir de c *)
              match loop (fst c.coord) (snd c.coord) (c::ch_courant) c_visitee_new with
                  Some chemin_trouve -> Some chemin_trouve  (* Chemin trouvé, on a finit *)
                | None -> loop_voisin ll (*Si échec, passer à la case accessible suivante*)
            else 
              loop_voisin ll  (*Si échec, passer à la case accessible suivante*)
        in 
        loop_voisin (cases_voisines gr c_curr)
        
      (* Case courante déjà visitée, abandon *)
      else 
        None  
    in 
    let loop_res = loop (fst coord_S) (snd coord_S) [] [] in

    (* On veut retrouver le couple d'indice (i, j) de arr *)
    (* Respectivement l'indice de la ligne contenant le sous-element qui n'est 
       pas une liste vide (unique car etant aux coordonnees de E), et puis le 
       sous-indice de cet element*)
    let i = find_index (fun l -> (find_index (fun c_list -> c_list <> []) l) <> -1) arr in 
    let j = find_index (fun c_list -> c_list <> []) (arr.(i)) in

    match loop_res with
      Some res -> arr.(i).(j)
    | None -> []


let pr_chemin_solved fichier =
    List.iter (fun case -> pr_coord case) (solve fichier)

let get_premier_elt l =
  match l with
      []        -> failwith "La liste est vide !"
    | elt :: ll -> (elt, ll)
;;

let pr_grid_chemin_solved fichier pretty =
  let c_list = solve fichier in
  let gr = load fichier in 

  (* On transforme la liste de cases en une liste de coordonnees *)
  let coord_c_list = List.map (fun c -> c.coord) c_list in 
  let arr = Array.copy gr.g in

  let rec loop l = 
    match l with 
      []           -> ()
    | (x, y) :: ll -> 
          let () = arr.(x).(y) <- cree_case x y Solution in
          loop ll 
  in 
  let () = loop coord_c_list in
  if pretty then 
      print_pretty arr 
  else 
      print arr


let pr_chemin_solved_pretty fichier = 
    let c_list = solve fichier in
    let (first_elt, c_list) = get_premier_elt c_list in
    Printf.printf "\n\n\nChemin solution :\n";
    Printf.printf "\x1b[31m(%d, %d), \x1b[0m" (fst first_elt.coord) (snd first_elt.coord);

    let rec loop l_reste = 
      match l_reste with 
        []         -> ()
      | c1::c2::ll -> pr_coord c1;
                      pr_coord c2;
                      loop ll
      | c::ll      -> Printf.printf "\x1b[32m(%d, %d), \x1b[0m" (fst c.coord) (snd c.coord)
    in
    loop c_list;






    (*Partie generateur*) 

(*affiche un tableau de case -> sert à tester la fonction cases_vides*)
let affiche_c_array c_a = 
  let affiche_case (c : case) =
    Printf.printf "(%d, %d) " (fst c.coord) (snd c.coord)
  in
  Array.iter affiche_case c_a


(*renvoie un tableau de toutes les cases de nature Vide dans une grille*)
let cases_vides gr =
  let rec ligne_vide i acc =
    if i = gr.n then acc
    else 
      let rec c_vide i j acc =
        if j = gr.m then acc
        else 
          if gr.g.(i).(j).n = Vide then 
            c_vide i (j + 1) ((get gr i j) :: acc)
          else c_vide i (j + 1) acc
      in let l = c_vide i 0 acc in
    ligne_vide (i + 1) l
  in Array.of_list(ligne_vide 0 [])

  (*place de manière aléatoire la case de départ et la case d'arrivée dans une grille passée en paramètre*)
let case_S_E gr =
  let c_vides = cases_vides gr in
  let a = Array.length c_vides in
  let c_S = c_vides.(Random.int a) in 
  gr.g.(fst c_S.coord).(snd c_S.coord) <- {coord = c_S.coord; n = S};
  let c_E = c_vides.(Random.int a) in
  gr.g.(fst c_E.coord).(snd c_E.coord) <- {coord = c_E.coord; n = E};
gr    


(*renvoie une grille initialisée avec que des Mur*)

let creer_grille nl nc =
  let rec c_g i acc =
    if i = 2*nl + 1 then acc
    else
      (*Creer_ligne renvoie une liste des cases avec les bonnes coordonnées j*)
      let rec c_ligne i j acc =
        if j = 2*nc + 1 then acc
        else
          if i mod 2 = 0 then
            if j mod 2 = 0 then 
              (*les cases de coordonées (paire, paire) sont des Murs verticaux*)
              c_ligne i (j + 1) ({coord = (i, j); n = Plus} :: acc)
              (*les cases de coordonnées (paire, impaire) sont des cases vides*)
            else c_ligne i (j + 1) ({coord = (i, j); n = Moins} :: acc)
          else
            if j mod 2 = 0 then
              (*les cases de coordonées (imapaire, paire) sont des Murs horizontaux*)
              c_ligne i (j + 1) ({coord = (i, j); n = Mur} :: acc)
              (*les cases de coordonées (impaire, impaire) sont des + *)
            else c_ligne i (j + 1) ({coord = (i, j); n = Vide} :: acc)
       
      in let t = Array.of_list (c_ligne i 0 [])
  in c_g (i + 1) (t :: acc) in
{g = Array.of_list (c_g 0 []); n = 2*nl + 1; m = 2*nc + 1}

  (*renvoie une liste avec les cases voisines des voisines de la case courante
     -> utiliser lorsque l'on regarde chaque cases autour de soi sans prendre en compte les Mur dans la fonction random*)
let voisin_de_voisin gr c =
  let c1 = (get gr (fst c.coord-2) (snd c.coord))   in (*Nord*)
  let c2 = (get gr (fst c.coord)   (snd c.coord+2)) in (*Est*)
  let c3 = (get gr (fst c.coord+2) (snd c.coord))   in (*Sud*)
  let c4 = (get gr (fst c.coord)   (snd c.coord-2)) in (*Ouest*)
  c1::c2::c3::c4::[]


(*génère un labyrinthe de manière aléatoire avec sa dimension passée en paramètre*)



let random nl nc =

  (*gr_i est la grille initialisée*)
  
  let gr_i = creer_grille nl nc in

  let a = cases_vides gr_i in
  (*on choisit aléatoirement une case de départ parmis les cases Vide*)
  let c_cou_coor = a.(Random.int (Array.length a)) in

  let c_courante = get gr_i (fst c_cou_coor.coord) (snd c_cou_coor.coord) in
(*partie récursive de la fonction*)

  let rec loop grille_courante i j (c_visitees : case list) =

      (*si la case n'a pas déjà été visitée*)
      if (not (case_visitee c_visitees c_courante)) then

        (*on la rajoute dans les cases visitées et on recommence la partie récursive*)
        loop grille_courante i j (c_courante :: c_visitees)

      (*une fois la case marquée comme visitée 
         (si elle ne l'avait pas été elle est rajoutée avec la ligne au dessus)*)
      else
        (*Pour toutes les cases voisines de la case courante*)
        let rec loop_voisins l_voisins grille_c =
          match l_voisins with
          (*quand on a vu tous les voisins on renvoie la grille*)
          [] -> grille_c
          (*sinon*)
          |(c : case) :: ll -> 
            (*on regarde la case entre la case courante et c*)
            let mi = get grille_c (((fst c.coord )+ (fst c_courante.coord))/2) (((snd c.coord )+ (snd c_courante.coord))/2) in
            
            (*si la case entre c_courante et c n'est pas un bord de la grille*)
            
            if not (estBord grille_c mi) then 
             
            (*on retire le mur entre la case courante et c*)
              grille_c.g.(fst mi.coord).(snd mi.coord) <- {coord = (fst mi.coord, snd mi.coord); n = Vide};
              
              (*on continue à partir de c*)  
              loop grille_c (fst c.coord) (snd c.coord) c_visitees in 
              
            (*on passe à la case accessible suivante/aux autres voisins*)
            loop_voisins ll grille_c
         (*on applique la fonction aux voisins de la case courante*)
        in
        let grille_modifiee = loop_voisins (voisin_de_voisin grille_courante c_courante) grille_courante in

  let g_finale = loop gr_i (fst c_courante.coord) (snd c_courante.coord)  [] in
  (*on positionne de manière aléatoire les cases de départ et d'arrivée*)
case_S_E g_finale
