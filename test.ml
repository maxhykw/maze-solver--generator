(*
let test_gvalide () =
  Printf.printf "Test de la fonction gvalide ;\n";
  try
    Grid.gvalide (Grid.load "test/maze_2x1");
    Grid.gvalide (Grid.load "test/maze_3x2");
    Grid.gvalide (Grid.load "test/maze_4x8");
    Grid.gvalide (Grid.load "test/maze_6x6");
    Grid.gvalide (Grid.load "test/maze_100x100");
  with
  | _ -> failwith "Erreur, tous les labyrinthes sont censés être valides."
;;
*)

let test_load () = 
  Printf.printf "\n\nTest de la fonction load (par affichage) ;\n";
  begin 
    Grid.print (Grid.grille_of_gr (Grid.load "test/maze_2x1.laby")) ;
    Grid.print (Grid.grille_of_gr (Grid.load "test/maze_3x2.laby")) ;
    Grid.print (Grid.grille_of_gr (Grid.load "test/maze_4x8.laby")) ;
    Grid.print (Grid.grille_of_gr (Grid.load "test/maze_6x6.laby")) ;
    Grid.print (Grid.grille_of_gr (Grid.load "test/maze_100x100.laby")) ;
    Printf.printf "OK.";
  end
;;

let test_case_nature_to_char () =
  Printf.printf "\n\nTest de la fonction case_nature_to_char ;\n";
  let i = 0 in 
  let j = 0 in
  assert (Grid.case_nature_to_char (Grid.cree_case i j (Grid.char_to_nature '|')) = '|');
  assert (Grid.case_nature_to_char (Grid.cree_case i j (Grid.char_to_nature '-')) = '-');
  assert (Grid.case_nature_to_char (Grid.cree_case i j (Grid.char_to_nature '+')) = '+');
  assert (Grid.case_nature_to_char (Grid.cree_case i j (Grid.char_to_nature 'S')) = 'S');
  assert (Grid.case_nature_to_char (Grid.cree_case i j (Grid.char_to_nature 'E')) = 'E');
  assert (Grid.case_nature_to_char (Grid.cree_case i j (Grid.char_to_nature ' ')) = ' ');
  Printf.printf "OK.";
;;

let test_pr_coord () =
  Printf.printf "\n\nTest de la fonction pr_coord ;\n";
  begin 
    let () = Grid.pr_coord (Grid.cree_case 0 0 (Grid.char_to_nature '|')) in
    let () = Grid.pr_coord (Grid.cree_case 5 1 (Grid.char_to_nature '|')) in
    let () = Grid.pr_coord (Grid.cree_case 2 3 (Grid.char_to_nature 'S')) in
    let () = Grid.pr_coord (Grid.cree_case 4 4 (Grid.char_to_nature 'E')) in
    Printf.printf "\nOK.";
  end
;;

let test_pr_case_list_coord () =
  Printf.printf "\n\nTest de la fonction pr_case_list_coord ;\n";
  let gr = Grid.load "test/maze_3x2.laby" in
  Array.iter (fun l-> Grid.pr_case_array_coord l ; Printf.printf "\n";) (Grid.grille_of_gr gr);
  Printf.printf "\nOK.";
;;


let test_print () =
  Printf.printf "\n\nTest de la fonction print ;\n";
  begin 
    Printf.printf "Deja testee dans test_load() !\n";
    Printf.printf "OK.";
  end
;;

let test_print_pretty () =
  Printf.printf "\n\nTest de la fonction print_pretty ;\n";
  begin 
    Grid.print_pretty (Grid.load "test/maze_2x1.laby") ;
    Grid.print_pretty (Grid.load "test/maze_3x2.laby") ;
    Grid.print_pretty (Grid.load "test/maze_4x8.laby") ;
    Grid.print_pretty (Grid.load "test/maze_6x6.laby") ;
    Grid.print_pretty (Grid.load "test/maze_100x100.laby") ;
    Printf.printf "OK.";
  end
;;

let test_get () =
  Printf.printf "\n\nTest de la fonction get ;\n";
  let gr = Grid.load "test/maze_2x1.laby" in
  assert (Grid.get gr 0 0 = (Grid.cree_case 0 0 (Grid.char_to_nature '+')));
  assert (Grid.get gr 1 1 = (Grid.cree_case 1 1 (Grid.char_to_nature 'E')));
  let () = Grid.pr_coord (Grid.get gr 3 1 ) in
  assert (Grid.get gr 3 1 = (Grid.cree_case 3 1 (Grid.char_to_nature 'S')));
  try assert (Grid.get gr 10 10 =(Grid.cree_case 10 10 (Grid.char_to_nature '|'))) with _ -> Printf.printf "OK.";
;;


let test_find_index () =
  Printf.printf "\n\nTest de la fonction find_index ;\n";
  assert (Grid.find_index (fun x -> x = 3) [|1; 2; 3; 4|] = 2);
  assert (Grid.find_index (fun x -> x = 5) [|1; 2; 3; 4|] = -1);
;;


let test_find_S () =
  assert (Grid.find_S (Grid.load "test/maze_2x1.laby") = (3, 1));
  assert (Grid.find_S (Grid.load "test/maze_3x2.laby") = (1, 3));
  assert (Grid.find_S (Grid.load "test/maze_4x8.laby") = (7, 15));
  assert (Grid.find_S (Grid.load "test/maze_6x6.laby") = (1, 1));
  Printf.printf "OK.";
;;

let test_cases_voisines () =
  Printf.printf "\n\nTest de la fonction cases_voisines ;\n";
  let gr = Grid.load "test/maze_2x1.laby" in
  assert (Grid.cases_voisines gr (Grid.get gr 1 1) = [Grid.get gr 0 1; Grid.get gr 1 2; Grid.get gr 2 1; Grid.get gr 1 0]);
  Printf.printf "OK.";
;;

let test_cases_visitees () =
  Printf.printf "\n\nTest de la fonction cases_visitees ;\n";
  let c1 = (Grid.cree_case 1 1 (Grid.char_to_nature 'E')) in 
  let c2 = (Grid.cree_case 1 2 (Grid.char_to_nature 'S')) in
  let c3 = (Grid.cree_case 1 6 (Grid.char_to_nature '|')) in
  let c_list = [c1; c2; c3] in
  assert (Grid.cases_visitees c_list c1);
  assert (Grid.cases_visitees c_list c2);
  assert (Grid.cases_visitees c_list c3);
  Printf.printf "OK.";
;;

let test_estBord () =
  Printf.printf "\n\nTest de la fonction estBord ;\n";
  let gr = Grid.load "test/maze_3x2.laby" in
  assert (Grid.estBord gr (Grid.get gr 0 0));
  assert (Grid.estBord gr (Grid.get gr 6 0));
  assert (Grid.estBord gr (Grid.get gr 6 4));
  Printf.printf "OK.";
;;

let test_solve () =
  Printf.printf "\n\nTest de la fonction solve ;\n";
  List.iter (fun c -> Printf.printf "(%d, %d), " (fst (Grid.coord_of_case c)) (snd (Grid.coord_of_case c))) (Grid.solve "test/maze_3x2.laby");
  Printf.printf "\nOK.\n\n";

;;

(*
let test_voisin_de_voisin () =
  Printf.printf "\n\nTest de la fonction voisin_de_voisin ;\n";
  let gr = Grid.load "test/maze_3x2.laby" in
  assert (Grid.voisin_de_voisin gr (Grid.get gr 2 2) = [Grid.get gr 0 2; Grid.get gr 2 4; Grid.get gr 4 2; Grid.get gr 2 0]);
  Printf.printf "Ok.";
;;*)
(*
let test_cases_vides () = 
  Printf.printf "\n\nTest de la fonction cases_vides ;\n";
  let gr = Grid.load "test/maze_3x2.laby" in
  Grid.print gr;
  Printf.printf "Les cases vides du labyrinthe 3x2.laby sont : \n";
  Grid.affiche_c_array (Grid.cases_vides gr);
  let gr = Grid.load "test/maze_2x1.laby" in
  Grid.print gr;
  Printf.printf "Les cases vides du labyrinthe 2x1.laby sont : \n";
  Grid.affiche_c_array (Grid.cases_vides gr);
  Printf.printf "Ok.";
;;

let test_creer_grille () =
  Printf.printf "\n\nTest de la fonction creer_grille ;\n";
  Printf.printf "Grille de dimension 5x5 -> labyrinthe de dimension 2x2 : \n";
  Grid.print (Grid.creer_grille 5 5);
  Printf.printf "\n Grille de dimension 7x5 -> labyrinthe de dimension 3x2 : \n";
  Grid.print (Grid.creer_grille 7 5);
  Printf.printf "\n Grille de dimension 13x13 -> labyrinthe de dimension 6x6 :\n";
  Grid.print (Grid.creer_grille 13 13);
  Printf.printf "Ok.";
;;
*)
(*
let test_case_S_E () =
  Printf.printf "\n\nTest de la fonction case_S_E ;\n";
  Printf.printf "Grille de dimension 5x5 -> labyrinthe de dimension 2x2 sans cases de départ et d'arrivée : \n";
  let gr = Grid.creer_grille 5 5 in
  Grid.print gr;
  Printf.printf "Grille avec cases S et E ;\n";
  Grid.print (Grid.case_S_E gr);
  Printf.printf "\n Grille de dimension 13x13 -> labyrinthe de dimension 6x6 : avec ajout des cases S et E\n";
  Grid.print (Grid.case_S_E (Grid.creer_grille 13 13));
  Printf.printf "Ok. \n\n";
;;
*)

let lance_test () = 
    (*test_gvalide (); 
    test_load ();*)
    test_case_nature_to_char ();
    test_pr_coord ();
    test_pr_case_list_coord ();
    (*test_print ();
    test_print_pretty (); *)
    test_get (); 
    test_find_index ();
    test_find_S ();
    test_cases_voisines ();
    test_cases_visitees ();
    test_estBord ();
    test_solve ();
    (*
    test_voisin_de_voisin ();
    test_cases_vides ();
    test_creer_grille ();
    test_case_S_E ();
    *)

