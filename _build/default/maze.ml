let dump_file f =
  let fichier = open_in f in 
  let rec read_fichier f = 
      try begin
        Printf.printf "%s\n" (input_line fichier) ;
        read_fichier f
      end
  with End_of_file -> Printf.printf "Fin du fichier"
  in
  read_fichier fichier ;
  close_in fichier 

  
let display_help_message () = 
  begin 
    Printf.printf "\n\x1b[32mmaze.exe print <fichier.laby> :\x1b[0m lit le labyrinthe se 
    trouvant dans le fichier fichier.laby et l’affiche dans la console.\n\n";

    Printf.printf "\x1b[32mmaze.exe print --pretty <fichier.laby> :\x1b[0m lit le labyrinthe se 
    trouvant dans le fichier fichier.laby et, de manière plus élégante, 
    l’affiche dans la console :). \n\n";
    
    Printf.printf "\x1b[32mmaze.exe solve <fichier.laby> :\x1b[0m lit le labyrinthe se
    trouvant dans le fichier fichier.laby et l’affiche dans la console
    en mettant en évidence le chemin du départ vers l’arrivée\n\n";

    Printf.printf "\x1b[32mmaze.exe solve -- pretty <fichier.laby> :\x1b[0m lit le labyrinthe se
    trouvant dans le fichier fichier.laby et l’affiche dans la console, de manière plus élégante,
    en mettant en évidence le chemin du départ vers l’arrivée\n\n";

    Printf.printf "\x1b[32mmaze.exe random <n> <m> [r] :\x1b[0m génère un labyrinthe
    aléatoire de hauteur n et de largeur m, utilisant éventuellement
    l’entier positif r comme graine initiale pour le générateur de nombres
    aléatoires.\n\n"; 

    Printf.printf "\x1b[32mmaze.exe test :\x1b[0m Lance les tests sur l'integralite des 
    fonctions, dont des affichages entres autres.\n\n"; 
  end


let exec () =
  match Array.to_list Sys.argv with
      [_; "--help"] -> display_help_message ()

    | [_; "print"; fichier] -> 
            Printf.printf "\nAffichage du labyrinthe à faire!\n";
            Grid.print (Grid.grille_of_gr (Grid.load fichier));
            Printf.printf "\n";
                                 
    | [_; "print"; "--pretty"; fichier] -> 
            Printf.printf "\nAffichage du labyrinthe à faire!\n"; 
            Grid.print_pretty (Grid.load fichier);
            Printf.printf "\x1b[32mEn vert, la sortie !\x1b[0m\n";
            Printf.printf "\x1b[31mEn rouge, l'entrée !\x1b[0m\n\n";
            Printf.printf "\x1b[36mB\x1b[35mo\x1b[33mn \x1b[34mc\x1b[35mo\x1b[36mu\x1b[33mr\x1b[0ma\x1b[31mg\x1b[32me\x1b[34m ~ \x1b[31m!\x1b[0m\n\n"    
            
    | [_; "solve"; fichier] -> 
            Printf.printf "\nRésolution à faire!\n\n";
            Grid.pr_grid_chemin_solved fichier;             
            Printf.printf "\n\n\nChemin solution :\n";
            Grid.pr_chemin_solved fichier;
            Printf.printf "\n\n";

    | [_; "solve"; "--pretty"; fichier] -> 
            Printf.printf "\nRésolution à faire!\n\n";
            Grid.pr_grid_chemin_solved_pretty fichier;
            Printf.printf "\n\n";

    | [_; "random"; n ; m] -> 
            Printf.printf "\nLabyrinthe aux dimensions %sx%s" n m;
            (*Grid.print (Grid.random (int_of_string n) (int_of_string m));
            *)Printf.printf "\n";

    | [_; "test"] -> Test.lance_test () 

    | _ -> failwith "Ligne de commande invalide, ou argument(s) manquant(s) !"

 
let () = exec ()
