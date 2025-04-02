(* Type de la nature d'une case, si c'est un mur, la case de départ, celle d'arrivée... *)
type nature

(* Type case qui a des coordonnées uniques, un type nature et une direction *)
type case

(* Le type grille qui est un tableau de tableaux de cases avec des entiers qui indiquent sa taille *)
type grille



(* Vérifie que la grille (labyrinthe) respecte bien les contraintes imposées *)
val gvalide : grille -> unit

(*Renvoie un booléen en fonction de si les coordonées sont valides ou non*)
val coordValide : int*int -> grille -> bool

(*Renvoie un booléen en fonction de si la case est un Mur ou non*)
val estBord : grille -> case -> bool

(* Prend un caracere un argument et en renvoie sa nature ; renvoie une erreur dans le cas echeant *)
val char_to_nature : char -> nature 

(* Prend en argument des coordonnees et une nature, puis renvoie la case ainsi cree *)
val cree_case : int -> int -> nature -> case

(* Prend en argument un array d'array de case et renverse l'ordre dans lequel est place les sous-arrays (et non pas leurs elements) *)
val rev_array_array : case array array -> case array array

(* Fonction qui télécharge un fichier et le charge en une grille *)
val load : string -> grille




(* Convertit la nature d'une case en char *)
val case_nature_to_char : case -> char

(* Prend en argument une grille et en renvoie le contenu de son champ g*)
val grille_of_gr : grille -> case array array

(* Prend en argument une liste de case et renvoie un couple compose de son premier 
   element, et de son reste*)
val get_premier_elt : case list -> case * case list 

(* Prend en argument une case et en renvoie ses coordonnees *)
val coord_of_case : case -> int * int

(* Affiche les coordonnées d'une case *)
val pr_coord : case -> unit

(* Affiche les coordonnées des cases d'un tableau de case *)
val pr_case_array_coord : case array -> unit 

(* Fonction qui prend en argument un tableau 2D de cases et l'affiche *)
val print : case array array -> unit

(* Fonction qui prend en argument une grille et l'affiche de manière plus élégante *)
val print_pretty : grille -> unit

(* Prend en argument un fichier (d'un labyrinthe) et en affiche la suite de coordonnees correspondant 
   au chemin gagnant du labyrinthe *)
val pr_chemin_solved : string -> unit 

(* Affiche le labyrinthe une fois resout, avec parametre 'pretty'
   le faisant afficher -ou non- de maniere elegante *)
val pr_grid_chemin_solved : string -> bool -> unit 

(* Prend en argument un fichier (d'un labyrinthe) et en affiche la suite de coordonnees correspondant 
   au chemin gagnant du labyrinthe, de maniere plus elegante*)
val pr_grid_chemin_solved_pretty : string -> unit 

(* Renvoie la case de la grille dont les coordonnées sont celles passées en arguments *)
val get : grille -> int -> int -> case



(* On a dû réécrire la fonction find_index du module Array comme la version OCaml
   sur les machines n'était pas à jour (version ancienne) *)
val find_index : ('a -> bool) -> 'a array -> int 

(* Prend en argument une grille valide et renvoie les coordonnées
   de la sortie S *)
val find_S : grille -> int * int

(* Renvoie la liste des cases voisines (dans l'ordre nord-est-sud-ouest)
   dans les directions respectives *)
val cases_voisines : grille -> case -> case list

(* Renvoie un booléen selon qu'une case soit déjà visitée ou non
   (présence dans la liste c_liste) *)
val cases_visitees : case list -> case -> bool

(* Prend en argument le nom d'un fichier contenant un labyrinthe, et
   en renvoie son chemin gagnant et s'il n'existe pas, une liste vide *)
val solve : string -> case list


(*
(*Affiche un tableau de cases -> utilisée pour les tests*)
val affiche_c_array : case array -> unit

(*Renvoie la liste des cases accessibles autour de soi -> ne prend pas en compte les Mur*)
val voisin_de_voisin : grille -> case -> case list

(*Renvoie un tableau de toutes les cases Vide d'une grille -> utilisée pour placer les cases S et E*)
val cases_vides : grille -> case array



(* Place sur la grille les cases départ et arrivée en fonction des coordonées passées en argument *)
val case_S_E : grille -> int -> int -> int ->int -> grille

(* Génère une grille ayant toutes ses cases initialisées à Mur, la dimension de la grille est passée en argument *)
val creer_grille : int -> int -> grille


(* Génère un labyrinthe de manière aléatoire à partir de deux entiers correspondant à la taille de la grille voulue *)
val random : int -> int -> grille
*)