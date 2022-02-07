#use "topfind";;
#require "graphics";;
open Graphics;;


(*Déclaration des variables globales et fonction utilitaires*)

let refresh  = 0.05;; (*Période de rafraichissement de l'écran, peut potenitellement être augmenter en fonction du pc, cependant celle-ci permet un jeu jouable ni trop rapide ni trop lent*)

let max int1 int2 = 
  if int1 > int2 then int1 else int2;
;;

let min int1 int2 = 
  if int1 > int2 then int2 else int1;
;;

(*Modifie la liste en remplacant chaque élement par le résultat de f(init , élement)*)
let rec applique_fonction f init liste = match liste with 
  |[] -> []
  |t::q -> (f init t )::(applique_fonction f init q)
;;


(*Déclaration des types (les différents éléments du jeu)*)

(*Le type vaisseau définit l'élement que le joueur pourra déplacer*)
type vaisseau = {
  mutable coordonnee_x : int; 
  mutable bonus : string; (*Le nom du bonus que le vaisseau possède actuellement, le joueur ne peut pas changer de bonus en_cours, il doit l'utiliser avant*)
  mutable nb_bonus : int; (*Le nombre de charge du bonus actuellement posséder*)
  mutable stun : bool; (*Permet de savoir si le vaisseau est actuellement en état de stun ou non*)
  mutable debut_stun : float (* Date à laquelle le vaisseau à commencer à être stun*)
}
;;

(*Le type missile définit toute objet qui sera en mouvement sur l'écran autre que le vaisseau*)
type missile = {
  mutable x : int;
  mutable y : int;
  mutable detruit : bool; (*True si le missille est détruit et doit donc être effacer false sinon**)
  mutable size_of_x : int; (*Permet de définir la taille du missile / sa hitbox*)
  mutable size_of_y : int; (*Permet de définir la taille du missile / sa hitbox*)
  style : string; (*De quelle type de missile il s'agit, les types étant : rocher, alien, bonus_laser, basic, laser*)
  mutable propriete : bool; (*Un boolén servant de différente manière, pour alien il indique la direction dans laquelle on se déplace, pour la bombe si elle a deja explose depuis un tour ou pas*)
}
;;


(*Le type etat définit toutes les valeurs actuels des élements en jeux, le vaisseau, les missiles, les bonus, si on peut tirer, la vie , le niveau auquel on joue*)
type etat = {
  mutable my_vaisseau : vaisseau; 
  mutable liste_of_missile : missile list; (*Contient uniquement les missiles hostiles au joueur*)
  mutable liste_of_missile_ally : missile list; (*Contient uniquement les missiles alliés au joueur*)
  mutable liste_of_bonus : missile list; (*Contient uniquement les missiles de type bonus que le joueur peut ramasser*)
  mutable buttonup :bool; (*True si le joueur peut appuyer pour tirer, false sinon,  cette variable permetra d'empecher le joueur de rester appuyer*)
  mutable life : int; 
  mutable niveau : int;
}
;;



(*Fonction lié au vaisseau*)

(*Efface le vaisseau*)
let erase_v vaisseau =
  set_color black; 
  fill_rect vaisseau.coordonnee_x 10 50 5;
  fill_rect (vaisseau.coordonnee_x+10) 15 30 10;
  fill_rect (vaisseau.coordonnee_x+22) 25 6 5;
;;
(*Affiche le vaisseau*)
let create_v vaisseau = 
  set_color blue;
  if vaisseau.bonus = "laser" then set_color magenta;
  if vaisseau.bonus = "bombe" then set_color green;
  if vaisseau.stun = true then set_color red;
  fill_rect vaisseau.coordonnee_x 10 50 5;
  fill_rect (vaisseau.coordonnee_x+10) 15 30 10;
  fill_rect (vaisseau.coordonnee_x+22) 25 6 5;

;;

let init_vaisseau =
   let my_vaisseau = {coordonnee_x = 125; bonus = "none"; nb_bonus = 0; stun = false; debut_stun = 0.} in
    my_vaisseau;
;;

(*Vérifie si le joueur doit être déstun, le stun dure 1 seconde*)
let actualisation_stun vaisseau =
  if ((Sys.time()) -. vaisseau.debut_stun ) > 1. then begin vaisseau.stun <- false; create_v vaisseau; synchronize() end;
;;

(*Actualise la position du vaisseau si celui_ci n'est pas stun, le déplacement dépend de la touche appuyé*)
let evolution_vaisseau vaisseau  touche =
  if vaisseau.stun = false then begin
    if touche = 'q' then begin
    erase_v vaisseau;
      vaisseau.coordonnee_x <- max 0 (vaisseau.coordonnee_x-15);
      create_v vaisseau
    end
    else if touche = 'd' then begin
      erase_v vaisseau;
      vaisseau.coordonnee_x <- min 300 (vaisseau.coordonnee_x + 15);
      create_v vaisseau
    end;
  end;
  synchronize();
;;

(*Vérifie si un missile est rentrer en colision avec le vaisseau*)
let colision_vaisseau missile vaisseau = 
  if missile.y > 27 then false (*Si au dessus de la hauteur du vaisseau (cas très probable) inutile de regarder *)
  else if (missile.x >= vaisseau.coordonnee_x  && missile.x <= (vaisseau.coordonnee_x + 50)) then true  
  else if ((missile.x + missile.size_of_x) >= vaisseau.coordonnee_x && (missile.x + missile.size_of_x) <= (vaisseau.coordonnee_x + 50)) then true else false
;;

(*fonction lié au missile*)

(*Efface le missile*)
let erase_m missile =
  set_color black; 
  fill_rect missile.x missile.y missile.size_of_x missile.size_of_y;
;;

(*Affiche le missile en fonction de son type*)
let create_m missile = 
  set_color red;
  if missile.style = "rocher"  then fill_circle ((missile.x + missile.x + missile.size_of_x)/2) ((missile.y + missile.y + missile.size_of_y)/2) (missile.size_of_y/2); 
  if missile.style = "basic"  then begin set_color blue; fill_rect missile.x missile.y missile.size_of_x missile.size_of_y end;
  if missile.style = "laser"|| missile.style  = "bonus_laser" then begin set_color magenta; fill_rect missile.x missile.y missile.size_of_x missile.size_of_y end;
  if missile.style = "bombe" || missile.style = "bonus_bombe" then begin set_color green; fill_circle ((missile.x + missile.x + missile.size_of_x)/2) ((missile.y + missile.y + missile.size_of_y)/2) (missile.size_of_y/2) end;
  if missile.style = "alien" then begin fill_rect (missile.x + 9) missile.y 6 3;fill_rect (missile.x + 18) missile.y 6 3;fill_rect missile.x  (missile.y + 3) 3 9;fill_rect (missile.x + 6) (missile.y + 3) 3 15;
  fill_rect (missile.x + 24) (missile.y + 3) 3 15;fill_rect (missile.x + 30) (missile.y + 3) 3 9;fill_rect (missile.x + 9) (missile.y + 6) 3 6;fill_rect (missile.x + 12) (missile.y + 6) 3 12;
  fill_rect (missile.x + 15) (missile.y + 6) 3 12;fill_rect (missile.x + 18) (missile.y + 6) 3 12;fill_rect (missile.x + 21) (missile.y + 6) 3 6;fill_rect (missile.x + 3) (missile.y + 9) 3 6;
  fill_rect (missile.x + 27) (missile.y + 9) 3 6;fill_rect (missile.x + 9) (missile.y + 15) 3 6;fill_rect (missile.x + 21) (missile.y + 15) 3 6;fill_rect (missile.x + 6) (missile.y + 21) 3 3;
  fill_rect (missile.x + 24) (missile.y + 21) 3 3 end;
;;



(* Gerer la taille / là où rebondit / l'endroit d'aparition des missiles ennemies
la propabilité d'apparition d'un missile dépend du niveau actuel auquel on joue, plus le niveau est élevé plus l'appartion d'un missile ennemie est probable.*)
let spawn_random_m etat  = 
(*apparition des rochers*)
  if ((Random.int 50) /etat.niveau) = 1 then begin 
    let my_missile = {x = Random.int 328; y = 780; detruit = false; size_of_x = 20; size_of_y = 15; style = "rocher"; propriete = true} in
    create_m my_missile;
    etat.liste_of_missile <- my_missile::etat.liste_of_missile
  end;
(*apparition des aliens, seulement à partir du niveau 3*)
  if (etat.niveau > 2 && ((Random.int 100) / etat.niveau) = 1) then begin 
    let my_missile = {x = Random.int 307; y = 777; detruit = false; size_of_x = 33; size_of_y = 27; style = "alien"; propriete = Random.bool()} in
    create_m my_missile;
    etat.liste_of_missile <- my_missile::etat.liste_of_missile
  end;
;;

(* Gerer la taille / là où rebondit / l'endroit d'aparition des bonus, les bonus apparaissent seulement à partir du niveau 4*)
let spawn_bonus etat =
  if (etat.niveau > 3 && (((Random.int 250 ) /etat.niveau)+1) = 1) then begin
    if (Random.int 2) = 1 then begin
      let my_missile = {x = Random.int 328; y = 780; detruit = false; size_of_x = 20; size_of_y = 15; style = "bonus_laser"; propriete = true} in
      create_m my_missile;
      etat.liste_of_bonus <- my_missile::etat.liste_of_bonus
    end
    else let my_missile = {x = Random.int 328; y = 780 ; detruit = false; size_of_x = 30 ; size_of_y = 30; style = "bonus_bombe"; propriete = true} in
      create_m my_missile;
      etat.liste_of_bonus <- my_missile::etat.liste_of_bonus
  end;
;;


(*Permet de tester si deux missiles sont en contact par rapport à leurs position x*)
let tester_x missile1 missile2 =
  if (missile2.x <= missile1.x && missile1.x <= missile2.x + missile2.size_of_x) then true 
  else 
    if (missile2.x <= missile1.x + missile1.size_of_x  && missile2.x >= missile1.x) then true else false 
;;

(*Permet de tester si deux missiles sont en contact par rapport à leurs position y*)
let tester_y missile1 missile2 =
  if (missile2.y <= missile1.y && missile1.y <= missile2.y + missile2.size_of_y) then true 
  else 
    if (missile2.y <= missile1.y + missile1.size_of_y  && missile2.y >= missile1.y) then true else false 
;;

(*Permet de tester la colision entre deux missiles, on regarder si leurs valeurs x et y se croisent. 
Dans ce cas les missiles sont détruit (sauf le cas particulier du laser)*)
let colision_deux_missile missile1 missile2 =
  if (tester_x missile1 missile2 && tester_y missile1 missile2) then begin
    erase_m missile2;missile2.detruit <- true;
    if (missile1.style <> "laser" && missile1.style <> "bombe") then begin erase_m missile1; missile1.detruit <- true end;
    (*Si le missile est une bombe et qu'il n'a pas déjà explosé (vérifié par la la taille qu'elle a, alors on la fati explosé)*)
    if (missile1.style = "bombe" && missile1.size_of_x != 200 )then begin missile1.x <- (missile1.x -90); missile1.size_of_x <- 200;missile1.y <- (missile1.y -90); missile1.size_of_y <- 200 end
    
  end;
  missile2;
;;


(*Teste la colision entre tous les missiles de deux listes*)
(*Renvoyer la liste 2 n'est pas nécessaire l'interert et juste d'affecter true ou rien a chaque missile.*)
let rec colision_liste liste1 liste2 = match liste1 with
  |[] -> ()
  |t::q -> colision_liste q (applique_fonction colision_deux_missile t liste2)
;;


let deplacer_rocher missile etat = 
  (*Si colision avec le vaisseau, celui-ci est stun et le rocher disparait*)
  if colision_vaisseau missile etat.my_vaisseau then begin etat.my_vaisseau.stun <- true; etat.my_vaisseau.debut_stun <- (Sys.time()); missile.detruit <- true; create_v etat.my_vaisseau end
  else begin 
    missile.y <- missile.y - missile.size_of_y + 5; 
    if missile.y >= 0 then create_m missile else begin missile.detruit <- true; etat.life <- max (etat.life - 10) 0 end; (*Si le rocher touche le sol la vie descend*)
  end;
;;

let deplacer_alien missile etat =
  (*Si colision avec le vaisseau, celui-ci est stun et l'alien disparait*)
  if colision_vaisseau missile etat.my_vaisseau then begin etat.my_vaisseau.stun <- true;etat.my_vaisseau.debut_stun <- (Sys.time());missile.detruit <- true; create_v etat.my_vaisseau end
  else begin
    if missile.propriete then 
      if missile.x > 0 then missile.x <- (missile.x - 15) else begin missile.x <- (missile.x + 15); missile.propriete <- false end 
    else 
      if missile.x < 307 then missile.x <-(missile.x + 15) else begin missile.x <- (missile.x -15); missile.propriete <- true end;
    
    missile.y <- missile.y - 5;
    if missile.y >= 0 then create_m missile else begin missile.detruit <- true; etat.life <- max (etat.life - 10) 0 end; (*Si l'alien touche le sol la vie descend*)
    end;
;;

let deplacer_basic missile etat =
  missile.y <- missile.y + missile.size_of_y;
  if missile.y <= 790  then create_m missile else missile.detruit <- true;
;;

let deplacer_bonus missile etat =
  missile.y <- missile.y - 15;
  (*Si le vaisseau récupère le bonus, il gagne le bonus associé*)
  if colision_vaisseau missile etat.my_vaisseau then begin
    missile.detruit <- true;
    (*On vérifie que le bonus récupérer est le même que celui actuel ou que aucun bonus ne sont en possession*)
    if ((missile.style = "bonus_laser" && etat.my_vaisseau.bonus = "laser") || (missile.style = "bonus_laser" && etat.my_vaisseau.bonus = "none")) then begin
      etat.my_vaisseau.bonus <- "laser";etat.my_vaisseau.nb_bonus <- (etat.my_vaisseau.nb_bonus + 1); erase_v etat.my_vaisseau; create_v etat.my_vaisseau end
    else
      if (missile.style = "bonus_bombe" && etat.my_vaisseau.bonus = "bombe") || (missile.style = "bonus_bombe" && etat.my_vaisseau.bonus = "none") then begin
      etat.my_vaisseau.bonus <- "bombe";etat.my_vaisseau.nb_bonus <- (etat.my_vaisseau.nb_bonus + 1); erase_v etat.my_vaisseau; create_v etat.my_vaisseau end
  
  end
else if missile.y >= 0  then create_m missile else missile.detruit <- true;
  
;;

let deplacer_laser missile etat = 
  (*Le laser ne disparait qu'une fois après avoir traversé entièrement l'écran*)
  missile.y <- missile.y + 40;
  if missile.y <= 800  then create_m missile else missile.detruit <- true;
;;

let deplacer_bombe missile etat =
  (*Si la bombe a explosé ET passer un tour de boucle (vérifier par propriete) alors on l'efface*)
  if missile.propriete = true then begin erase_m missile; missile.detruit <- true; create_v etat.my_vaisseau end
  else begin 
  (*Si la bombe a explosé seulement alors on l'afficher et on inidque qu'elle a fait un tour de boucle*)
    if missile.size_of_y = 200 then begin create_m missile; missile.propriete <- true end
    else begin
      missile.y <- missile.y + 15;
      if missile.y <= 770 then create_m missile else missile.detruit <- true
    end
  end;
;;

(*Fait évoluer l'état d'un missile quelconque à l'aide des fonctions précedentes*)
let evoluer_un_missile missile etat = 
  erase_m missile;
  if missile.style = "rocher" then deplacer_rocher missile etat;
  if missile.style = "alien" then deplacer_alien missile etat;
  if missile.style = "basic" then deplacer_basic missile etat;
  if missile.style = "bombe" then deplacer_bombe missile etat;
  if missile.style = "bonus_laser" then deplacer_bonus missile etat;
  if missile.style = "bonus_bombe" then deplacer_bonus missile etat;
  if missile.style = "laser" then deplacer_laser missile etat;
  missile;
;;




(*Fait l'actualisation de tous les missiles d'une liste de missiles, si celui-ci sont détruit ont les retire de la liste*)
let rec evolution_liste_missile liste_missile  etat = match liste_missile with
  |[] -> []
  |first_missile::suite_missile when first_missile.detruit = false  -> (evoluer_un_missile first_missile etat)::(evolution_liste_missile suite_missile etat)
  |first_missile::suite_missile  -> (evolution_liste_missile suite_missile etat)
;;

(*Fait apparaitre le missille tiré par le vaisseau en fcontion du bonus actuel, celui-ci à des propriétés différentes.*)
let apparition_missile_ally etat = 
  if etat.my_vaisseau.bonus = "none" then
    etat.liste_of_missile_ally <-  {x = etat.my_vaisseau.coordonnee_x + 22; y = 30 ; detruit = false; size_of_x = 6; size_of_y = 10; style = "basic"; propriete = true}::etat.liste_of_missile_ally
  else begin
    if etat.my_vaisseau.bonus = "laser" then 
    etat.liste_of_missile_ally <-  {x = etat.my_vaisseau.coordonnee_x ; y = 30 ; detruit = false; size_of_x = 60 ; size_of_y = 800; style = "laser"; propriete = true}::etat.liste_of_missile_ally
    else
    etat.liste_of_missile_ally <-  {x = etat.my_vaisseau.coordonnee_x + 15; y = 30 ; detruit = false; size_of_x = 20; size_of_y = 20; style = "bombe"; propriete = false}::etat.liste_of_missile_ally;

    etat.my_vaisseau.nb_bonus <- (etat.my_vaisseau.nb_bonus -1) ;if etat.my_vaisseau.nb_bonus = 0 then etat.my_vaisseau.bonus <- "none"; erase_v etat.my_vaisseau; create_v etat.my_vaisseau
  end;    
;;



(*Fonction lié à la barre de vie*)
let afficher_life life = 
  set_color black;
  fill_rect 0 0 350 10;
  set_color green;
  fill_rect 0 0 life 10;
;;

(*fonction lié à l'état*)
let init_etat = 
  let my_etat = {my_vaisseau = init_vaisseau;  liste_of_missile = [];liste_of_missile_ally = []; liste_of_bonus =[]; buttonup = true; life = 350; niveau = 8} in
    my_etat;
;;

(*Fait évoluer le niveau en fonction du temps de jeu*)
let evoluer_niveau etat temps_init temps_actuel =
  let temps = int_of_float(temps_actuel-. temps_init)in
    if temps > 20*etat.niveau && etat.niveau < 10 then etat.niveau <- etat.niveau + 1
;;

let evolution_etat_jeu etat temps_init temps_actuel  =
  (*On fati évoluer les différentes listes de missiles*)
  colision_liste etat.liste_of_missile_ally etat.liste_of_missile; (*La réaffectation n'est pas nécessaire car les listes seront réduites de tailles quand elles évolueront*)

  etat.liste_of_missile <- (evolution_liste_missile etat.liste_of_missile etat );
  etat.liste_of_missile_ally <- (evolution_liste_missile etat.liste_of_missile_ally etat);
  etat.liste_of_bonus <- (evolution_liste_missile etat.liste_of_bonus etat);

  (*On fati apparaître de nouveaux bonus / ennemis*)
  spawn_random_m etat;
  spawn_bonus etat;
  (*On affiche la barre de vie et modifie le niveau si necéssaire*)
  afficher_life etat.life;
  evoluer_niveau etat temps_init temps_actuel;
  
  synchronize();
;;




(*Fonction lié au jeu*)
let afficher_score temps_final temps_init = 
  let temps = string_of_int(int_of_float(temps_final -. temps_init))in
    set_color white;
    fill_rect 115 340 110 25;
    set_color black;
    moveto 120 350;
    draw_string "Vous avez survecu";
    moveto 145 340;
    draw_string temps;
    rmoveto 5 0;
    draw_string "secondes";
    synchronize();
;;

(*Fonction lié au graphe*)
(*N'a pas pu être beaucoup dévellopé car visual studio code à un pb avec la gestion du texte / image avec graphics*)

(*Affiche l'image d'intro*)
let initialiser_graphe_intro =  
  open_graph " 350x800";
  set_color black; 
  fill_rect 0 0 350 800;
  set_color magenta;
  moveto 135 600;
  draw_string "Space Invader 2";
  moveto 15 400;
  draw_string "You have to protect the Earth from asteroid and alien";
  moveto 40 350;
  draw_string " left : Q     right : D     Shoot : Leftclick";
  moveto 85 275;
  draw_string "Leftclick to start the Game";
  auto_synchronize false;
;;

(*Initialise le graphe de jeu*)
let initialiser_graphe  vaisseau = 
  open_graph " 350x800";
  set_color black; 
  fill_rect 0 0 350 800;
  create_v vaisseau;
  auto_synchronize false;

;;

(*Code princiapl*)
let jeuEnCours = ref true and toucheAppuyee = ref false and touche = ref ' ' and etat = init_etat and temps_init = Sys.time() and temps = ref (Sys.time()) in

 

  (*On attend que le joueur confirme le début du jeu*)
  while etat.buttonup = true do
    if (button_down() = true) then etat.buttonup <- false
  done;

  initialiser_graphe etat.my_vaisseau;
  synchronize();
  

  while !jeuEnCours do
    
    (*Vérification à chaque tour du déplacement du joueur pour des déplacements fluides*)
    toucheAppuyee := key_pressed();
    if !toucheAppuyee then begin
      touche := read_key();
      evolution_vaisseau etat.my_vaisseau !touche
    end;
    


    (*Seulement lorsque le temps écoulé depuis la dernière boucle est plus grande que la période de refresh près_définit, on actualise tout le jeu*)
    (*En effet cela permet de ne pas actualisé le jeu trop vite et de le rendre injouable*)
    if ((Sys.time() -. !temps) > refresh) then begin
      (*Si le joueur à bien relaché le bouton depuis le dernier tour de boucle, il pourra à nouveau tiré*)
      if (button_down() && etat.buttonup = true) then begin
        etat.buttonup <- false;
        apparition_missile_ally etat end
      else  if (button_down() = false) && etat.buttonup = false then etat.buttonup <- true;
      
      (*actualisation du stun du vaisseau*)
      if etat.my_vaisseau.stun = true then actualisation_stun etat.my_vaisseau;
      (*actualisation générale du jeu*)
      evolution_etat_jeu etat temps_init (Sys.time());
      temps := Sys.time ()
    end;



    if (etat.life = 0) then jeuEnCours := false;

  done;
  
  afficher_score (Sys.time ()) temps_init;
;;
