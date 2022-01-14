#use "topfind";;
#require "graphics";;
#require "unix";;

open Graphics;;
open Unix;;
 

(*Projet OCaml de Nathan RAPIN et Nicolas SEGALA. *)

(*Le jeu se joue à la souris.*)

let hauteur=720 and largeur=1280;;
open_graph "";resize_window largeur hauteur;;
 
(*Ces variables serviront à contrôler l'affichage du jeu*)
let t = ref  (Unix.gettimeofday () );;
let dt = ref (1./.60.);;
 
let pi = acos(-1.);;
 
(*TYPES*)
 
(*Types outils*)
 
(*Définition de types outils.*)
type point = {
  x : float;
  y : float;
};;
 
type vecteur = {
  mutable vx : float;
  mutable vy : float;
};;
 
(*Types objets*)
 
(*Les points bleus provoquent des collisions mortelles qui font recommencer le niveau.*)
(*Ils peuvent être en rotation ou en translation.*)
type nature = | Rotation | Translation ;;
type point_bleu = { mutable p_pos : vecteur ; mutable p_vit : vecteur ; p_r : float ; nature_mouvement : nature ; mutable caracterisation_mouvement : point * point ; vitesse_deplacement : float};;
(*Si le point est en Rotation, les points de caracterisation_mouvement correspondent au centre de rotation du point et à un point du cercle (caractérise le rayon).
Si le point est Translation, les deux points définissent le segment sur lequel se déplace le point *)
 
(*Le carré rouge est le joueur.*)
type carre_rouge = {mutable c_pos : vecteur ; mutable c_vit : vecteur ;  c_r : float};;
 
(*Les pièces jaunes doivent être ramassées pour gagner.*)
type piece_jaune = { piece_pos : vecteur ; piece_r : float ; mutable piece_prise : bool};;
 
(*Les murs délimitent la zone de déplacement du joueur*)
type mur = { xmin : float; xmax : float ; ymin : float ; ymax : float};;
 
 
(*FONCTIONS OUTILS*)
(*Ces fonctions servent à créer et manipuler les vecteurs de position et de vitesse des objets*)
let add_vecteur vect1 vect2 = {vx = vect1.vx +. vect2.vx ; vy = vect1.vy +. vect2.vy };;
let oppose_vecteur vect = {vx = -.vect.vx; vy = -.vect.vy};;
let substract_vecteur vect1 vect2 = add_vecteur vect1 (oppose_vecteur vect2);;
let produit_scalaire vect1 vect2 = vect1.vx *. vect2.vx +. vect1.vy *. vect2.vy;;
let direction vect1 vect2 = if produit_scalaire vect1 vect2 > 0. then 1. else -.1.;;
let norme vect = sqrt(produit_scalaire vect vect);;
let unitaire vect = let facteur = norme vect in { vx=vect.vx /. facteur; vy = vect.vy /.facteur;};;
let scale_vecteur_bord vect norme = let u = unitaire vect in vect.vx <- (u.vx *. norme) ; vect.vy <- (u.vy *. norme);;
let scale_vecteur vect norme = let u = unitaire vect in { vx=u.vx *. norme; vy = u.vy *. norme;};;
let vecteur_normal vect = { vx = vect.vy ; vy = -. vect.vx};;
let make_vect_vitesse_normal vect_position norme_v = let normal = vecteur_normal vect_position in  scale_vecteur normal norme_v;;
let make_point x y = { x = x; y = y };;
let make_vecteur point1 point2 = { vx = (point2.x -. point1.x) ; vy = (point2.y -. point1.y)};;
let vecteur_position point1 = make_vecteur (make_point 0. 0. ) point1;;
let vecteur_position_xy x y = make_vecteur (make_point 0. 0.) (make_point x y);;
let point_correspondant vecteur_pos1 = make_point vecteur_pos1.vx vecteur_pos1.vy;;
let vecteur_nul = make_vecteur (make_point 0. 0.) (make_point 0. 0.);;
 
(*Ces références servent à l’affichage*)
let origine = ref ({ x = (float_of_int (size_x()/2)); y=(float_of_int (size_y()/2));});;
let zoom = ref ((min !origine.x !origine.y)/.50.) ;;
 
 
(*FONCTIONS D'AFFICHAGE*)
 
(*Cette fonction prend en argument un vecteur position, un rayon et des couleurs et affiche le cercle correspondant à l’écran. 
Elle est utilisée pour représenter les points bleus et pièces jaunes.*)
let affiche_cercle vect r couleur_interieur couleur_bord =
  let x,y,r = int_of_float(!origine.x +. vect.vx *. !zoom), int_of_float (!origine.y +. vect.vy *. !zoom), int_of_float (r *. !zoom) in
  set_color couleur_interieur;
  fill_circle x y r;
  set_color couleur_bord;
  draw_circle x y r;
();;
 
(*Cette fonction prend en argument un vecteur position, des dimensions et des couleurs, puis affiche le carré correspondant.
Elle est utilisée pour afficher le joueur, un carré rouge, et les murs rectangulaires.*)
let affiche_carre vect largeur hauteur couleur_interieur couleur_bord =
  let x,y,l,h = int_of_float(!origine.x +. vect.vx *. !zoom), int_of_float (!origine.y +. vect.vy *. !zoom), int_of_float (largeur *. !zoom), int_of_float (hauteur *. !zoom) in
 
  set_color couleur_interieur;
  fill_rect x y l h;
  set_color couleur_bord;
  draw_rect x y l h;
  ();;
(*Cette fonction prend un argument un tableau de points bleus, et les affiche à l’aide de la fonction affiche_cercle*)
let affiche_points_bleus array_points_bleus =
  set_color blue;
  for i=0 to (Array.length array_points_bleus)-1 do
     affiche_cercle array_points_bleus.(i).p_pos array_points_bleus.(i).p_r blue black;
  done;
;;
 
(*Cette fonction prend en argument un tableau de murs et les affiche à l’aide de la fonction affiche_carre*)
let affiche_murs array_murs =
  let extract_coordinates mur = (mur.xmin,mur.xmax,mur.ymin, mur.ymax) in
  for i = 0 to (Array.length array_murs)-1 do
     let xmin,xmax,ymin,ymax= extract_coordinates array_murs.(i) in
     let vect,largeur,hauteur = vecteur_position (make_point xmin ymin) , xmax -. xmin, ymax -. ymin in
     affiche_carre vect largeur hauteur (rgb 127 127 127) black;
  done;
;;
(*Cette fonction prend en argument le joueur et l’affiche à l’aide de la fonction affiche_carre*)
let affiche_carre_rouge carre_rouge = affiche_carre carre_rouge.c_pos carre_rouge.c_r carre_rouge.c_r red black;;
 
 
 
 
 
(*FONCTIONS MOUVEMENTS*)
(*La fonction update prend en argument un vecteur position et un vecteur vitesse.
Elle renvoie la somme des deux, le nouveau vecteur position*)
let update vect_position vect_vitesse = add_vecteur vect_position vect_vitesse;;
(*La fonction rotation prend en argument un point bleu et actualise sa position et sa vitesse.
Grâce à la caractérisation du mouvement stockée dans le point bleu, on peut calculer un vecteur vitesse à ajouter au vecteur position pour faire tourner le point.*)
let rotation pointbleu =
 let vitesse_angulaire = pointbleu.vitesse_deplacement in
 let centre , point_quelconque = pointbleu.caracterisation_mouvement in
 let vect_position , vect_centre = pointbleu.p_pos , vecteur_position centre in
 let vecteur_difference = (substract_vecteur vect_position vect_centre) in
 let rayon = norme (make_vecteur centre point_quelconque) in
 let vect_vitesse = make_vect_vitesse_normal vecteur_difference (vitesse_angulaire *.rayon) in
 let pos = update vecteur_difference vect_vitesse in
 pointbleu.p_pos <- add_vecteur (scale_vecteur pos rayon) vect_centre;
 pointbleu.p_vit <- vect_vitesse;
;;
 
(*La fonction translation prend en argument un point bleu et actualise sa position et sa vitesse.
Grâce à la caractérisation du mouvement stockée dans le point bleu, on peut calculer un vecteur vitesse à ajouter au vecteur position pour faire se déplacer le point.*)
let translation pointbleu =
 let vitesse_translation = pointbleu.vitesse_deplacement in
 let borne1 , borne2 = pointbleu.caracterisation_mouvement in
 let vecteur_translation = make_vecteur borne1 borne2 in
 let position = point_correspondant pointbleu.p_pos in
 let vecteur_vitesse = ref (make_vecteur position borne2) in
 if direction !vecteur_vitesse vecteur_translation = 1. then
   begin
      vecteur_vitesse := scale_vecteur !vecteur_vitesse (min vitesse_translation (norme !vecteur_vitesse));
   end
 else
   begin
        pointbleu.caracterisation_mouvement<-(borne2,borne1);
   end;
 pointbleu.p_pos <- update (pointbleu.p_pos) (!vecteur_vitesse);
 pointbleu.p_vit <- (!vecteur_vitesse);
;;
 
(*Cette fonction prend en argument un tableau de points bleus et actualise leurs position en prenant en compte la nature de leur mouvement*)
let update_all_points_bleus ensemble_points_bleus =
 for i = 0 to Array.length(ensemble_points_bleus)-1 do
   let point_en_question = ensemble_points_bleus.(i) in
   if point_en_question.nature_mouvement = Rotation then
     begin
       rotation point_en_question;
     end
   else
     begin
       translation point_en_question;
     end;
 done;
;;
  
(*Cette fonction vérifie s’il y a une collision entre le joueur et les points bleus.
Si il y a collision, on renvoie true, afin de déclencher des actions dans le programme principal.*)
let collision_mortelle joueur ensemble_points_bleus =
   let i = ref 0 and obstacle_rencontre = ref false in
   while !i < Array.length(ensemble_points_bleus) && not(!obstacle_rencontre) do
       let point_concerne = ensemble_points_bleus.(!i) in
       let vect_centre_hitbox = vecteur_position (make_point (joueur.c_r/.2.) (joueur.c_r/.2.) ) in
       if norme (substract_vecteur (add_vecteur joueur.c_pos vect_centre_hitbox) point_concerne.p_pos) < (joueur.c_r/.2. +. point_concerne.p_r) then obstacle_rencontre := true;
       i := !i + 1;
   done;
!obstacle_rencontre;;
 
(*Une fonction outil qui permet de facilement créer un mur.*)
let make_mur a b c d = {xmin = a; xmax = b ; ymin = c ; ymax = d};;
 
let collision_mur joueur decor =
 for i = 0 to Array.length(decor)-1 do
    let mur_concerne = decor.(i) and point_joueur = point_correspondant joueur.c_pos in
    let distance_droite, distance_gauche, distance_bas, distance_haut = point_joueur.x -. mur_concerne.xmax , point_joueur.x -. mur_concerne.xmin +. joueur.c_r, point_joueur.y -. mur_concerne.ymin +. joueur.c_r, point_joueur.y -. mur_concerne.ymax in
    if (distance_droite) < 0. && (distance_gauche) > 0. && (distance_haut) < 0. && distance_bas > 0. then
        begin
		(* On inverse la vitesse pour la composante adéquate *)
           let vecteur_redirection = {vx = 0. ; vy = 0.} in
           if (abs_float distance_droite) > (abs_float distance_gauche) then vecteur_redirection.vx <- (-.distance_gauche) else vecteur_redirection.vx <- (-.distance_droite);
 
           if (abs_float distance_haut) > (abs_float distance_bas) then vecteur_redirection.vy <- (-.distance_bas) else vecteur_redirection.vy <- (-.distance_haut);
          (* On inverse la vitesse uniquement pour la composante qui a produit le contact : cela permet de se déplacer en longeant le mur, entre autres *)
           if abs_float(vecteur_redirection.vx) > abs_float(vecteur_redirection.vy) then
              begin
                 vecteur_redirection.vx <- 0.;
              end
           else
              begin
                 vecteur_redirection.vy <- 0.;
              end;
           (* On replace le joueur sur le bord *)
           joueur.c_pos <- update joueur.c_pos vecteur_redirection;
        end;
  done;
;;
 
 
(*Partie Point bleus*)
 
(*r est le rayon du point bleu
nature détermine si le point est en rotation ou translation
caracterisation est un couple de points qui sert a definir la rotation ou la translation
fact_omega est un flottant qui sert à définir la vitesse de rotation ou la position initiale dans le cas d'une translation*)
 
let position_relative_cercle centre_cercle rayon fact =
  let angle = fact *. 2. *. pi in
  {x = centre_cercle.x +. (rayon *. cos(angle)) ; y = centre_cercle.y +. (rayon *. sin(angle))}
;;
 
(* Cette fonction permet l’utilisation d’un facteur de position permettant d’initialiser où l’on veut le point sur le cercle *)
 
let make_point_bleu r nature caracterisation fact_position vitesse =
  let point1,point2 = caracterisation in
  if nature = Translation then
     begin
        let vect_pos = let vect_total = make_vecteur point1 point2 in add_vecteur (make_vecteur (make_point 0. 0.) (point1) ) (scale_vecteur vect_total (fact_position *. (norme vect_total) ) ) in
        let vect_vit = make_vecteur point1 point2 in
        {p_pos = vect_pos ; p_vit = vect_vit ; p_r = r ; nature_mouvement = nature ;  caracterisation_mouvement = caracterisation ; vitesse_deplacement = vitesse};
     end
  else
     begin
        let vect_pos = make_vecteur (make_point 0. 0.) (position_relative_cercle point1 (norme (make_vecteur point1 point2)) fact_position) in
        let vect_vit = let p = make_point 0. 0. in make_vecteur p p in
        {p_pos = vect_pos ; p_vit = vect_vit ; p_r = r ; nature_mouvement = nature ;  caracterisation_mouvement = caracterisation ; vitesse_deplacement = vitesse};
     end;
;;
 
let make_point_bleu_array n r tab_nature tab_caracterisation tab_fact_position tab_vitesse =
  let array_points_bleus = Array.make n (make_point_bleu 1. Translation ((make_point 0. 0.),(make_point 1. 0.)) 0. 1.) in
  for i = 0 to n-1 do
     array_points_bleus.(i) <- (make_point_bleu r tab_nature.(i) tab_caracterisation.(i) tab_fact_position.(i) tab_vitesse.(i));
  done;
  array_points_bleus
;;
 
let make_caracterisation x1 y1 x2 y2 = (make_point x1 y1 , make_point x2 y2);;
    
 
 
 
(*Partie carre rouge*)
let celerite_carre = ref 1.;;
 
let make_carre_rouge pos r = {c_pos = pos ; c_vit = vecteur_nul; c_r = r} ;;

(*Cette fonction met à jour la position du joueur en lui faisant suivre la souris*)
let update_joueur carre_rouge =
  let mouse_x_raw, mouse_y_raw = mouse_pos() in
   (* On ajuste la position de la souris pour centrer sur le carré *)
  let mouse_x, mouse_y = ((float_of_int mouse_x_raw) -. !origine.x) /. !zoom -. carre_rouge.c_r/.2., ((float_of_int mouse_y_raw) -. !origine.y) /. !zoom -. carre_rouge.c_r/.2. in
  let mouse_vect = vecteur_position (make_point mouse_x mouse_y)in
  let to_mouse_vector = substract_vecteur mouse_vect carre_rouge.c_pos in
  let vect_vitesse = scale_vecteur to_mouse_vector !celerite_carre in
  if norme vect_vitesse < norme to_mouse_vector then let new_pos = update carre_rouge.c_pos vect_vitesse in carre_rouge.c_pos<- new_pos ;;
 
 
 
 
(*Partie pieces jaunes*)
 
let affiche_pieces_jaunes piece_jaune_array =
   set_color yellow;
   for i=0 to (Array.length piece_jaune_array)-1 do
     if not piece_jaune_array.(i).piece_prise then
      affiche_cercle piece_jaune_array.(i).piece_pos piece_jaune_array.(i).piece_r yellow black;
   done;
;;
 let ramasse_pieces_jaunes joueur pieces_jaunes =
  for i = 0 to Array.length(pieces_jaunes) - 1 do
   let vect_centre_hitbox = vecteur_position (make_point (joueur.c_r/.2.) (joueur.c_r/.2.) ) in
    if norme (substract_vecteur (add_vecteur vect_centre_hitbox joueur.c_pos)  pieces_jaunes.(i).piece_pos) < (joueur.c_r +. pieces_jaunes.(i).piece_r)/.2. then pieces_jaunes.(i).piece_prise<-true;
  done;
;;
 
let reset_pieces_jaunes pieces_jaunes =
 for i = 0 to Array.length(pieces_jaunes) - 1 do
   pieces_jaunes.(i).piece_prise<- false;
   done;
;;
let make_piece_jaune r pos  =
{piece_pos = pos ; piece_r = r ; piece_prise = false};
;;
 let make_piece_jaune_array n r tab_position =
 let array_piece_jaune = Array.make n (make_piece_jaune 1. vecteur_nul) in
 for i = 0 to n-1 do
    array_piece_jaune.(i) <- (make_piece_jaune r tab_position.(i));
 done;
 array_piece_jaune;
;;
 
(*Partie zone verte/ zone finale*)
 
let affiche_zone_verte zone_verte =
 let extract_coordinates zone_verte = (zone_verte.xmin,zone_verte.xmax,zone_verte.ymin, zone_verte.ymax) in
     let xmin,xmax,ymin,ymax= extract_coordinates zone_verte in
     let vect,largeur,hauteur = vecteur_position (make_point xmin ymin) , xmax -. xmin, ymax -. ymin in
     affiche_carre vect largeur hauteur green black;;
 
let collision_zone_verte joueur zone_verte =
 let point_joueur = point_correspondant joueur.c_pos in
 let distance_droite, distance_gauche, distance_bas, distance_haut = point_joueur.x -. zone_verte.xmax +. joueur.c_r , point_joueur.x -. zone_verte.xmin, point_joueur.y -. zone_verte.ymin, point_joueur.y -. zone_verte.ymax +. joueur.c_r in
 if distance_droite < 0. && distance_gauche > 0. && distance_haut < 0. && distance_bas > 0. then true else false;
;;

(*Renvoie true si toutes les pièces sont récupérées et le joueur est dans la zone finale*)
let victoire joueur pieces_jaunes zone_finale =
 let a_toutes_les_pieces = ref true in
 for i = 0 to Array.length(pieces_jaunes) - 1 do
   if not(pieces_jaunes.(i).piece_prise) then
   begin
     a_toutes_les_pieces := false;
   end;
 done;
 !a_toutes_les_pieces && collision_zone_verte joueur zone_finale ;;

(*Ces refs servent a creer plus facilement notre niveau. Malheureusement elles introduisent aussi un problème qui déforme la configuration du niveau, pour des tailles de fenêtres peu communes.
Notre jeu fonctionne cependant bien en plein écran et pour des tailles de fenêtres raisonnables.*)
let cote_gauche =ref (-. !origine.x /. !zoom );;
let cote_droit =ref (!origine.x /. !zoom);;
let plafond =ref (!origine.y /. !zoom);;
let sol =ref (-. !origine.y /. !zoom);;
let largeur1 =ref (5.);;
let size_fenetre = ref (0);;
 
let main () =
  auto_synchronize false;
  set_color black;
  moveto (int_of_float !origine.x) (int_of_float !origine.y);
 
  celerite_carre := 0.5;
 
  (*Déclaration des ref qui contiendront les objets à afficher*)
  let zone_verte_debut = ref ( (make_mur (!cote_gauche +. !largeur1) (!cote_gauche +. 5.*. !largeur1) (!plafond -.5.*. !largeur1) (!plafond -. !largeur1))   ) in
  let zone_verte_fin = ref ( make_mur (!cote_gauche +. (4. *. !largeur1)) (!cote_gauche +. (6. *. !largeur1)) (!sol +. (6. *. !largeur1)) (!plafond -. (9. *. !largeur1)) ) in
  let position_spawn = ref ( vecteur_position_xy ((!zone_verte_debut.xmin +. !zone_verte_debut.xmax)/.2.) ((!zone_verte_debut.ymin +. !zone_verte_debut.ymax)/.2.)  ) in
  let joueur = ref ( make_carre_rouge (!position_spawn) 2. )
 
 and tab_pieces_jaunes = ref (  make_piece_jaune_array 4 1. [|vecteur_position_xy (5.) (5.); vecteur_position_xy (-50.) (5.); vecteur_position_xy (50.) (38.); vecteur_position_xy (50.) (38.)|] ) in
 
 
   let n = 1
   and r = 1.
   and nature_array = [|Translation|]
   and caract_array = [|make_caracterisation (!cote_gauche +. 6.*. !largeur1 ) (!plafond -. !largeur1) (!cote_gauche +. 6.*. !largeur1) (!plafond -. 5. *. !largeur1) |]
   and fact_pos_array = [| 0.|]
   and vitesse_array = [|1. |] in
 
   let array_points_bleus = ref ( make_point_bleu_array n r nature_array caract_array fact_pos_array vitesse_array )
  and array_points_bleus1 = ref ( make_point_bleu_array n r nature_array caract_array fact_pos_array vitesse_array )
  and array_points_bleus2 = ref ( make_point_bleu_array n r nature_array caract_array fact_pos_array vitesse_array )
  and array_points_bleus4 = ref ( make_point_bleu_array n r nature_array caract_array fact_pos_array vitesse_array )
 
  and array_points_bleus3 = ref ( make_point_bleu_array n r nature_array caract_array fact_pos_array vitesse_array ) in
 
(*Ces 4 tableaux correspondent aux 4 ensembles de points de même déplacement (les 2 cercles de droite, les points en translation et la croix tournante*)
 
 
 let environnement =ref (  [| make_mur (!cote_gauche) (!cote_gauche +. !largeur1) (!sol) (!plafond)|] ) in

 let pasgagne = ref true in

 while !pasgagne do
   (*On recalcule les positions des objets si la taille de la fenêtre change*)
     if size_x() * size_y() <> !size_fenetre then
        begin
           size_fenetre:= size_x() * size_y();
           origine :=  { x = (float_of_int (size_x()/2)); y=(float_of_int (size_y()/2));};
           zoom:= ((min !origine.x !origine.y)/.50.);
 
           cote_gauche := -. !origine.x /. !zoom ;
           cote_droit := !origine.x /. !zoom;
           plafond := !origine.y /. !zoom;
           sol := -. !origine.y /. !zoom;
           largeur1 := 5.;
 
           zone_verte_debut := (make_mur (!cote_gauche +. !largeur1) (!cote_gauche +. 5.*. !largeur1) (!plafond -.5.*. !largeur1) (!plafond -. !largeur1)) ;
           zone_verte_fin := make_mur (!cote_gauche +. (4. *. !largeur1)) (!cote_gauche +. (6. *. !largeur1)) (!sol +. (6. *. !largeur1)) (!plafond -. (9. *. !largeur1)) ;
 
 
            position_spawn := vecteur_position_xy ((!zone_verte_debut.xmin +. !zone_verte_debut.xmax)/.2.) ((!zone_verte_debut.ymin +. !zone_verte_debut.ymax)/.2.) ;
          
           tab_pieces_jaunes := make_piece_jaune_array 4 1. [|vecteur_position_xy (-13. *. !largeur1) (-5.5 *. !largeur1); vecteur_position_xy (-6. *. !largeur1) (-2. *. !largeur1); vecteur_position_xy (11.5 *. !largeur1) (-. !largeur1) ; vecteur_position_xy (0.) (7. *. !largeur1)|] ;
		
           let n= 16 in
           let r = 1.
           and nature_array1 = Array.make n Translation
                 and caract_array1 = let tab = Array.make 16 (make_caracterisation (!cote_gauche +. 6.*. !largeur1 ) (!plafond -. !largeur1) (!cote_gauche +. 6.*. !largeur1) (!plafond -. 5. *. !largeur1) )
                 in
                 for i=1 to 15 do
                   tab.(i)<- make_caracterisation (!cote_gauche +. (float_of_int (i+6)) *. !largeur1 ) (!plafond -. !largeur1) (!cote_gauche +. (float_of_int (i+6)) *. !largeur1) (!plafond -. 5. *. !largeur1);
                 done;
                 tab;
 
           and fact_pos_array1 = [| 0. ; 1.; 0. ; 1.; 0. ; 1.; 0. ; 1.; 0. ; 1.;0. ; 1.; 0. ; 1.; 0. ; 1.; 0. ; 1.; 0. ; 1.|]
           and vitesse_array1 = Array.make n 0.5 in
           array_points_bleus1 := make_point_bleu_array n r nature_array1 caract_array1 fact_pos_array1 vitesse_array1 ;
          
           let n = 9 in
           let nature_array2 = Array.make n Rotation
           and caract_array2 = Array.make n (make_caracterisation (11.5 *. !largeur1) (-. !largeur1) (11.5 *. !largeur1) (-. 2.*. !largeur1) )
           and fact_pos_array2 = [| 0. ; 0.1 ; 0.2 ; 0.3 ; 0.4 ; 0.5 ; 0.6 ; 0.7 ; 0.8 |]
           and vitesse_array2 = Array.make n 0.03 in
           array_points_bleus2 := make_point_bleu_array 9 r nature_array2 caract_array2 fact_pos_array2 vitesse_array2  ;
 
           let n = 9 in
           let nature_array4 = Array.make n Rotation
           and caract_array4 = Array.make n (make_caracterisation (11.5 *. !largeur1) (-. !largeur1) (11.5 *. !largeur1) (-. 3. *. !largeur1) )
           and fact_pos_array4 = [| 0. ; 0.1 ; 0.2 ; 0.3 ; 0.4 ; 0.5 ; 0.6 ; 0.7 ; 0.8 |]
           and vitesse_array4 = Array.make n (-0.03) in
           array_points_bleus4 := make_point_bleu_array 9 r nature_array4 caract_array4 fact_pos_array4 vitesse_array4  ;
 
 
           let nature_array3 = [| Rotation ; Rotation ; Rotation ; Rotation ; Rotation ; Rotation ; Rotation ; Rotation ; Rotation ; Rotation ; Rotation ; Rotation|]
           and caract_array3 = [|(make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-8. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-8. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-8. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-8. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-10. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-10. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-10. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-10. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-12. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-12. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-12. *. !largeur1) (-2. *. !largeur1)) ;
              (make_caracterisation (-6. *. !largeur1) (-2. *. !largeur1) (-12. *. !largeur1) (-2. *. !largeur1)) |]
           and fact_pos_array3 = [| 0. ; 0.25 ; 0.5 ; 0.75 ; 0. ; 0.25 ; 0.5 ; 0.75 ; 0. ; 0.25 ; 0.5 ; 0.75 ;|]
           and vitesse_array3 = [|0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03 ; 0.03|] in
           array_points_bleus3 := make_point_bleu_array 12 r nature_array3 caract_array3 fact_pos_array3 vitesse_array3 ;
 
 
           array_points_bleus := Array.concat((!array_points_bleus1)::(!array_points_bleus2)::(!array_points_bleus4)::(!array_points_bleus3)::[]);
         
 
           environnement :=
           [| make_mur (!cote_gauche) (!cote_gauche +. !largeur1) (!sol) (!plafond);
           make_mur (!cote_droit -. !largeur1) (!cote_droit) !sol !plafond ;
           make_mur (!cote_gauche +. !largeur1) (!cote_droit-. !largeur1) (!plafond -. !largeur1) !plafond ;
           make_mur (!cote_gauche +. !largeur1) (!cote_droit-. !largeur1) !sol (!sol +. !largeur1) ;
           make_mur (!cote_gauche +. !largeur1) (!cote_droit -. (12. *. !largeur1)) (!plafond -. (6. *. !largeur1)) (!plafond -. (5. *. !largeur1)) ;
           make_mur (!cote_droit -. (13. *. !largeur1)) (!cote_droit -. (12. *. !largeur1)) (!sol +. (3. *. !largeur1)) (!plafond -. (6. *. !largeur1)) ;
           make_mur (!cote_gauche +. (3. *. !largeur1)) (!cote_droit -. (12. *. !largeur1)) (!sol +. (3. *. !largeur1)) (!sol +. (4. *. !largeur1)) ;
           make_mur (!cote_gauche +. (3. *. !largeur1)) (!cote_gauche +. (4. *. !largeur1)) (!sol +. (4. *. !largeur1)) (!plafond -. (8. *. !largeur1)) ;
           make_mur (!cote_gauche +. (4. *. !largeur1)) (!cote_droit -. (17. *. !largeur1)) (!plafond -. (9. *. !largeur1)) (!plafond -. (8. *. !largeur1));
           make_mur (!cote_gauche +. (4. *. !largeur1)) (!cote_droit -. (17. *. !largeur1)) (!sol +. (5. *. !largeur1)) (!sol +. (6. *. !largeur1)) |] ;
        end;
   
   (*Actualisation des positions des éléments*)
     if button_down() then update_joueur !joueur;
 
     update_all_points_bleus !array_points_bleus ;

   (*Vérification des collisions*)
     if collision_mortelle !joueur !array_points_bleus then
       begin
         !joueur.c_pos<- !position_spawn ;
         reset_pieces_jaunes !tab_pieces_jaunes;
       end;
 
     collision_mur !joueur !environnement;
     ramasse_pieces_jaunes !joueur !tab_pieces_jaunes;
 
     while Unix.gettimeofday() < !t +. !dt do (); done; (*Attendre que 1/60s s'écoule*)
	   t:=Unix.gettimeofday();
      
   (*Affichage de tous les éléments*)
     clear_graph();
     affiche_murs !environnement;
     affiche_pieces_jaunes !tab_pieces_jaunes;
     affiche_zone_verte !zone_verte_debut;
     affiche_zone_verte !zone_verte_fin;
     affiche_points_bleus !array_points_bleus ;
     affiche_carre_rouge !joueur ;
    
 
 
if victoire !joueur !tab_pieces_jaunes !zone_verte_fin then
        begin
           pasgagne := false;
        end;
	(* Permet de Terminer la partie *)
 
 
     synchronize();
 
     
     done;
()
;;
 
main();;
 

