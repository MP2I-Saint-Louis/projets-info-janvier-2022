#load "graphics.cma";;
open Graphics;;
#load "unix.cma";;
open Unix;;


(* ---------- TYPES ---------- *)

type coo = {mutable x: float; mutable y: float};;

type mur = {p1: coo; p2: coo};;

type boule = {centre: coo; rayon: float};;

(* Caracterisation d'un mur par une fonction affine, de la forme: 
f: x -> [coef] * x + [ord].
Lorsque le mur est vertical; abs([relatif]) vaut 2. *)
type equaMur = {relatif: float; coef: float; ord: float; normale: coo};;

type bumperPoly = {cotes: equaMur list; normale: coo; ajout: float};;

type bumperBoule = {forme: boule; ajout: float};;

type flipper = {limite: equaMur; taille: boule};;

type trous = {place: boule; points: int};;



(* ---------- Fonctions sur les points et les vecteurs ---------- *)

let norme vec = sqrt(vec.x ** 2. +. vec.y ** 2.);;

let vec_unit vec =
	let n = norme vec in
	{x = vec.x /. n; y = vec.y /. n};;

let creer_vecteur p1 p2 = {x = p2.x -. p1.x; y = p2.y -. p1.y};;

let prod_scalaire v1 v2 = v1.x *. v2.x +. v1.y *. v2.y;;

let perpend vec =
	{x = -. (prod_scalaire vec {x=0.;y=1.});
	 y = prod_scalaire vec {x=1.;y=0.}};;



(* ---------- Creation des éléments (brut) ---------- *)

let listeMurs = 
	[{p1 = {x=700.;y=670.}; p2 = {x=0.;y=670.}};
	 {p1 = {x=20.;y=700.}; p2 = {x=20.;y=0.}};
	 {p1 = {x=680.;y=0.}; p2 = {x=680.;y=700.}};
	 {p1 = {x=10.;y=310.}; p2 = {x=310.;y=10.}};
	 {p1 = {x=390.;y=10.}; p2 = {x=690.;y=310.}};
	 {p1 = {x=0.;y=0.}; p2 = {x=700.;y=0.}}];;

let listeTrous = 
	[{place={centre={x=40.;y=650.};rayon=16.};points=5};
	 {place={centre={x=350.;y=650.};rayon=16.};points=10};
	 {place={centre={x=170.;y=510.};rayon=16.};points=10};
	 {place={centre={x=630.;y=650.};rayon=16.};points=20};];;

let listeBoules =
	[{centre = {x=500.;y=350.}; rayon = 70.}];;

let listeBumpersBoules = [{forme={centre={x=450.;y=605.};rayon=70.};ajout=4.};
								  {forme={centre={x=160.;y=605.};rayon=70.};ajout=4.}];;

let listeSommets1 = {x=580.; y=580.}::{x=690.; y=540.}::{x=690.; y=300.}::{x=580.; y=190.}::[]
and listeSommets2 = {x=10.; y=400.}::{x=10.; y=680.}::{x=75.; y=680.}::{x=75.; y=400.}::[]
and listeSommets3 = [{x=570.; y=580.};{x=690.;y=690.};{x=690.; y=530.}]
and listeSommets4 = [{x=65.;y=320.};{x=65.;y=680.};{x=105.;y=680.};{x=105.;y=320.}]
and listeSommets5 = [{x=85.;y=320.};{x=85.;y=539.};{x=210.;y=459.};{x=240.;y=290.}];;



(* ------ Fonctions pour la creation de la liste d'equations des murs ------ *)

let equa_mur mur = 
	
	let rel = ref 0.
	and coef = ref 0.
	and ord = ref 0.
	and x1 = mur.p1.x
	and x2 = mur.p2.x
	and y1 = mur.p1.y
	and y2 = mur.p2.y in
	
	if x1 = x2 then
		begin
			if y1 > y2 then rel := 2.
			else rel := -.2.;
			
			ord := x1
		end
	else
		begin
			if x1 > x2 then rel := 1.
			else rel := -.1.;
			
			coef := (y1-.y2)/.(x1-.x2);
			ord := y1 -.(x1 *. !coef);
		end;
	
	let d = vec_unit (creer_vecteur mur.p1 mur.p2) in
	let n = perpend d in
	
	{relatif = !rel;
	 coef = !coef;
	 ord = !ord;
	 normale = n};;

let rec creer_liste_equa listeMurs = match listeMurs with
	| hd::tl -> (equa_mur hd)::(creer_liste_equa tl)
	| [] -> [];;

let listeEquaMurs = creer_liste_equa (listeMurs);;

let flipperGauche={limite=(equa_mur {p1={x=245.;y=75.};p2={x=250.;y=80.}});
						taille={centre={x=245.;y=75.};rayon=95.}}
and flipperDroit={limite=(equa_mur {p1={x=450.;y=80.};p2={x=455.;y=75.}});
						taille={centre={x=455.;y=75.};rayon=95.}};;



(* ---------- Fonctions pour la creation de bumpers en polygones ---------- *)

let rec creer_poly premier listeSommets = match listeSommets with
(* Les sommets sont listes dans le sens horaire, on cree une liste de mur entre
chaque sommets voisins *)
	| hd1::hd2::tl -> 
		(equa_mur {p1=hd1;p2=hd2})::creer_poly premier (hd2::tl)
	| hd::[] -> (equa_mur {p1=hd;p2=premier})::[]
	| [] -> [];;

let creer_bumper_poly listeSommets normale ajout = 
	{cotes = creer_poly (List.hd listeSommets) listeSommets;
	 normale = normale;
	 ajout = ajout};;

let listePolys = [(creer_poly {x=570.; y=580.} listeSommets3);
						(creer_poly {x=65.;y=320.} listeSommets4);
						(creer_poly {x=85.;y=320.} listeSommets5)];;

let listeBumpersPolys = [(creer_bumper_poly listeSommets1 {x=(-.0.71);y=0.71} 4.);
								 (creer_bumper_poly listeSommets2 {x=0.71;y=0.71} 4.)];;



(* ---- Fonction pour changer la vitesse de la balle lors d'un rebond ---- *)

let rebond pos p2 vit normale ajout = 
	
	let projN = prod_scalaire normale vit in
	
(*dans le cas où l'ajout se fait dans le sens actuel de la vitesse:*)
	if projN > 0. then
		begin
			vit.x <- vit.x +. ajout *. normale.x;
			vit.y <- vit.y +. ajout *. normale.y
		end
(*Sinon: *)
	else
		begin
			vit.x <- vit.x +. (ajout -. projN) *. normale.x;
			vit.y <- vit.y +. (ajout -. projN) *. normale.y
		end;
	
	pos.x <- p2.x;
	pos.y <- p2.y;;



(* ---------- Fonctions verifiant si la balle est sur une forme ---------- *)

let sur_mur pos mur =
	
	let rel = mur.relatif in 
	
	if abs_float rel = 1. then
		
		let diff = pos.x *. mur.coef +. mur.ord -. pos.y in
		
		if (diff >= 0. && rel < 0.) || (diff <= 0. && rel > 0.) then true
		else false
		
	else
		
		let diff = mur.ord -. pos.x in
		
		if (diff >= 0. && rel > 0.) || (diff <= 0. && rel < 0.) then true
		else false;;

let rec sur_poly pos forme = match forme with
	| hd::_ when not (sur_mur pos hd) -> false
	| _::tl -> sur_poly pos tl
	| [] -> true;;

(*Cette fonction indique de quel 'cote' du polygone la balle est entree dans le
polygone en considerant la position precedente de la balle*)
let rec cote_dentree p2 forme = match forme with
	| hd::_ when not (sur_mur p2 hd) -> hd
	| _::tl -> cote_dentree p2 tl;;

let sur_boule pos boule = 
	norme (creer_vecteur pos boule.centre) <= boule.rayon;;

let sur_flipper pos flipper = 
	(sur_mur pos flipper.limite) && (sur_boule pos flipper.taille);;



(* ----- Fonctions traitant le rebond de la balle sur les formes DURES ----- *)

let rec sur_murS pos p2 vit listeEquaMurs = match listeEquaMurs with
	| hd::_ when sur_mur pos hd -> 
	rebond pos p2 vit hd.normale ((norme vit)*.0.5) (*Absorbe de la vitesse*)
	| _::tl -> sur_murS pos p2 vit tl
	| [] -> ();;

let rec sur_polyS pos p2 vit listePolys = match listePolys with
(* On estime que la balle n'etait pas sur le polygone a la position p2 *)
	| hd::_ when sur_poly pos hd ->
		let mur = cote_dentree p2 hd in
		rebond pos p2 vit mur.normale ((norme vit)*.0.5)
	| _::tl -> sur_polyS pos p2 vit tl
	| [] -> ();;

let rec sur_bouleS pos p2 vit listeBoules = match listeBoules with
	| hd::tl ->
		if sur_boule pos hd then
			begin
				let normale = vec_unit (creer_vecteur hd.centre p2) in
				rebond pos p2 vit normale ((norme vit)*.0.5)
			end
		else sur_bouleS pos p2 vit tl
	| [] -> ();;



(* ---- Fonctions traitant le rebond de la balle sur les autres formes ---- *)

let rec sur_bumper_polyS pos p2 vit listeBumpersPolys =
	match listeBumpersPolys with
	| hd::_ when sur_poly pos hd.cotes ->
		rebond pos p2 vit hd.normale (hd.ajout)
	| _::tl -> sur_bumper_polyS pos p2 vit tl
	| [] -> ();;

let rec sur_bumper_bouleS pos p2 vit listeBumpersBoules = 
	match listeBumpersBoules with
	| hd::tl -> 
		if sur_boule pos hd.forme then
			begin
				let normale = vec_unit (creer_vecteur hd.forme.centre p2) in
				rebond pos p2 vit normale (hd.ajout)
			end
		else sur_bumper_bouleS pos p2 vit tl
	| [] -> ();;

let rec sur_flipperS timer pos p2 vit =
(* Contact lorsque les flippers sont 'en mouvement' (avant 0.2 sec) *)
	if !timer > 0.5 then
		begin 
			if sur_flipper pos flipperGauche then
				begin 
					let d = (creer_vecteur flipperGauche.taille.centre pos) in
					let n = vec_unit {x=(-.d.y);y=d.x} in
					rebond {x=0.;y=0.} {x=0.;y=0.} vit n (1. +. 0.1 *. (norme d))
				end
			else if sur_flipper pos flipperDroit then
			begin
				let d = (creer_vecteur flipperDroit.taille.centre pos) in
				let n = vec_unit {x=d.y;y=(-.d.x)} in
				rebond {x=0.;y=0.} {x=0.;y=0.} vit n (1. +. 0.1 *. (norme d))
			end
			else ();
			timer:= 0.5
		end
	else
(* Contact lorsque les flippers sont immobiles (après 0.2 sec) *)
		begin
			let a1 = sur_flipper pos flipperGauche
			and b1 = sur_mur p2 flipperGauche.limite in
			if a1 && not(b1) then 
				let d = (creer_vecteur flipperGauche.taille.centre pos) in
				let n = vec_unit {x=(-.d.y);y=d.x} in
				rebond {x=0.;y=0.} {x=0.;y=0.} vit n ((norme vit)*.0.5)
			else
				let a2 = sur_flipper pos flipperDroit
				and b2 = sur_mur p2 flipperDroit.limite in
			if a2 && not(b2) then 
				let d = (creer_vecteur flipperDroit.taille.centre pos) in
				let n = vec_unit {x=d.y;y=(-.d.x)} in
				rebond {x=0.;y=0.} {x=0.;y=0.} vit n ((norme vit)*.0.5)
			else ()
		end;;

(* ---------- Fonction pour marquer des points ---------- *)

let rec sur_trouS pos p2 vit score vies listeTrous = match listeTrous with
	| hd::tl -> if sur_boule pos hd.place then
			begin
				score := !score + hd.points;
				vies := !vies - 1;
				pos.x <- 50.;
				pos.y <- 360.;
				p2.x <- 50.;
				p2.y <- 360.;
				vit.x <- 0.;
				vit.y <- 2.;
			end
		else sur_trouS pos p2 vit score vies tl
	| [] -> ();;



(* ---------- Fonction pour limiter la vitesse instantanée ---------- *)

let limiteVitesse vit vitesseLimite =
	let nVit = norme vit in
	if nVit > vitesseLimite then
		begin
			let coef = vitesseLimite /. nVit in
			vit.x <- vit.x *. coef;
			vit.y <- vit.y *. coef;
		end
	else ();;



(* ---------- Fonction d'affichage ---------- *)

let affichage vies pos etat flipperTimer bumperPolyTimer score lastScore =
	clear_graph();
	
	(* Flippers Activés *)
	if flipperTimer > 0.25 then 
		begin
			set_color blue;
			draw_circle 245 75 95;
			draw_circle 455 75 95;
			set_color white;
			fill_poly [|(235,65);(335,165);(235,265);(135,165)|];
			fill_poly [|(465,65);(565,165);(465,265);(365,165)|];
			set_color blue;
			fill_poly [|(240,70);(315,145);(250,60)|];
			fill_poly [|(460,70);(385,145);(450,60)|]
		end
	else if flipperTimer > 0. then
		begin
			set_color blue;
			fill_poly [|(240,70);(315,145);(250,60)|];
			fill_poly [|(460,70);(385,145);(450,60)|]
		end
	else ();
	
	(* Bumper Polygones *)
	set_color (if bumperPolyTimer<0.05 then green else cyan);
	fill_rect 10 400 65 280;
	fill_poly [|(580,580);(690,540);(690,300);(580,190)|];
	
	(* Decors *)
	set_color black;
	fill_rect 0 10 10 700;
	fill_rect 690 10 10 700;
	fill_rect 0 670 700 10;
	fill_poly [|(10,300);(300,10);(260,10);(10,260)|];
	fill_poly [|(690,300);(690,260);(440,10);(400,10)|];
	fill_poly [|(150,120);(280,0);(150,0)|];
	fill_poly [|(550,120);(420,0);(550,0)|];
	
	fill_circle 500 350 60;
	
	fill_poly [|(580,580);(690,680);(690,540)|];
	fill_rect 75 330 20 340;
	fill_poly [|(95,330);(95,530);(200,450);(230,300)|];
	
	(* trous pour marquer *)
	draw_circle 40 650 10;
	draw_circle 350 650 10;
	draw_circle 170 510 10;
	draw_circle 630 650 10;
	
	(* Bumpers Boules *)
	set_color yellow;
	fill_circle 450 605 60;
	fill_circle 160 605 60;
	set_color black;
	fill_circle 450 605 50;
	fill_circle 160 605 50;
	
	(* Flippers desactivés *)
	if flipperTimer<=0. then 
		begin
			set_color blue;
			fill_poly [|(240,70);(300,10);(230,60)|];
			fill_poly [|(460,70);(400,10);(470,60)|]
		end
	else ();
	
	(* Barre létale *)
	set_color red;
	fill_rect 0 0 700 10;
	
	(* Vies *)
	set_color blue;
	for j = 1 to vies do
		fill_circle (700 - (j*25)) 690 10
	done;
	
	(* - Commentaires - *)
	set_text_size 30;
	set_color green;
	moveto 20 200;
	draw_char 'A';
	
	set_color blue;
	moveto 670 200;
	draw_char 'Z';
	
	set_color black;
	if etat == 3 then
		begin
			set_color black;
			moveto 230 250;
			draw_string "FIN DE PARTIE";
			
			set_text_size 20;
			moveto 555 100;
			draw_string "Appuyer sur";
			moveto 585 70;
			draw_string "C pour";
			moveto 560 40;
			draw_string "recommencer";
		end
	else if etat == 2 then
		begin
			set_text_size 20;
			moveto 555 100;
			draw_string "Appuyer sur";
			moveto 585 70;
			draw_string "C pour";
			moveto 570 40;
			draw_string "continuer";
		end
	else ();
	
	set_text_size 20;
	moveto 20 120;
	draw_string "SCORE:";
	moveto 20 90;
	draw_string (string_of_int score);
	moveto 20 60;
	draw_string "PRECEDENT:";
	moveto 20 30;
	draw_string (string_of_int lastScore);
	
	(* Balle *)
	fill_circle (int_of_float pos.x) (int_of_float pos.y) 10;
	synchronize();;


(* ---------- Fonctions principales ---------- *)

let maj_frame pos p2 vit g =
	
	pos.x <- pos.x +. vit.x;
	pos.y <- pos.y +. vit.y;
	
	vit.y <- vit.y -. g (* gravite *);;

let main () =
	open_graph "700x700";
	auto_synchronize false;
	
	let temps = ref (Sys.time())
	and tamponTouches = ref 'o'
	and score = ref 0
	and lastScore = ref 0
	and vies = ref 10 in
	
	(* Actuelle position de la balle *)
	let pos = {x=50.; y=360.}
	
	
	(* Ancienne position de la balle *)
	and p2 = {x=50.; y=360.}
	
	and vit = {x = 0.; y = 2.}
	
	and g = 0.05
	
	and flipperPressed = ref false
	and bumperPolyPressed = ref false
	and flipperTimer = ref 0.
	and bumperPolyTimer = ref 0. 
	
	and vitesseLimite = 7.5 in
	
	(* Boucle principale *)
	while true do
		
		while Sys.time() -. !temps < 0.01 do
			if key_pressed() then tamponTouches := read_key()
			else ()
		done;
		temps := Sys.time();
		
		maj_frame pos p2 vit g;
		
		(*Collision avec des formes 'dures'*)
		sur_murS pos p2 vit listeEquaMurs;
		sur_bouleS pos p2 vit listeBoules;
		sur_polyS pos p2 vit listePolys;
		(*Collision avec des Bumpers en boule gérée automatiquement*)
		sur_bumper_bouleS pos p2 vit listeBumpersBoules;
		
		(*Collision avec les formes interractives*)
		if !tamponTouches <> 'o' then
			begin
				if !tamponTouches == 'z' then
					if not(!flipperPressed) then 
						begin
							flipperPressed := true; 
							flipperTimer := 0.6
						end
					else if !flipperTimer < 0.2 then 
						flipperTimer := 0.2
					else ()
					
				else if !tamponTouches == 'a' && !bumperPolyTimer <= 0. then
					begin
						bumperPolyPressed := true; 
						bumperPolyTimer := 0.4
					end;
				tamponTouches := 'o'
			end
		else ();
		
		if !flipperPressed then
			if !flipperTimer>0. then
				begin
					sur_flipperS flipperTimer pos p2 vit;
					flipperTimer := !flipperTimer -.0.01
				end
			else
				flipperPressed := false
		else ();
		
		if !bumperPolyPressed then
			begin
				bumperPolyPressed := false;
				sur_bumper_polyS pos p2 vit listeBumpersPolys;
			end
		else ();
		
		if !bumperPolyTimer > 0. then
			bumperPolyTimer := !bumperPolyTimer -. 0.01
		else ();
		
		(*Verifie si l'on a marqué des points*)
		sur_trouS pos p2 vit score vies listeTrous;
		
		(*verifie si l'on a perdu*)
		if pos.y < 20. || !vies < 0 then
			begin
				pos.x <- 50.;
				pos.y <- 360.;
				p2.x <- 50.;
				p2.y <- 360.;
				vit.x <- 0.;
				vit.y <- 2.;
				vies := !vies - 1;
				if !vies < 0 then
					begin
						affichage 0 pos 3 !flipperTimer !bumperPolyTimer !score !lastScore;
						lastScore := !score;
						score := 0;
						vies := 10
					end
				else affichage !vies pos 2 !flipperTimer !bumperPolyTimer !score !lastScore;
				
				while !tamponTouches <> 'c' do
					if key_pressed() then tamponTouches := read_key()
					else ()
				done
			end
		else ();
		
		affichage !vies pos 1 !flipperTimer !bumperPolyTimer !score !lastScore;
		p2.x <- pos.x;
		p2.y <- pos.y;
		
		limiteVitesse vit vitesseLimite;
		
	done;;
main ();;
