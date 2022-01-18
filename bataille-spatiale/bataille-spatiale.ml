(*importation des fonctions*)
#load "graphics.cma";; 
open Graphics;;
open Random;;
#load "unix.cma";;

(*définition des types*)
type point= { x: float; y: float; z: float};;
type vecteur= { vx: float; vy: float; vz: float};;
type vaisseau= {mutable state : bool; pv_de_base: int; mutable pv: int; mutable pv_tir_impact: int; periode_tir: float ;mutable dernier_tir: float; mutable centre: int array;origine: int array; cote: float; args_f: float array; couleur: int*int*int; f_deplacement: int; mutable vitesse: float; role: char};;
type tir= { mutable statut: bool; pv_impact: int; t_creation: float; mutable pos : int array; origine: int array; vitesse_tir: float; tireur: char};;

(*Quelques variables utiles de base*)
let v_tir_ennemi=100.;;
let v_tir_joueur=200.;;
let pv_de_base_joueur=300;;
let pi = 4.0 *. atan 1.0;;
let joue=ref false;;

(*fonctions concernant les tirs*)

let avance_tir liste_tirs t = 
	for i=0 to (Array.length !liste_tirs)-1 do
		let tir= !liste_tirs.(i) in
			if (tir.tireur='J')&&(tir.statut) then
				tir.pos <- [|tir.origine.(0); tir.origine.(1)+int_of_float (tir.vitesse_tir *.(t-. tir.t_creation))|]
			else
				tir.pos <- [|tir.origine.(0); tir.origine.(1)-int_of_float (tir.vitesse_tir *.(t-.tir.t_creation))|]
	done;;

(*gestion des collisions entretirs et vaisseaux*)
let rec impact tir liste_vaisseaux debut longueur points =
	begin
		for i=debut to longueur-1 do
		let vaisseau= !liste_vaisseaux.(i) in
		let cote=vaisseau.cote in
		if (vaisseau.state)&&(tir.pos.(0)<=vaisseau.centre.(0)+ (int_of_float( cote/.2.))+1)&&(tir.pos.(0)>=vaisseau.centre.(0)- (int_of_float( cote/.2.))-1) then
			let demi_hauteur= int_of_float ((sqrt (cote*.cote*.(1. -. 1./.4.)))*.1./.2.) in
				if (tir.pos.(1)>= vaisseau.centre.(1)-demi_hauteur)&&(tir.pos.(1)<= vaisseau.centre.(1)+demi_hauteur) then
					begin
						vaisseau.pv <- (vaisseau.pv - tir.pv_impact);
						tir.statut <- false;
						if (vaisseau.pv)<=0 then 
							begin
								vaisseau.state <- false;
								if debut=1 then points:= !points+ vaisseau.pv_de_base;
							end;
					end
		done;
	end;;



let impact_tirs liste_tirs liste_vaisseaux points= 
	let l_v= Array.length !liste_vaisseaux 
	and l_t= Array.length !liste_tirs in 
	begin
	for i=0 to l_t-1 do
		let tir= !liste_tirs.(i) in
			if tir.tireur='E' then impact tir liste_vaisseaux 0 1 points
			else impact tir liste_vaisseaux 1 l_v points
	done;
	end;;

(*fonctions d'affichage des tirs*)
let trace_tir_joueur tir=
	let x= tir.pos.(0) and y=tir.pos.(1) in
	begin
		set_color red;
		for i=0 to 3 do
			begin
				plot (x) (y-i);
				plot (x-1) (y-i);
				plot (x-2) (y-i);
				plot (x+1) (y-i);
				plot (x+2) (y-i);
			end
		done;
		set_color (rgb 255 127 0);
		for j=0 to 4 do
			begin
			plot x (y-j-3);
			plot (x-1) (y-j-3);
			plot (x+1) (y-j-3);
			end
		done;
		set_color yellow;
		for g=0 to 7 do
			plot x (y-7-g)
		done;
	end;;
	
let trace_tir_ennemi tir=
	let x= tir.pos.(0) and y=tir.pos.(1) in
	begin
		set_color (rgb 9 221 42);
		for i=0 to 3 do
			begin
				plot (x) (y+i);
				plot (x-1) (y+i);
				plot (x-2) (y+i);
				plot (x+1) (y+i);
				plot (x+2) (y+i);
			end
		done;
		set_color (rgb 6 13 183);
		for j=0 to 4 do
			begin
			plot x (y+j+3);
			plot (x-1) (y+j+3);
			plot (x+1) (y+j+3);
			end
		done;
		set_color (rgb 128 0 128);
		for g=0 to 7 do
			plot x (y+7+g)
		done;
	end;;
	
let affiche_tir liste_tir=
	for i=0 to (Array.length !liste_tir) -1 do
		let tir= !liste_tir.(i) in
		if tir.tireur='J' then trace_tir_joueur tir
		else trace_tir_ennemi tir
	done;;
	
(*vérifications que les trs ne sont pas sortis de la fenetre*)
let tirs_valides liste_tirs =
	for i=0 to (Array.length !liste_tirs) -1 do
		let tir= !liste_tirs.(i) in
			if tir.statut then
				if (tir.pos.(0)>size_x())||(tir.pos.(1)>size_y())||(tir.pos.(0)<0)||(tir.pos.(1)<0) then
					tir.statut<- false
	done;;
	
(*fonction pour tirer*)
let tirer vaisseau liste_tirs t=
	if vaisseau.state then
		let longueur= (Array.length !liste_tirs) and indice= ref 0 in
		begin
		let v_tir= ref 0. in
		if vaisseau.role='E' then v_tir:=v_tir_ennemi
		else v_tir:= v_tir_joueur;
		while (!indice<=longueur-1)&&(!liste_tirs.(!indice).statut) do indice:= !indice +1 done;
		let nouveau_tir={statut=true; pv_impact= vaisseau.pv_tir_impact; t_creation=t;pos=[|vaisseau.centre.(0);vaisseau.centre.(1)|];origine=[|vaisseau.centre.(0); vaisseau.centre.(1)|]; vitesse_tir= !v_tir; tireur= vaisseau.role} in
		if !indice=longueur then liste_tirs:= Array.append !liste_tirs [|nouveau_tir|]
		else !liste_tirs.(!indice)<- nouveau_tir;
		end;;

(*Fonctions servant à faire tirer les vaisseaux ennemis*)
let tirs_ennemi_regulier liste_vaisseaux t liste_tirs=
	begin
	for i=1 to (Array.length !liste_vaisseaux)-1 do
		let vaisseau= !liste_vaisseaux.(i) in
			if ((t-.vaisseau.dernier_tir)>= vaisseau.periode_tir)&&(vaisseau.state) then 
				begin
					tirer vaisseau liste_tirs t;
					vaisseau.dernier_tir<- t;
				end
	done;
	!liste_tirs;
	end;;
	
	
(*fonctions de fond*)
(*affichage du fond*)
let fond x y=
	begin
		set_color black;
		fill_rect 0 0 x y;
		let e_par_ligne=(x -(x mod 25))/25 and 
		nb_col_e=(y -(y mod 50))/50 in
		for i=1 to e_par_ligne*nb_col_e do
		begin
			set_color white;
			plot (int x) (int y);
			plot (int x) (int y);
			set_color blue;
			plot (int x) (int y);
			set_color red;
			plot (int x) (int y);
		end
		done;
	end;;

let ecran_d_acceuil x y=
	begin
	fond x y;
	set_color green;
	let partie = (float_of_int y)*.1./.15.  and x_float= (float_of_int x) in
	set_text_size 30;
	moveto (int_of_float (x_float*.1./.3.)) (int_of_float (partie*.14.));
	draw_string "INSTRUCTIONS";
	set_text_size 20;
	moveto (int_of_float (x_float*.1./.4.)) (int_of_float (partie*.12.));
	draw_string "Pour tirer, pressez 't'";
	moveto (int_of_float (x_float*.1./.4.)) (int_of_float (partie*.10.));
	draw_string "Pour aller en arrière, pressez 'h'";
	moveto (int_of_float (x_float*.1./.4.)) (int_of_float (partie*.8.));
	draw_string "Pour aller vers l'avant, pressez 'y'";
	moveto (int_of_float (x_float*.1./.4.)) (int_of_float (partie*.6.));
	draw_string "Pour aller à gauche, pressez 'g'";
	moveto (int_of_float (x_float*.1./.4.)) (int_of_float (partie*.4.));
	draw_string "Pour aller à droite, pressez 'j'";
	moveto (int_of_float (x_float*.1./.4.)) (int_of_float (partie*.2.));
	draw_string "Pour quitter le jeu, pressez 'e'";
	moveto (int_of_float (x_float*.1./.20.)) (int_of_float (partie*.1.));
	draw_string "Faites attention à ne pas presser la touche maj, pressez 'a' pour jouer,";
	moveto (int_of_float (x_float*.1./.2.5)) 0;
	draw_string "Bon jeu!";
	end;;

(*affichage des pv du joueur*)
let affiche_barre_pv vaisseau =
	begin
		moveto 0 (size_y() - 20);
		set_text_size 10;
		set_color green;
		draw_string "PV";
		set_color red;
		fill_rect 20 (size_y()-20) (10+(int_of_float ((float_of_int pv_de_base_joueur)*.0.10))) (size_y()-10);
		set_color green;
		let espace_rempli_vert= int_of_float ((float_of_int vaisseau.pv)*.0.10) in
		if vaisseau.pv>0 then fill_rect 20 (size_y()-20) (10+espace_rempli_vert) (size_y()-10);
	end;;

(*affichage des points obtenus par le joueur*)
let affiche_points point =
	begin
		moveto 0 (size_y() - 40);
		set_text_size 10;
		set_color red;
		draw_string "PTS";
		moveto 25 (size_y() - 40);
		draw_string (string_of_int !point);
	end;;

(*Si le joueur perd, on affihe l'écran d'échec*)
let lose points = 
	begin
	fond (size_x()) (size_y());
	set_color red;
	set_text_size 250;
	let x_max= float_of_int (size_x()) and y_max= float_of_int (size_y()) in
		moveto (int_of_float (x_max*.1./.8.)) (int_of_float (y_max*.1./.4.));
		draw_string ("ECHEC");
		set_color green;
		moveto (int_of_float (x_max*.1./.4.)) (int_of_float (y_max*.1./.10.));
		set_text_size 20;
		draw_string "Pour arrêter de jouer, pressez 'e'.";
		moveto (int_of_float (x_max*.1./.4.)) (int_of_float (y_max*.1./.20.));
		draw_string "Pour rejouer, pressez 'r'!";
		moveto (int_of_float (x_max*.1./.3.)) (int_of_float (y_max-. 80.));
		draw_string "points: ";
		moveto (int_of_float (x_max*.1./.3. +. 80.)) (int_of_float (y_max -. 80.));
		draw_string (string_of_int points);
	end;;

(*Si le joueur arrive à la fin des niveaux*)
let win points =
	begin
	fond (size_x()) (size_y());
	set_color green;
	set_text_size 250;
	let x_max= float_of_int (size_x()) and y_max= float_of_int (size_y()) in
		moveto (int_of_float (x_max*.1./.8.)) (int_of_float (y_max*.1./.4.));
		draw_string ("Gagné!");
		set_color red;
		moveto (int_of_float (x_max*.1./.4.)) (int_of_float (y_max*.1./.10.));
		set_text_size 15;
		draw_string "Pour arrêter de jouer, pressez 'e'.";
		moveto (int_of_float (x_max*.1./.4.)) (int_of_float (y_max*.1./.20.));
		draw_string "Pour jouer à des niveau aléatoires, pressez 'w'!";
		moveto (int_of_float (x_max*.1./.4.)) (int_of_float (y_max*.1./.30.));
		draw_string "Pour rejouer, pressez 'r'!";
		moveto (int_of_float (x_max*.1./.3.)) (int_of_float (y_max-.80.));
		draw_string "points: ";
		moveto (int_of_float (x_max*.1./.3. +. 80.)) (int_of_float (y_max-.80.));
		draw_string (string_of_int points);
	end;;
(*fonctions de mouvement des vaisseaux ennemis*)

(* fonctions permettant aux vaisseaux de faire de mouvement circulaires*)
let cercle_x o t k periode= int_of_float ( k*. cos (o*. t/.periode));;
let cercle_y o t k periode= int_of_float ( k*. sin (o*. t/.periode));;

(*fonction permettant aux vaisseaux de faire des mouvement triangulaires*)
let triangle_x periode cote t hauteur= let res= ref 0. in
	let t2= mod_float t periode in
	if (t2<periode/.3.) then res:=t2*. (cote/.2.) /. (periode/.3.)
	else 
		if (t2> 2.*.periode/.3.) then res:=t2*. cote /.(2.*. periode/.3.) -.3.*. cote/.2.
		else res:= 3.*.cote/.2. -. t2*. cote /. (periode/.3.); (*IDK ce qui se passe *)
	int_of_float !res;;
	
let triangle_y periode cote t hauteur= let res= ref 0. in
	let t2= mod_float t periode in
	if (t2 <periode/.3.) then res:= t2*.hauteur/. (periode/.3.)
	else
		if ( t2> 2.*.periode/.3.) then res:= 3.*.hauteur -. t2*.hauteur/. (periode/.3.)
		else res:=hauteur;
	int_of_float !res;;
	
(*fonctions permettants de faire des mouvements en forme de vagues comme la courbe
cosinus ou sinus*)
let vague_x t vitesse = let x= int_of_float (t*.vitesse) in x;;
let vague_y t theta periode amplitude = let y= int_of_float(amplitude*.cos(theta*.2.*.pi*.t/.periode)) in y;;

(*fonctions pour afficher les vaisseaux, on différenciera
l'affichage des ennemis et du joueur*)
let trace_vaisseaux tableau longueur=
	for i=0 to longueur-1 do
		let vaisseau= !tableau.(i) in
		let cote= vaisseau.cote in
			if vaisseau.state=true then
			if vaisseau.role='E' then
				let hauteur= sqrt (cote*.cote*.(1. -.1./.4.)) in
					let p1=(int_of_float ((float_of_int vaisseau.centre.(0)) -. cote/.2.),int_of_float ((float_of_int vaisseau.centre.(1)) +. hauteur/.2.))
					and p2=(int_of_float ((float_of_int vaisseau.centre.(0)) +. cote/.2.),int_of_float ((float_of_int vaisseau.centre.(1)) +. hauteur/.2.))
					and p3=(vaisseau.centre.(0),int_of_float ((float_of_int vaisseau.centre.(1)) -. hauteur/.2.)) 
					and (a, b, c)=vaisseau.couleur in
					begin
						set_color (rgb a b c);
						fill_poly [|p1;p2;p3|];
					end;
			else
				let hauteur= sqrt (cote*.cote*.(1. -.1./.4.)) in
					let p1=(int_of_float ((float_of_int vaisseau.centre.(0)) -. cote/.2.),int_of_float ((float_of_int vaisseau.centre.(1)) -. hauteur/.2.))
					and p2=(int_of_float ((float_of_int vaisseau.centre.(0)) +. cote/.2.),int_of_float ((float_of_int vaisseau.centre.(1)) -. hauteur/.2.))
					and p3=(vaisseau.centre.(0),int_of_float ((float_of_int vaisseau.centre.(1)) +. hauteur/.2.)) 
					and (a, b, c)=vaisseau.couleur in
					begin
						set_color (rgb a b c);
						fill_poly [|p1;p2;p3|];
					end;
	done;;			

(*fonctions pour faire bouger le joueur, on fera attention à ce que le vaisseau
ne sorte pas de la fenetre*)
let move_joueur vx vy vaisseau= let centre= vaisseau.centre 
	and cote=vaisseau.cote in
	let resx= ref 0 
	and resy=ref 0 
	and demi_hauteur= int_of_float ((sqrt (cote*.cote -.(cote*.cote/.4.)))/.2.) in
	begin
	if (centre.(0)+ (int_of_float (vx*.vaisseau.vitesse))>size_x()-demi_hauteur) then resx:= size_x()-demi_hauteur
	else
		if (centre.(0)+ (int_of_float (vx*.vaisseau.vitesse))<0 + demi_hauteur) then resx:= demi_hauteur
		else resx:=centre.(0)+ (int_of_float (vx*.vaisseau.vitesse));
	if (centre.(1)+ (int_of_float (vy*.vaisseau.vitesse))> size_y() - int_of_float (cote/.2.)) then resy:=size_y() - int_of_float (cote/.2.)
	else
		if (centre.(1)+ (int_of_float (vy*.vaisseau.vitesse))< 0 + int_of_float (cote/.2.)) then resy:= int_of_float (cote/.2.)
		else resy:=centre.(1)+ (int_of_float (vy*.vaisseau.vitesse));
	vaisseau.centre.(0)<- !resx;
	vaisseau.centre.(1)<- !resy;
	end;;

(*fonction pur faire bouger tous les vaisseaux ennemi en fonction de 
leur mouvement à l'instant t*)
let move_ennemi fonctions longueur t liste_vaisseaux=
	for i=1 to longueur-1 do
	let vaisseau= !liste_vaisseaux.(i) in
	let fonction= fonctions.(vaisseau.f_deplacement) in
	let resx= ref 0 and resy=ref 0 in
	if (vaisseau.state)&&(vaisseau.role='E') then
		begin
		let args= vaisseau.args_f in
		if fonction="cercle" then
			begin
				resx:= (cercle_x args.(0) t args.(1) vaisseau.vitesse)+ vaisseau.origine.(0);
				resy:= (cercle_y args.(0) t args.(1) vaisseau.vitesse) + vaisseau.origine.(1);
			end
		else
			if fonction="triangle" then
				begin
				resx:= (triangle_x args.(0) (vaisseau.cote*.args.(2)) t (args.(1)*.args.(2))) + vaisseau.origine.(0);
				resy:= (triangle_y args.(0) (vaisseau.cote*. args.(2)) t (args.(1)*.args.(2)))+ vaisseau.origine.(1);
				end
			else
				begin
				resx:= ((vague_x t vaisseau.vitesse)+ vaisseau.origine.(0)) mod size_x();
				resy:= (vague_y t args.(0) vaisseau.vitesse args.(1) + vaisseau.origine.(1)) mod size_y();
				end;
		vaisseau.centre.(0)<- !resx;
		vaisseau.centre.(1)<- !resy;
		end
	done;;

(*Définition des niveaux*)
(*fonction afin d'obtenir un vaisseau avec les mêmes caractèristique 
que celui donné en argument excepté sa position*)
let copie_vaisseau vaisseau pos_x pos_y =
	{state =true; pv_de_base=vaisseau.pv_de_base; pv=vaisseau.pv; pv_tir_impact =vaisseau.pv_tir_impact;
	periode_tir= vaisseau.periode_tir; dernier_tir=vaisseau.dernier_tir;
	centre=[|pos_x; pos_y|]; origine=[|pos_x; pos_y|]; cote=vaisseau.cote;
	args_f=vaisseau.args_f; couleur=vaisseau.couleur; f_deplacement=vaisseau.f_deplacement;
	vitesse=vaisseau.vitesse; role=vaisseau.role};;

(*définition des types de vaisseaux ennemis*)
let bleu_clair =(176,224,230);;
let orange= (255,165,0);;
let jaune_clair= (240,230,140);;
let bordeau= (220,20,60);;
let fuschia = (255,0,255);;
let bleu_vert= (0,250,154);;
let menthe = (127,255,212);;

let v1= { state=true; pv_de_base=8; pv=8; pv_tir_impact=3; periode_tir= 1.3; 
dernier_tir=0.; centre=[|100;300|]; origine= [|100;300|]; cote=25.;
args_f=[|30.; 60.|]; couleur= orange; f_deplacement=0;
vitesse= 15.; role='E'};;

let v2 = { state=true; pv_de_base=10;pv=10; pv_tir_impact=10; periode_tir= 2.5; 
dernier_tir=0.; centre=[|100;350|]; origine= [|100;350|]; cote=50.;
args_f=[|2.5; sqrt (50.*.50. -. 25.*.25.); 4.5|]; couleur= fuschia; f_deplacement=1;
vitesse= 5.; role='E'};;

let v3= { state=true; pv_de_base=15;pv=15; pv_tir_impact=2; periode_tir= 1.; 
dernier_tir=0.; centre=[|100;300|]; origine= [|100;300|]; cote=20.;
args_f=[|25.; 50.|]; couleur= jaune_clair; f_deplacement=2;
vitesse= 50.; role='E'};;

let v4= { state=true; pv_de_base=30;pv=30; pv_tir_impact=2; periode_tir= 2.; 
dernier_tir=0.; centre=[|100;300|]; origine= [|100;300|]; cote=15.;
args_f=[|25.; 50.|]; couleur= bordeau; f_deplacement=0;
vitesse= 10.; role='E'};;

let boss1= { state=true;pv_de_base=50; pv=50; pv_tir_impact=5; periode_tir= 1.2; 
dernier_tir=0.; centre=[|500;200|]; origine= [|500;200|]; cote=50.;
args_f=[|30.; 60.|]; couleur= menthe; f_deplacement=0;
vitesse= 15.; role='E'};;

let boss2= { state=true; pv_de_base=100;pv=100; pv_tir_impact=7; periode_tir= 1.5; 
dernier_tir=0.; centre=[|500;200|]; origine= [|500;200|]; cote=50.;
args_f=[|2.5;sqrt (50.*.50. -. 25.*.25.); 4.5 |]; couleur= bleu_clair; f_deplacement=1;
vitesse= 8.; role='E'};;

let boss3= { state=true;pv_de_base=100; pv=100; pv_tir_impact=10; periode_tir= 1.8; 
dernier_tir=0.; centre=[|500;200|]; origine= [|500;200|]; cote=50.;
args_f=[|25.;50.|]; couleur= bleu_vert; f_deplacement=2;
vitesse= 70.; role='E'};;
(*liste des vaisseaux ennemis possibles*)
let ennemis_possibles= [|v1;v2;v3;v4;v1;v2;v3;v4;boss1;boss2;boss3|];;

(*niveau 1*)
let vaisseaux_niveau_1 = [|copie_vaisseau boss1 500 200;
copie_vaisseau v1 100 300; copie_vaisseau v1 150 300;
copie_vaisseau v1 200 300;copie_vaisseau v1 250 300;
copie_vaisseau v1 300 300;copie_vaisseau v1 350 300;
copie_vaisseau v1 400 300;copie_vaisseau v1 450 300;
copie_vaisseau v1 500 300;copie_vaisseau v1 550 300;
copie_vaisseau v1 600 300;copie_vaisseau v1 650 300;
copie_vaisseau v1 700 300;copie_vaisseau v1 750 300;
copie_vaisseau v1 800 300;copie_vaisseau v1 850 300|];;

let puissance_tir_1 = 5;;

(*niveau 2*)
let vaisseaux_niveau_2 = [|copie_vaisseau boss2 500 200;
 copie_vaisseau v2 500 100;
copie_vaisseau v2 450 150;copie_vaisseau v2 550 150;
copie_vaisseau v2 400 200;copie_vaisseau v2 600 200;
copie_vaisseau v2 350 250;copie_vaisseau v2 650 250;
copie_vaisseau v2 300 300;copie_vaisseau v2 700 300;
copie_vaisseau v2 250 350;copie_vaisseau v2 750 350;
copie_vaisseau v2 200 400;copie_vaisseau v2 800 400;
copie_vaisseau v2 150 450;copie_vaisseau v2 850 450;
copie_vaisseau v2 100 500;copie_vaisseau v2 900 500|];;

let puissance_tir_2=8;;

(*niveau 3*)
let vaisseaux_niveau_3 =[| copie_vaisseau boss3 500 200; 
copie_vaisseau v3 100 150; 
copie_vaisseau v3 150 150;copie_vaisseau v3 200 150;
copie_vaisseau v3 250 150;copie_vaisseau v3 300 150;
copie_vaisseau v3 350 150;copie_vaisseau v3 400 150;
copie_vaisseau v3 450 150;copie_vaisseau v3 500 150;
copie_vaisseau v3 550 150;copie_vaisseau v3 600 150;
copie_vaisseau v3 650 150;copie_vaisseau v3 700 150;
copie_vaisseau v3 750 150;copie_vaisseau v3 800 150;
copie_vaisseau v3 850 150; copie_vaisseau v4 100 500;
copie_vaisseau v4 900 500; copie_vaisseau v4 100 400; 
copie_vaisseau v4 900 400|];;

let puissance_tir_3=10;;

(*fonction pour réinitialiser les vaisseaux*)
let reinitialisation vaisseaux_lvl =
	for i=0 to 2 do
		if i=0 then
			vaisseaux_lvl.(i)<-[|copie_vaisseau boss1 500 200;
				copie_vaisseau v1 100 300; copie_vaisseau v1 150 300;
				copie_vaisseau v1 200 300;copie_vaisseau v1 250 300;
				copie_vaisseau v1 300 300;copie_vaisseau v1 350 300;
				copie_vaisseau v1 400 300;copie_vaisseau v1 450 300;
				copie_vaisseau v1 500 300;copie_vaisseau v1 550 300;
				copie_vaisseau v1 600 300;copie_vaisseau v1 650 300;
				copie_vaisseau v1 700 300;copie_vaisseau v1 750 300;
				copie_vaisseau v1 800 300;copie_vaisseau v1 850 300|]		
		else if i=1 then 
				vaisseaux_lvl.(i)<-[|copie_vaisseau boss2 500 200;
				copie_vaisseau v2 500 100;
				copie_vaisseau v2 450 150;copie_vaisseau v2 550 150;
				copie_vaisseau v2 400 200;copie_vaisseau v2 600 200;
				copie_vaisseau v2 350 250;copie_vaisseau v2 650 250;
				copie_vaisseau v2 300 300;copie_vaisseau v2 700 300;
				copie_vaisseau v2 250 350;copie_vaisseau v2 750 350;
				copie_vaisseau v2 200 400;copie_vaisseau v2 800 400;
				copie_vaisseau v2 150 450;copie_vaisseau v2 850 450;
				copie_vaisseau v2 100 500;copie_vaisseau v2 900 500|]
				 		
		else vaisseaux_lvl.(i)<-[| copie_vaisseau boss3 500 200; 
				copie_vaisseau v3 100 150; 
				copie_vaisseau v3 150 150;copie_vaisseau v3 200 150;
				copie_vaisseau v3 250 150;copie_vaisseau v3 300 150;
				copie_vaisseau v3 350 150;copie_vaisseau v3 400 150;
				copie_vaisseau v3 450 150;copie_vaisseau v3 500 150;
				copie_vaisseau v3 550 150;copie_vaisseau v3 600 150;
				copie_vaisseau v3 650 150;copie_vaisseau v3 700 150;
				copie_vaisseau v3 750 150;copie_vaisseau v3 800 150;
				copie_vaisseau v3 850 150; copie_vaisseau v4 100 500;
				copie_vaisseau v4 900 500; copie_vaisseau v4 100 400; 
				copie_vaisseau v4 900 400|]
	done;;
(*liste des niveaux*)
let liste_v_niveaux=[| vaisseaux_niveau_1; vaisseaux_niveau_2;
vaisseaux_niveau_3|];;
let liste_p_tirs_niveau=[|puissance_tir_1; puissance_tir_2;
puissance_tir_3|];;

(*fonctions concernant les changemens de niveau*)

(*Vérification que tous les ennemis sont morts*)
let niveau_fini liste_vaisseaux=
	let res= ref true in
	begin
	for i=1 to (Array.length !liste_vaisseaux)-1 do
		let vaisseau= !liste_vaisseaux.(i) in
			res:= !res&&( not vaisseau.state)
	done;
	!res;
	end;;


(*changement de niveau*)
let changement_niveau lvl liste_vaisseaux liste_tirs=
	begin
	lvl:= !lvl+1;
	let joueur= !liste_vaisseaux.(0) in
	joueur.pv_tir_impact <- liste_p_tirs_niveau.(!lvl);
	liste_vaisseaux:= Array.append [|joueur|] liste_v_niveaux.(!lvl);
	liste_tirs:=[||];
	end;;
	
(*Pour recommencer du début*)
let rejouer liste_vaisseaux lvl liste_tirs points=
	begin
	points:=0;
	reinitialisation liste_v_niveaux;
	lvl:= 0;
	let joueur_= !liste_vaisseaux.(0) in
	joueur_.pv_tir_impact <- liste_p_tirs_niveau.(!lvl);
	joueur_.pv<-pv_de_base_joueur;
	joueur_.state<-true;
	liste_vaisseaux:= Array.append [|joueur_|] liste_v_niveaux.(!lvl);
	liste_tirs:=[||];
	end;;

(* pour accéder à u niveau aléatoire*)
let niveau_aleatoire liste_vaisseaux liste_tirs =
	begin
	liste_tirs:= [||];
	let joueur_= !liste_vaisseaux.(0) in
	joueur_.pv_tir_impact<- (int 10)+3;
	let max_vaisseau= int 5 +12  and nb_e_poss= Array.length ennemis_possibles in
	liste_vaisseaux:= Array.make max_vaisseau ennemis_possibles.(0);
	for i=0 to max_vaisseau -1 do
		!liste_vaisseaux.(i)<- copie_vaisseau ennemis_possibles.(int (nb_e_poss-1)) (int (size_x() -200) +100) (int (size_y() -200) +100)
	done;
	liste_vaisseaux:= Array.append [|joueur_|] !liste_vaisseaux;
	end;;


(*gestion des évènements *)
try 
	open_graph "1000x600+0";
	auto_synchronize false;
	fond 1000 600;
	let joueur={state=true; pv_de_base=pv_de_base_joueur;pv=pv_de_base_joueur; pv_tir_impact=5 ;periode_tir= 2.4;dernier_tir=280.9;centre= [|30;30|];origine =[|200;400|]; cote=20.; args_f=[|0.;0.|]; couleur=(200,10,30); f_deplacement=1; vitesse=10.; role='J'} in
	let liste_fonctions=[|"cercle";"triangle";"vague"|] and
	liste_vaisseaux= ref (Array.append [|joueur|] liste_v_niveaux.(0)) and
	liste_tirs=  ref [||] in
	let key = ref '5' and origine=Sys.time() and niveau= ref 0 and points = ref 0 in
	rejouer liste_vaisseaux niveau liste_tirs points;
	while true do
		key := if key_pressed () then read_key () else '5';
		if !key='a' then joue:=true;
		if !joue=true then
			begin
			if !key = 'e' then 
				begin
					joue:= false;
					raise Exit;
				end;
			if !key = 'y' then move_joueur 0. 1. !liste_vaisseaux.(0);
			if !key = 'g' then move_joueur (-.1.) 0. !liste_vaisseaux.(0);
			if !key = 'j' then move_joueur 1. 0. !liste_vaisseaux.(0);
			if !key = 'h' then move_joueur 0. (-.1.) !liste_vaisseaux.(0);
			if !key ='t' then tirer !liste_vaisseaux.(0) liste_tirs ((Sys.time()) -. origine);
			if !liste_vaisseaux.(0).state then
					if (niveau_fini liste_vaisseaux) then
						if (!niveau< (Array.length liste_v_niveaux)-1) then changement_niveau niveau liste_vaisseaux liste_tirs
						else 
							begin
								win(!points);
								if !key= 'r' then rejouer liste_vaisseaux niveau liste_tirs points;
								if !key = 'w' then niveau_aleatoire liste_vaisseaux liste_tirs;
							end
					else
						begin
							fond 1000 600;
							move_ennemi liste_fonctions (Array.length !liste_vaisseaux) ((Sys.time()) -. origine) liste_vaisseaux;
							trace_vaisseaux liste_vaisseaux (Array.length !liste_vaisseaux);
							avance_tir liste_tirs ((Sys.time()) -. origine);
							liste_tirs:=tirs_ennemi_regulier liste_vaisseaux ((Sys.time()) -. origine) liste_tirs;
							impact_tirs liste_tirs liste_vaisseaux points;
							tirs_valides liste_tirs;
							affiche_tir liste_tirs;
							affiche_barre_pv !liste_vaisseaux.(0);
							affiche_points points;
						end
			else 
				begin
					if !key= 'r' then rejouer liste_vaisseaux niveau liste_tirs points;
					lose !points;
				end;
			end
		else ecran_d_acceuil (size_x()) (size_y());
		synchronize ();
	done;
with
	_ -> close_graph ();;
