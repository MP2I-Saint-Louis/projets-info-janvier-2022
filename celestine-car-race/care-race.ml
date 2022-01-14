(*IMPORTATION DES MODULES NECESSAIRES*)
#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Unix;;

(*CREATION DES FONCTIONS D'AFFICHAGE*)

(*Design de la voiture des deux côtés*)
let voitureg x y couleur = let (a, b, c) = couleur in
	set_color (rgb 10 10 10);
	fill_circle (x + 10) (y + 7) 7;
	fill_circle (x + 40) (y + 7) 7;
	set_color (rgb 106 255 241);
	fill_rect (x + 30) (y + 27) 10 15;
	set_color (rgb 10 10 10);
	fill_circle (x + 35) (y + 32) 4;
	fill_rect (x + 34) (y + 27) 3 3;
	set_color (rgb a b c);
	fill_rect x (y + 12) 50 15;
	fill_rect (x + 10) (y + 27) 20 15;;

let voitured x y couleur = let (a, b, c) = couleur in
	set_color (rgb 10 10 10);
	fill_circle (x + 10) (y + 7) 7;
	fill_circle (x + 40) (y + 7) 7;
	set_color (rgb 106 255 241);
	fill_rect (x + 10) (y + 27) 10 15;
	set_color (rgb 10 10 10);
	fill_circle (x + 15) (y + 32) 4;
	fill_rect (x + 14) (y + 27) 3 3;
	set_color (rgb a b c);
	fill_rect x (y + 12) 50 15;
	fill_rect (x + 20) (y + 27) 20 15;;

(*Dessine une voiture blanche*)
let clean x y =
	set_color white;
	fill_circle (x + 10) (y + 7) 7;
	fill_circle (x + 40) (y + 7) 7;
	fill_rect (x + 10) (y + 27) 10 15;
	fill_circle (x + 15) (y + 32) 4;
	fill_rect (x + 14) (y + 27) 3 3;
	fill_rect x (y + 12) 50 15;
	fill_rect (x + 20) (y + 27) 20 15;;

(*Gestion des différents circuits en utilisant un indice de circuit*)
let decor dec =
	set_color blue;
	fill_rect 0 0 1200 800;
	if dec = 1 then begin
			set_color white;
			fill_rect 50 50 1100 700;
			set_color black;
			for i = 0 to 2 do
				for j = 0 to 4 do
					if i = 1 then (if j <> 7 then fill_rect (550 + 10 * i) (60 + j * 20) 10 10)
					else fill_rect (550 + 10 * i) (50 + j * 20) 10 10 done done;
			set_color blue;
			fill_rect 150 150 900 50;
			fill_rect 150 150 50 500;
			fill_rect 300 300 50 450;
			fill_rect 450 150 50 500;
			fill_rect 500 550 250 100;
			fill_rect 1000 150 50 500;
			fill_rect 850 300 50 450;
			fill_rect 600 300 250 150 end;
	if dec = 0 then begin
			set_color white;
			fill_rect 400 50 400 700;
			fill_circle 400 400 350;
			fill_circle 800 400 350;
			set_color black;
			for i = 0 to 2 do
				for j = 0 to 10 do
					if i = 1 then (if j <> 10 then fill_rect (550 + 10 * i) (60 + j * 20) 10 10)
					else fill_rect (550 + 10 * i) (50 + j * 20) 10 10 done done;
			set_color blue;
			fill_rect 400 250 400 300;
			fill_circle 400 400 150;
			fill_circle 800 400 150;
		end;
	if dec = 2 then begin
			set_color white;
			fill_rect 50 50 1100 700;
			set_color black;
			for i = 0 to 2 do
				for j = 0 to 4 do
					if i = 1 then (if j <> 7 then fill_rect (550 + 10 * i) (60 + j * 20) 10 10)
					else fill_rect (550 + 10 * i) (50 + j * 20) 10 10 done done;
			set_color blue;
			fill_rect 150 150 900 50;
			fill_rect 50 350 800 50;
			fill_rect 150 550 900 50;
			fill_rect 200 670 100 120;
			fill_rect 400 550 100 120;
			fill_rect 600 670 100 120;
			fill_rect 800 550 100 120;
			fill_rect 200 320 300 40;
			fill_rect 200 200 300 40;
			fill_rect 1000 150 50 450;
		end;;

(*Design de la page d'accueil*)
let start a =
	set_color blue;
	fill_rect 0 0 1200 800;
	set_text_size 50;
	moveto 250 550;
	set_color white;
	draw_string "Click anywhere to start";
	set_color (rgb 245 3 121);
	moveto 50 450;
	draw_string "Pink player uses:";
	moveto 260 350;
	draw_string "Z";
	moveto 200 300;
	draw_string "Q S D";
	set_color (rgb 255 224 0);
	moveto 600 450;
	draw_string "Yellow player uses:";
	moveto 820 350;
	draw_string "0";
	moveto 760 300;
	draw_string "K L M";
	for i = 0 to 6 do
		voitureg (i * 200 +25) 100 (245, 3, 121);
		voitured (i * 200 + 125) 100 (255, 224, 0);
		voitureg (i * 200 +25) 700 (245, 3, 121);
		voitured (i * 200 + 125) 700 (255, 224, 0) done;;

(*Design de la page de fin*)
let fin (a, b, c) =
	moveto 300 350;
	set_color blue;
	fill_rect 0 0 1200 800;
	set_text_size 100;
	set_color (rgb 255 244 0);
	(*On dessine aléatoirement des confettis*)
	for i = 0 to 100 do
		fill_rect (Random.int 1200) (Random.int 800) 10 10 done;
	set_color (rgb 245 3 121);
	for i = 0 to 100 do
		fill_rect (Random.int 1200) (Random.int 800) 10 10 done;
	set_color (rgb a b c);
	if a = 255 then draw_string "Yellow win";
	if a = 245 then draw_string "Pink win!!";
	set_color white;
	set_text_size 50;
	moveto 250 300;
	draw_string "Click anywhere to replay";;

(*Fonction qui affiche un décompte à l'écran*)
let decompte dec =
	set_color black;
	set_text_size 150;
	moveto 50 450;
	for i = 0 to 2 do
		draw_string (string_of_int (3 - i));
		draw_string "...";
		(*On attends une seconde puis on actualise l'affichage*)
		sleep 1;
		synchronize () done;
	moveto 500 300;
	sleep 1;
	draw_string "GO";
	synchronize ();
	sleep 1;;

(*CREATION DES FONCTIONS DE JEU*)

(*Fonction qui gère la majorité de la partie, c'est à dire le déplacement des voitures.*)
let rec mouvement_players x1 y1 x2 y2 i1 j1 i2 j2 dec =
	(*On définit les positions initiales des voitures en fonction du circuit actuel*)
	let y1_init = ref 0 and y2_init = ref 0 in
	if dec = 0 then begin y1_init := 98; y2_init := 138 end;
	if dec = 1 then begin y1_init := 58; y2_init := 98 end;
	if dec = 2 then begin y1_init := 58; y2_init := 98 end;

	(*On affiche le decor et les voitures à leur position initiale*)
	decor dec;
	voitured !x1 !y1_init (245, 3, 121);
	voitured !x2 !y2_init (255, 224, 0);

	(*On lance le decompte puis on affiche une version "propre" du circuit*)
	decompte dec;
	decor dec;
	voitured !x1 !y1_init (245, 3, 121);
	voitured !x2 !y2_init (255, 224, 0);

	(*On utilise une boucle toujours true qui nous permettra ainsi d'interrompre le programme en changeant juste sa valeur*)
	let boucle = ref true in
	while !boucle do
		(*on initialise notre timer*)
		let timer = gettimeofday () in
		let tmpx1, tmpx2, tmpy1, tmpy2 = !x1, !x2, !y1, !y2 in
		(*Lorsqu'une touche est pressée, cela change les directions verticales et horizontales*)
		if key_pressed () then begin
				let key = read_key () in

				if key = 'd' then begin i1 := 1; j1 := 0; end
				else if key = 's' then begin i1 := 0; j1 := (- 1); end
				else if key = 'q' then begin i1 := (- 1); j1 := 0; end
				else if key = 'z' then begin i1 := 0; j1 := 1; end
				else if key = 'm' then begin i2 := 1; j2 := 0; end
				else if key = 'l' then begin i2 := 0; j2 := (- 1); end
				else if key = 'k' then begin i2 := (- 1); j2 := 0; end
				else if key = 'o' then begin i2 := 0; j2 := 1; end;
			end;

		(*Si la case suivante est noire en arrivant par la gauche, on a gagné*)
		if point_color (!x1 - 20) !y1 = black then begin
			(*On affiche la page de fin et on attends jusqu'à ce que le joueur clique*)
				fin (245, 3, 121);
				synchronize ();
				boucle := false;
				let s = wait_next_event [Button_down] in
				(*Lorsque le joueur clique on relance la partie en appelant le décor suivant*)
				if s.button then begin
						if dec = 0 then mouvement_players (ref 450) (ref 58) (ref 450) (ref 98) (ref (- 1)) (ref 0) (ref (- 1)) (ref 0) ((dec + 1) mod 3);
						if dec = 1 then mouvement_players (ref 450) (ref 58) (ref 450) (ref 98) (ref (- 1)) (ref 0) (ref (- 1)) (ref 0) ((dec + 1) mod 3);
						if dec = 2 then mouvement_players (ref 450) (ref 98) (ref 450) (ref 138) (ref (- 1)) (ref 0) (ref (- 1)) (ref 0) ((dec + 1) mod 3);
					end
			end

		(*Le joueur avance de 12 pixels dans les directions actuelles*)
		else begin
				x1 := !x1 + (!i1) * 12;
				y1 := !y1 + (!j1) * 12;
				(*On dessine une voiture blanche a son ancien emplacement pour ne pas recharger tout le décor*)
				clean tmpx1 tmpy1;
				(*Si il est sur une case bleu, ses coordonées sont remises à 0*)
				if point_color !x1 !y1 = blue || point_color !x1 (!y1 + 42) = blue || point_color (!x1 + 50) !y1 = blue || point_color (!x1 + 50) (!y1 + 42) = blue || point_color (!x1 + 60) !y1 = black || point_color (!x1 + 70) (!y1) = black then
					begin clean tmpx1 tmpy1; x1 := 450; y1 := !y1_init; i1 := (- 1); j1 := 0; end
				
				(*On modifie la direction de la voiture en fonction de sa direction et on l'affiche*)
				else if (!i1) = (- 1) then
					voitured (!x1) (!y1) (245, 3, 121)
				else
					voitureg (!x1) (!y1) (245, 3, 121);
			end;

		(*De même pour la voiture jaune*)

		if point_color (!x2 - 20) !y2 = black then begin
				fin (255, 244, 0);
				synchronize ();
				boucle := false;
				let s = wait_next_event [Button_down] in
				if s.button then begin
						if dec = 0 then mouvement_players (ref 450) (ref 58) (ref 450) (ref 98) (ref (- 1)) (ref 0) (ref (- 1)) (ref 0) ((dec + 1) mod 3);
						if dec = 1 then mouvement_players (ref 450) (ref 58) (ref 450) (ref 98) (ref (- 1)) (ref 0) (ref (- 1)) (ref 0) ((dec + 1) mod 3);
						if dec = 2 then mouvement_players (ref 450) (ref 98) (ref 450) (ref 138) (ref (- 1)) (ref 0) (ref (- 1)) (ref 0) ((dec + 1) mod 3);
					end
			end

		else begin
				x2 := !x2 + (!i2) * 12;
				y2 := !y2 + (!j2) * 12;
				clean tmpx2 tmpy2;
				if point_color !x2 !y2 = blue || point_color !x2 (!y2 + 42) = blue || point_color (!x2 + 50) !y2 = blue || point_color (!x2 + 50) (!y2 + 42) = blue || point_color (!x2 + 60) !y2 = black || point_color (!x2 + 70) (!y2) = black then
					begin clean tmpx2 tmpy2;  x2 := 450; y2 := !y2_init; i2 := (- 1); j2 := 0; end
				else if (!i2) = (- 1) then
					voitured (!x2) (!y2) (255, 244, 0)
				else
					voitureg (!x2) (!y2) (255, 244, 0);
			end;
	
		(*On synchronise notre affichage*)
		synchronize ();
		(* On attends de manière à ce que peu importe la vitesse de calcul de la machine, les voitures aient la meme vitesse*)
		while (gettimeofday () -. timer) < 0.03 do
			() done;
	done;;

(*Lance une partie pour la première fois*)
let partie () =
	(*On créé la fenetre*)
	open_graph "1200x800+10+10";
	(*On préfère une synchronisation manuelle pour éviter les scintillements*)
	auto_synchronize false;

	(*On affiche la page d'acceuil tant que rien n'est cliqué*)
	while button_down () <> true do start "a"; synchronize () done;

	(*Enfin on lance une partie*)
	mouvement_players (ref 450) (ref 98) (ref 450) (ref 138) (ref (- 1)) (ref 0) (ref (- 1)) (ref 0) 0;;

partie ();;
