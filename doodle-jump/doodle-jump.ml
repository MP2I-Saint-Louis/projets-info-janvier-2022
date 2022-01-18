(*Version plus aboutie avec des monstres, mais qui présente quelques problèmes
dans le test pour savoir si le monstre a été touché*)

#load "graphics.cma";;
open Graphics;;
exception Perdu;;

(* création personnage *)

type perso = {mutable xg : int; mutable yg : int; mutable pt : int; mutable c : color};;

(* configuration plateformes *)

type plateforme = {mutable x: int; mutable y: int; taille: int; pv: int (*type de plateforme*); mutable dir: int};;

(* différents types (pv) de plateforme :
		- classique : grise -> 0
		- cassée : rouge -> 1 
		- super saut : bleue -> 2
		- deplacement x : vert -> 3
		- monstre -> 4 *)

let proba_pltf n perso = 
(* Cette fonction prend en argument le personnage et une dénominateur pour calculer des probabilités.
ELle renvoie le numero de la plateforme à ajouter selon des probabilités *)
	let p = (Random.int n) 
	and frq_r = perso.pt/200+1 in
	if p = 1 || p = 2 || p = 3
	then 2
	else
		if p = 4 || p = 5
		then 3
		else 
			if p = 6
			then 4
			else 
				if  p <= frq_r+6 && p <= 35
				then 1
				else 0;;

let creation_plateforme n perso = 
(* Cette fonction prend en arguement la hauteur de la plateforme à créer et le personnage.
Elle renvoie la plateforme nouvellement créée *)
	 let taille = (Random.int 40) + 80 in
	 let x = Random.int (size_x()-taille) in
	 let y = n in
	 let pv = (proba_pltf 41 perso) in
	 let dir = ref 0 in
	 if pv = 3 || pv = 4
	 then dir := 1;
	 if perso.pt=0 && pv=4
	 then {x=x; y=y; taille=taille; pv=1 ; dir=(!dir)} 
	 else {x=x; y=y; taille=taille; pv=pv ; dir=(!dir)} ;;

let rec supp_plateforme l_pltf = match l_pltf with
(* Cette fonction récursive prend en arguemnt une liste de plateformes.
ELle renvoie cette liste après avoir supprimé les plateformes non visibles (y<0) et déjà  parcourues par le personnage *)
| [] -> []
| tete::queue when (tete.pv = 4 && tete.y+40 < 0) || (tete.y+10 < 0) -> supp_plateforme queue
| tete::queue -> tete::supp_plateforme queue ;;

let ajoute_plateforme l_pltf n perso = 
(* Cette fonction prend en argument la hauteur de la plateforme à ajouter à la liste et le personnage.
Elle renvoie la liste après ajout de la nouvelle plateforme et suppression des plateformes inutiles *)
	(creation_plateforme n perso):: (supp_plateforme l_pltf);;

(* Decalage des plateformes *)

let rec decale_pas l_pltf pas = match l_pltf with
(* Cette fonction récursive prend en argument la liste des plateformes et le pas de déplacement.
Elle renvoie la liste des plateformes après décalage de chaque plateforme d'un certain pas *)
| [] -> ()
| tete::queue -> 
	begin 
		tete.y <- tete.y-pas;
		decale_pas queue pas
	end;;

(* Affichage *)

let affiche_perso perso = let x,y=perso.xg,perso.yg in
(* Cette fonction prend en argument le personnage.
Elle renvoie un unit et affiche le personnage à partir des coordonnées de son pied gauche *)
	set_color perso.c;
	fill_circle (x+6) (y+42) 12;
	moveto x y;
	lineto x (y+14);
	fill_rect x (y+14) 12 20;
	moveto (x+11) y;
	lineto (x+11) (y+14);
	moveto x (y+26);
	lineto (x-12) (y+26);
	moveto (x+12) (y+26);
	lineto (x+24) (y+26);
	set_color (rgb 0 0 0);
	fill_circle (x+1) (y+43) 2;
	fill_circle (x+11) (y+43) 2;
	plot (x+1) (y+38);
	plot (x+11) (y+38);
	moveto (x+2) (y+37);
	lineto (x+11) (y+37);;

let affiche_monstre monstre = 
(* Cette fonction prend en argument la plateforme de pv 4 : monstre.
Elle renvoie un unit et affiche la plateforme monstre *)
	let x,y,t = monstre.x,monstre.y,monstre.taille in
	set_color (rgb 100 50 141);
	fill_rect x y t 39;
	set_color white;
	fill_circle (x+t/2-20) (y+20) 7;
	fill_circle (x+t/2+20) (y+20) 7;
	set_color black;
	fill_circle (x+t/2-20) (y+20) 3;
	fill_circle (x+t/2+20) (y+20) 3;
	moveto (x+t/2-10) (y+23);
	lineto (x+t/2-30) (y+33);
	moveto (x+t/2+10) (y+23);
	lineto (x+t/2+30) (y+33);
	moveto (x+t/2-10) (y+24);
	lineto (x+t/2-30) (y+34);
	moveto (x+t/2+10) (y+24);
	lineto (x+t/2+30) (y+34);
	fill_rect (x+t/2-10) (y+7) 20 3;
	fill_rect (x+t/2-13) (y+4) 3 3;
	fill_rect (x+t/2+10) (y+4) 3 3;
	moveto x (y+40);
	lineto (x+t) (y+40);;

let couleur_pltf pltf = 
(* Cette fonction prend en argument une plateforme.
Elle renvoie la couleur de la plateforme pour l'affichage *)
	if pltf.pv = 0 
		then set_color (rgb 95 114 150);
	if pltf.pv = 1 
		then set_color (rgb 213 44 75);
	if pltf.pv = 2
		then set_color (rgb 75 207 244);
	if pltf.pv = 3
		then set_color (rgb 120 224 117);;

let affiche_pltf pltf = 
(* Cette fonction prend en argument une plateforme.
Elle renvoie un unit et affiche la plateforme de la couleur associée à son type*)
	if pltf.pv = 4 
	then affiche_monstre pltf
	else begin
		couleur_pltf pltf;
		fill_rect pltf.x pltf.y pltf.taille 9;
		set_color (rgb 0 0 0);
		moveto pltf.x (pltf.y+9);
		lineto (pltf.x+pltf.taille) (pltf.y+9);
		end;;

let rec affiche_liste_pltf l_pltf = match l_pltf with
(* Cette fonction récursive prend en arguemnt la liste des plateformes.
Elle renvoie un unit et affiche chaque plateforme *)
| [] -> ();
| tete::queue -> 
	begin 
		affiche_pltf tete;
		affiche_liste_pltf queue;
	end;;

let affiche_pt perso = 
(* Cette fonction prend en argument le personnage.
Elle renvoie un unit, affiche les points du joueur et change la couleur du personnage*)
	if perso.pt mod 500 <= 30 && perso.pt > 30
	then perso.c <- rgb ((Random.int 216)+30) ((Random.int 216)+30) ((Random.int 216)+30)
	else set_color (rgb 0 0 0);
	moveto 30 (size_y()-60);
	set_text_size 50;
	set_color perso.c;
	draw_string (string_of_int perso.pt);;

let affiche perso l_pltf =
(* Cette fonction prend en argument le personnage et la liste des plateformes.
Elle renvoie un unit et affiche les points du joueur, le personnage et les plateformes *)
	clear_graph();
	affiche_perso perso;
	affiche_liste_pltf l_pltf;
	affiche_pt perso;
	synchronize();;

(* Defaite *)

let perdu perso = 
(* Cette fonction prend en argument le personnage.
Elle renvoie un unit et affiche la page de fin (avec le score et des consignes) après défaite en attendant une action du joueur *)
	clear_graph();
	moveto (size_x()/2 -150) (size_y() - 80);
	set_text_size 40;
	set_color (perso.c);
	draw_string "Doodle Jump";
	set_color (rgb 0 0 0);
	moveto 20 (size_y()-170);
	draw_string ("Score final : ");
	set_color perso.c;
	draw_string (string_of_int perso.pt);
	moveto 20 (size_y()-210);
	set_text_size 25;
	if perso.pt < 1000
	then begin
		draw_string ("Continue comme ça, tu peux t'améliorer");
		if perso.pt = 0
		then begin
			set_text_size 20;
			set_color black;
			moveto 25 (size_y()-250);
			draw_string ("Petit rappel en cas de besoin :");
			moveto 25 (size_y()-280);
			draw_string ("Appuie sur Q pour aller vers la gauche");
			moveto 25 (size_y()-310);
			draw_string ("Appuie sur D pour aller vers la droite");
			moveto 25 (size_y()/2-50);
			set_color perso.c;
			draw_rect 10 (size_y()-320) (size_x()-20) 100;
			end;
		end
	else begin
		if perso.pt > 2500
		then draw_string("Tu vas devenir un vrai champion !")
		else draw_string ("Bien joué !")
		end;
	moveto 20 (size_y()/2);
	set_text_size 25;
	set_color (rgb 0 0 0);
	draw_string ("Prêt pour une nouvelle partie ?");
	moveto 20 (size_y()/2 -35);
	draw_string ( "Appuie sur S");
	moveto 20 (size_y()/2 -70);
	draw_string ("Sinon appuie sur Z, à la prochaine !");
	synchronize() ;;

(* Mouvement et test des différentes plateformes *)

let rec mvt_pltf_x l_pltf = match l_pltf with
(* Cette fonction récursive prend en argument la liste des plateformes.
Elle parcourt la liste et teste chaque plateforme. Si celle-ci est de pv 3 ou 4, elle modifie ses coordonnées selon x et renvoie la liste *)
| [] -> []
| tete :: queue when tete.pv = 3 || tete.pv = 4 -> 
	begin
	if tete.x = 0 || tete.x = size_x()-tete.taille
	then tete.dir <- (-tete.dir);
	tete.x <- tete.x + tete.dir;
	tete :: mvt_pltf_x queue;
	end
| tete :: queue -> tete :: mvt_pltf_x queue;;

let rec test_color_b perso l_pltf = match l_pltf with
(* Cette fonction récursive prend en argument la liste des plateformes et le personnage.
Elle teste chaque plateforme et renvoie le multiple de hauteur du saut, si pv=2 et le personnage est dessus, elle renvoie 3 sinon 1 *)
| [] -> 1
| pltf::queue when pltf.y+9 = perso.yg && pltf.pv = 2 -> 3
| pltf::queue -> test_color_b perso queue;;

let rec test_color_r perso l_pltf = match l_pltf with
(* Cette fonction récursive prend en argument la liste des plateformes et le personnage.
Elle teste chaque plateforme et renvoie la liste après suppression de la plateforme de pv=1 lorsque le personnage l'a touchée *)
| [] -> []
| pltf::queue when pltf.y+9 = perso.yg && pltf.pv = 1 -> queue
| pltf::queue -> pltf :: (test_color_r perso queue);;

let rec test_monstre perso l_pltf = match l_pltf with
(* Cette fonction récursive prend en argument la liste des plateformes et le personnage.
Elle teste chaque plateforme et renvoie une exception si le personnage touche un monstre (plateforme pv=4) *)
| [] -> ()
| tete :: queue when tete.pv = 4 -> 
	begin
		if perso.xg+18>tete.x && perso.xg-6<tete.x+tete.taille && perso.yg+54>tete.y && perso.yg<tete.y+40
		then raise Perdu
		else test_monstre perso queue;
	end;
| tete :: queue -> test_monstre perso queue;;

(* Deplacment selon x du personnage *)

let action perso c =
(* Cette fonction prend en argument le personnage et le caractère de la touche pressée par le joueur.
Elle renvoie un unit et modifie les coordonées selon x du personnage *)
		if c = 'q' || c = 'Q'
		then perso.xg <-perso.xg-7;
		if c = 'd' || c = 'D'
		then perso.xg <- perso.xg+7;
		if perso.xg >= size_x()
		then perso.xg <- perso.xg - size_x();
		if perso.xg <= 0
		then perso.xg <- perso.xg + size_x();;

(* Deplacement selon y du personnage *)

let montee perso liste = 
(* Cette fonction prend en argument le personnage et la liste des plateformes.
Elle renvoie cette liste après modification des coordonées y du personnage ou des plateformes *)
	let deltaT = ref 0.0055
	and t = ref (Sys.time())
	and pas = ref 3
	and l_pltf = ref liste in
	let high = (test_color_b perso !l_pltf) in
	for i = 1 to high*120 do
	begin 
		while !deltaT >= (Sys.time() -. !t) do
		()
		done;
		(*ralentissement du saut*)
		if i>45*high 
		then pas := 2;
		if i>=95*high
		then pas := 1;
		t:= Sys.time();
		if key_pressed()
		then action perso (read_key());
		if perso.yg < size_y()/2
		then begin (*si personnage est en dessous de la moitié, il monte*)
			perso.yg <-perso.yg+ !pas;
			l_pltf := mvt_pltf_x !l_pltf;
			test_monstre perso !l_pltf;
			end
		else
			begin (*sinon les plateformes descendent*)
			decale_pas !l_pltf !pas;
			l_pltf := mvt_pltf_x !l_pltf;
			if i mod 40 = 0 && (List.hd !l_pltf).y +10 < (size_y()*4/5)
			then begin
				l_pltf := ajoute_plateforme !l_pltf (size_y()*4/5) perso;
				perso.pt <- perso.pt+10;
			end
			end;
		if i mod 2 = 0
		then affiche perso !l_pltf;
	end
	done;
	!l_pltf;;

(* Fonction de déroulement de la partie *)

let start perso liste =
(* Cette fonction prend en argument le personnage et la liste des plateformes.
Elle renvoie  un unit et déroule la partie jusqu'à une défaite *)
	let deltaT = 0.0045
	and t = ref (Sys.time())
	and l_pltf = ref [] 
	and n = ref 50
	and defaite = ref false
	and i = ref 0 in
	(* réinitialisation du personnage *)
	perso.xg <- size_x()/2;
	perso.yg <- 20;
	perso.pt <- 0;
	(* redéfinition des plateformes *) 
	l_pltf:= {x=(size_x()/2)-50; y=0; taille=100; pv=0; dir=0}::!l_pltf;
	while !n< size_y() do
	begin
		l_pltf := ajoute_plateforme !l_pltf !n perso;
		n:=!n+50;
	end;
	done;
	affiche perso !l_pltf;
	while perso.yg >0 && !defaite=false do
	if deltaT <= Sys.time() -. !t
	then try begin
		if point_color perso.xg perso.yg = (rgb 0 0 0) || point_color (perso.xg+11) perso.yg = (rgb 0 0 0)
		then 
			begin 
			l_pltf := test_color_r perso !l_pltf;
			l_pltf := montee perso !l_pltf;
			
			end
		else 
			begin
				perso.yg <- perso.yg-1;
				l_pltf := mvt_pltf_x !l_pltf;
				test_monstre perso !l_pltf;
				if !i mod 2 = 0
				then affiche perso !l_pltf;
			end;
		t := Sys.time();
		if key_pressed()
		then action perso (read_key());
		i:=!i+1;
	end;
	with | Perdu -> defaite:=true;
	done;
	perdu perso;;

(* Redémarrage et initialisation d'une artie *)

let recommence c perso l_pltf = 
(* Cette fonction prend en argument le personnage, la liste des plateformes et le caractère de la touche pressée.
Elle renvoie un unit ou ferme la fenêtre graphique *)
	if c = 's' || c = 'S'
	then begin
		affiche perso l_pltf;
		start perso l_pltf;
		end;
	if c = 'z' || c = 'Z'
	then close_graph();;

let init() = 
(* Cette fonction ne prend aucun argument.
Elle lance la partie et se déroule tant que la fenêtre graphique est ouverte *)
	open_graph "700x800+350+10" ;
	auto_synchronize false;
	let partie_en_cours = ref true
	and l_pltf = ref [] 
	and n = ref 50 
	and perso = { xg= size_x()/2; yg= 20; pt=0; c=(rgb 97 224 209)} in
	(*definition des plateformes initiales*)
	l_pltf:= {x=(size_x()/2)-50; y=0; taille=100; pv=0; dir=0}::!l_pltf;
	while !n< size_y() do
	begin
		l_pltf := ajoute_plateforme !l_pltf !n perso;
		n:=!n+50;
	end
	done;
	(* page d'accueil *)
	clear_graph();
	moveto (size_x()/2 -150) (size_y() - 80);
	set_text_size 40;
	set_color perso.c;
	draw_string "Doodle Jump";
	set_color (rgb 0 0 0);
	moveto 20 (size_y()/2);
	set_text_size 25;
	draw_string ("Prêt à jouer, appuie sur S !");
	moveto 25 (size_y()-170);
	draw_string ("Quelques règles avant de démarrer :");
	moveto 25 (size_y()-210);
	draw_string ("Appuyer sur Q pour aller vers la gauche");
	moveto 25 (size_y()-250);
	draw_string ("Appuyer sur D pour aller vers la droite");
	moveto 25 (size_y()/2-50);
	set_color perso.c;
	draw_rect 10 (size_y()-290) (size_x()-20) 170;
	synchronize();
	while !partie_en_cours do
	begin
		if key_pressed()
		then recommence (read_key()) perso (!l_pltf);
	end
	done;;

init();;


