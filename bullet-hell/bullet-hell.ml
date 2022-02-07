#load "graphics.cma";;

open Graphics;;

let path="E:\\Prépa\\Info\\OCaml\\DM\\highscore.txt" ;;

type ship =   { mutable x : int; mutable y :int ; r : int  	
	; mutable dx : int ; mutable dy : int  ; mutable hpM : int ;mutable hpC : int ;
		mutable spM : int ; mutable spC: int} ;;  (* hpP -> health point max   hpC-> current health point 
							sp -> shield point*)

type projectile = { typeP : char ; mutable x : int; mutable y :int ; 
	 mutable dx : int ; mutable dy : int  ; mutable sizeX : int  ; mutable sizeY : int   ; 
	mutable exploded : bool ; damage : int
		} ;;  


type sfx ={ typeSFX : char ; mutable x : int  ; mutable y : int; mutable duree : float ; mutable tF : float ;
				mutable previousR : int};;





(* Valeur de départ*)

let deltaT=0.05;;
let deltaTWD=10.;;
let deltaTPD= 0.15;;   
let recoverSTD=7.5;;



let score=ref 0;;
let deltaTP= ref deltaTPD;;
let recoverST=ref recoverSTD;;
let deltaTW= ref deltaTWD;;
let lastHit=ref 0. ;;

let listeSFX=ref ({typeSFX='e'; x=0;y=0;duree=0.;tF=0.;previousR=0}::[]) ;;


let colorHull = rgb 50 255 50;;
let colorGlass = rgb 50 50 250;;



			(* Le vaisceau, définition et fonctions *)

let ship={ x= 0 ; y= 0 ; dx= 0; dy= 0; hpM =  100; hpC =  100 ; r=  20; spM= 10 ; spC=0};;

let d_ship ( )= begin    (* d comme draw *)
	
	set_color colorHull;
	fill_circle (ship.x) (ship.y) (ship.r);
	set_color colorGlass;
	fill_circle (ship.x)  (ship.y) ((ship.r)/2)
	end;;
	
(* e_nomDeL'Objet veut dire erase*)

let e_ship ( )= begin 
	
	set_color black;
	fill_circle (ship.x) (ship.y) (ship.r+5);

	end;;
	
let set_ship x y=begin

	e_ship ( );
	ship.x<-x;
	ship.y<-y;
	d_ship ( )
	end;;
	
let set_speed_ship dx dy=begin
	ship.dx<- dx;
	ship.dy<- dy;
	end;;
	


let move_ship () = begin
	e_ship ()  ;
	ship.x<- (ship.x)+ (ship.dx);
	ship.y<- (ship.y)+ (ship.dy);
	d_ship ()
	end;;

let speed_ship dx dy = begin
	ship.dx<- (ship.dx)+ dx;
	ship.dy<- (ship.dy)+ dy;
	end;;
	
let set_MHP hp = ship.hpM <- hp;;

let restore_HP () = ship.hpC <- (ship.hpM);;

let set_MSP sp= ship.spM <- sp;;

    (* Fonctions diverses pour controler le jeu *)

let restart ()= begin
	set_color black;				(* remet toutes les variables et la  *)
	fill_rect 0 0 ((size_x())) (size_y()) ;		(* fenetre graphique à l'état initial*) 
	set_MHP 100;						(* afin de relancer une partie  *)
	restore_HP () ;
	set_MSP 0;
	deltaTP:=deltaTPD;
	deltaTW:=deltaTWD;
	recoverST:=recoverSTD;
	listeSFX:=[];
	score:=0;
	set_color red;
	moveto (((size_x()))/7 ) ((size_y())/2);
	set_text_size 75;
	draw_string ("OBJECTIVE : SURVIVE!" );
	moveto (((size_x()))/6 ) ((size_y())-(size_y())/4);
	draw_string ("CLICK TO START" );

	end;; 







let depart  ( )= begin  (* même chose que restart mais ouvre la fenetre graphique *)
	open_graph "1500x800";
	auto_synchronize false;
	restart()
	end;;
	
let trie a n=
	let t=ref a.(0)
	and j=ref 0 in
	begin
	for i=1 to n do
		t:=a.(i);
		j:=i;
		let (_,ni)=a.(i) in
		while (!j>0 && let (_,nj1)=a.(!j-1) in ni<nj1) do
				a.(!j)<- a.(!j-1);
				j:= !j -1;
			done;
			
			a.(!j)<- !t;
		done
	end
			;;
 
(* Il faut changer le path *)	
let update_high_score score=
	let read=open_in path
	and scores=Array.make 11 ("",0) 
	and memeNom=ref false in
	 begin	 	
	 	for i=0 to 9 do
	 		scores.(i)<-Scanf.sscanf (input_line read) "%s : %d" (fun x y->(x,y)) ;
		done;
		
		let (nom,nScore)=score in
		for i=0 to 9 do
			let (nomi,scorei)=scores.(i) in
				if nom=nomi then begin scores.(i)<- (nom,max nScore scorei); memeNom:=true end;
		done;
		if !memeNom then scores.(10)<-("",0)
			else scores.(10)<-score;
		trie scores 10;
		close_in read;
		let write=open_out path in
		for i=1 to 10 do
			let (nom,scor) = scores.(i) in 
			let bytes=Bytes.of_string (nom^" : "^(string_of_int scor)^"\n") in 
			output write bytes 0 (Bytes.length bytes);
			

		done;
		flush_all ();
		close_out write;
		scores.(10);
	end
	
	
	;;

	
let display_score ()=
	let read=open_in path in
	 begin
	 	set_text_size 50;
	 	set_color black;
	 	fill_rect 0 0 (size_x()) (size_y());
	 	set_color red;	 	
	 	for i=0 to 9 do
	 		moveto ((size_x())/8) ((size_y())-(9-i)*50 - 100);
			let (nom,score)=Scanf.sscanf (input_line read) "%s : %d" (fun x y->(x,y))  in
			draw_string ((string_of_int (10-i))^" : "^nom^" "^(string_of_int score));
			 
		done;
		synchronize();
		close_in read;
	end
;;	


let game_over ()=	
	begin
	set_color black;         (* affiche l'écran de GAME OVER*)
	fill_rect 0 0 ((size_x())) (size_y()) ;
	set_color red;
	moveto (((size_x()))/8 ) ((size_y())/2);
	set_text_size 75;
	draw_string ("YOU SCORED : "^(string_of_int !score)^" POINTS");
	moveto (((size_x()))/8 ) ((size_y())-3*(size_y())/4);
	set_text_size 50;
	draw_string ("Enter your nickname then press SPACE");
	synchronize();
	let name = ref "" 
	and boucle=ref true in
	while !boucle do
		
		if key_pressed() then 
			begin
			let c=read_key() in 
			if c=' '||c='\013' then boucle:=false 
			else if c='\008' then name:=String.sub (!name) 0 ((String.length !name)-1)
			else name:= !name^(String.make 1 c);
			
			moveto (((size_x()))/8 ) ((size_y())-3*(size_y())/4 -75);
			set_color black;
			fill_rect (((size_x()))/8 ) ((size_y())-3*(size_y())/4 -105) 1000 100;
			set_color red;
			draw_string (!name);
			synchronize();
			end;
		done;
	moveto (((size_x()))/8 ) ((size_y())-(size_y())/4);
	let (master,highScore)=update_high_score (!name,(!score)) in
	draw_string ("High Score : "^(string_of_int highScore)^" POINTS BY "^master);
	set_color black;
	fill_rect 0 ((size_y())-3*(size_y())/4) (size_x()) 60;
	set_color red;
	moveto (((size_x()))/8 ) ((size_y())-3*(size_y())/4);
	draw_string "Press h to view the scoreboard";
	end
;;


let keyboard_handler k=			(* permet de pause avec p et de quitter avec q *)
	if k='P' || k='p' 
		then true
		else if k='q' || k='Q' then 
		begin
			close_graph(); false end
		else false
	;;
	
let mouse_control ()= let mx,my=mouse_pos () in begin		(* controle la vitesse du vaisseau grâce à la position relative de la souris par rapport au vaisseau*)
	ship.dx<- (mx- (ship.x))/4;
	ship.dy<- (my- (ship.y))/4
	end;;
	
	
	
	
			(*	SFX *)
			
			
let d_explosion x y r= begin 
	set_color (rgb 255 50 50);
	fill_circle x y r;
	set_color (rgb 255 140 20);
	fill_circle  x y (r/2)
	end;;
	
let e_explosion x y r= 
	begin 
		set_color black;
		fill_circle x y (r+1);
	end;;

let d_sfx sfx=match sfx.typeSFX with		(* dessine tout les types de sfx (il n'y a que les explosions actuellement)*)
	|'e'-> let r=int_of_float ((sfx.tF -. Sys.time())/. sfx.duree *. 25.) in begin 
				d_explosion sfx.x sfx.y r; 
				sfx.previousR<-r end
	|_ -> ();;

let e_sfx sfx=match sfx.typeSFX with
	|'e'-> e_explosion sfx.x sfx.y sfx.previousR
	|_ -> ();;
	
let update_sfx sfx= begin  
		e_sfx sfx;
		d_sfx sfx
	end;;

let rec sfxs listeS = match listeS with	(* dessine toutes les sfx de la liste de sfx et supprime ceux qu'il faut supprimer, renvoie la nouvelle liste de sfx*)
	|sfx::queue when sfx.tF < Sys.time() -> begin e_sfx sfx; sfxs queue end
	|sfx::queue ->begin update_sfx sfx; sfx::(sfxs queue) end
	|[]->[];;

let create_explosion	x y = 
	{ typeSFX='e' ;x=x ; y=y; duree=0.5; tF = Sys.time() +. 0.5; previousR=1} ;;



	
	
	
	
	
	
		(* Projectiles *)
		
	(* Les coordonees des projectiles sont en bas à gauche*)
	

	(* Missiles *)
	
	
  (* 4 fonctions pour afficher les différentes directions des missiles *)
let d_missileB x y = 
	begin 
	set_color red;
	fill_poly [|(x,y);(x+7,y+10);(x+15,y)|];
	set_color white;
	fill_rect (x+5) y 5 25;
	set_color red;
	fill_poly [|(x+3,y+25);(x+8,y+35);(x+12,y+25)|]
	end;;

	
let e_missileB x y = 
	begin 
	
	set_color black;
	fill_poly [|(x,y);(x+7,y+10);(x+15,y)|];
	fill_rect (x+5) y 5 25;
	fill_poly [|(x+3,y+25);(x+8,y+35);(x+12,y+25)|]
	end;;
	
let d_missileH x y = 
	begin 
	set_color red;
	fill_poly [|(x,y+35);(x+7,y+25);(x+15,y+35)|];
	set_color white;
	fill_rect (x+5) (y+10) 5 25;
	set_color red;
	fill_poly [|(x+3,y+10);(x+8,y);(x+12,y+10)|]
	end;;
	
	
let e_missileH x y = 
	begin 
	
	set_color black;
	fill_poly [|(x,y+35);(x+7,y+25);(x+15,y+35)|];
	fill_rect (x+5) (y+10) 5 25;
	fill_poly [|(x+3,y+10);(x+8,y);(x+12,y+10)|]
	end;;
	
	
	
let d_missileR x y = 
	begin 
	set_color red;
	fill_poly [|(x+35,y);(x+25,y+7);(x+35,y+15)|];
	set_color white;
	fill_rect (x+10) (y+5) 25 5;
	set_color red;
	fill_poly [|(x+10,y+3);(x,y+8);(x+10,y+12)|]
	end;;
	
	
let e_missileR x y = 
	begin 
	set_color black;
	fill_poly [|(x+35,y);(x+25,y+7);(x+35,y+15)|];
	fill_rect (x+10) (y+5) 25 5;
	fill_poly [|(x+10,y+3);(x,y+8);(x+10,y+12)|]
	end;;
	

	
let d_missileL x y = 
	begin 
	set_color red;
	fill_poly [|(x,y);(x+10,y+7);(x,y+15)|];
	set_color white;
	fill_rect x (y+5) 25 5;
	set_color red;
	fill_poly [|(x+25,y+3);(x+35,y+8);(x+25,y+12)|]
	end;;
	
	
let e_missileL x y = 
	begin 
	set_color black;
	fill_poly [|(x,y);(x+10,y+7);(x,y+15)|];
	fill_rect x (y+5) 25 5;
	fill_poly [|(x+25,y+3);(x+35,y+8);(x+25,y+12)|]
	end;;
	
	
	(* lance un missile dans une direction aléatoire avec une position aléatoire
	 et une vitesse aléatoire dans une certaine plage 	  *)
let launch_missile listeP speed damage= let missile={typeP='m'; x= 0; y=  0; dx=  0;
					dy=  0;  sizeX=  15; sizeY=  35; exploded= false ; damage= damage} in 
		begin
			if (Random.int 4)=0 then begin missile.dx <- ((Random.int 16+speed)+5+speed) ; missile.y<- (Random.int (size_y())); missile.sizeX<-35 ; missile.sizeY<- 15 end
				else if (Random.int 3)=0 then begin missile.dx <- -((Random.int 16+speed)+5+speed) ; missile.x<- ((size_x())) ;missile.y<- (Random.int (size_y())) ; missile.sizeX<-35 ; missile.sizeY<- 15 end
					else if (Random.int 2)=0 then begin missile.dy <- ((Random.int 16+speed)+5+speed) ; missile.x<- (Random.int ((size_x()))) end
						else begin missile.dy <- -((Random.int 16+speed)+5+speed) ; missile.x<- (Random.int (size_x())) ;missile.y<- (size_y()) end;
			
			missile::listeP 
		end;;
	
	
	(* permet de dessiner n'importe quel projectile (meme si il n'y a que des missiles pour l'instant*)
let d_proj proj = match (proj.typeP) with
	|'m' when (proj.dx)>0-> d_missileL (proj.x) (proj.y)  
	|'m' when (proj.dx)<0-> d_missileR (proj.x) (proj.y) 
	|'m' when (proj.dy)>0-> d_missileB (proj.x) (proj.y) 
	|'m' when (proj.dy)<0-> d_missileH (proj.x) (proj.y) 
	|_ -> ();;
	
let e_proj proj = match (proj.typeP) with
	|'m' when (proj.dx)>0-> e_missileL (proj.x) (proj.y)   
	|'m' when (proj.dx)<0-> e_missileR (proj.x) (proj.y) 
	|'m' when (proj.dy)>0-> e_missileB (proj.x) (proj.y) 
	|'m' when (proj.dy)<0-> e_missileH (proj.x) (proj.y) 
	|_ -> ();;

(* deplace un projectile*)
let move_proj proj=begin 
	e_proj proj;
	proj.x<-(proj.x)+ (proj.dx);
	proj.y<-(proj.y)+ (proj.dy);
	d_proj proj
	end;;
	(* teste les collisions et renvoie un bool *)
let hit_proj proj=  ((ship.x) - ship.r ) < (proj.sizeX + proj.x)   && ((ship.x) + ship.r ) > ((proj.x)) 
						&&  ((ship.y) - ship.r ) < ((proj.y) + (proj.sizeY)) && ((ship.y)+ ship.r ) > ((proj.y));;


(* fait tout ce qu'il y à faire quand on touche un projectile *)
let impact proj= begin
	ship.spC<- (ship.spC) - (proj.damage);
	if ship.spC < 0 then 
		begin 
			ship.hpC<- (ship.hpC) + (ship.spC);
			ship.spC<- 0
		end;
	proj.exploded<- true;
	lastHit:= Sys.time();
	listeSFX:= (create_explosion proj.x proj.y):: !listeSFX;
	e_proj proj;
	end
;;
	
(* deplace tout les projectiles de la liste et renvoie la liste sans les projectiles inutiles*)
let rec projectiles listeP =  match listeP with
	| proj::queue when (proj.exploded) -> projectiles queue
	| proj::queue -> if (hit_proj proj) 
		then 	begin impact proj; proj::( projectiles queue) end
		else 	begin 
			if (proj.x)>((size_x())+10) ||  (proj.x)<(-50) || (proj.y)>((size_y())+10) || (proj.y)<(-10) then proj.exploded<-true ;
			move_proj proj ; proj::( projectiles queue) end 
	| [] -> [];;
	

	

		(* UI *)

let health_bar ()= begin
	set_color (rgb 100 100 100);
	fill_rect 0 0 (size_x()) 50 ;
	set_color green;
	fill_rect 0 0 ( (ship.hpC) * (size_x()) / (ship.hpM)) 50
	end;;
	
let shield_bar ()= begin 
	set_color (rgb 100 100 100);
	fill_rect 0 ((size_y())-50) (size_x()) 50 ;
	set_color blue;
	try fill_rect 0 ((size_y())-50) ( (ship.spC) * (size_x()) / (ship.spM)) 50 with Division_by_zero-> ()
	end;;

let update_score nWave= score:= !score + nWave;;
	
let draw_score ()=
begin 
	
		set_color red;
		moveto 10 (10);
		set_text_size 30;
		draw_string (string_of_int(!score));
	end
 ;;
 
 let draw_wave nWave = 
		begin
		set_color red;
		moveto ((size_x())-150) (10);
		set_text_size 30;
		draw_string ("Wave :"^string_of_int(!nWave));
		end;;
 

	(* Main *)


let jeu ( )= begin
	restart() ;
	synchronize ();
	let t=ref (Sys.time())  (* variables de temps pour le déroulement de la plupart du déroulement du jeu *)
	and dt= ref 0. 
	and listeP=ref ([]) (* la listeP est la liste de projectiles *)
	and dtW=ref 0.  (* variables de temps pour le lancement des vagues*)
	and tW= ref (Sys.time()) 
	and fire=ref true  (* est ce que on tire des projectiles ou pas *)
	and start =ref false (* est ce que le jeu commence ou pas *)
	and dtP=ref 0. 
	and tP=ref (Sys.time ())	(* variables de temps pour le lancement de la projectile *)
	and damageM=ref 10  (* dégats des missiles*)
	and speed= ref 0	
	and nWave=ref 1 
	and playing= ref true 
	and pause=ref false in
	while !playing do		(* regarde si on a perdu *)
		if !start && not !pause then begin 
			dt:= Sys.time() -. !t;		(* met à jour les variables de temps *)
			dtW := Sys.time() -. !tW;
			dtP := Sys.time() -. !tP;
			
			if key_pressed() then  if keyboard_handler (read_key()) then pause:=true;
					(* lancement des projectiles *)							
			if (!fire && !deltaTP <= !dtP)
						then begin listeP:=launch_missile !listeP !speed !damageM; 
							update_score !nWave; 
							tP:=Sys.time () end;
			
				
			if deltaT <= !dt 
				then begin 
					if Sys.time() -. !lastHit > !recoverST && ship.spC<ship.spM then ship.spC<- ship.spC +1; (* recharge le bouclier si on ne se fait pas toucher pendant plus de 10s *)
					move_ship () ; 
					mouse_control ();
					listeP:=projectiles (!listeP);
					t:=Sys.time() ;
					listeSFX:=sfxs !listeSFX;
					if ship.hpC <=	0 then begin
							game_over ();
							playing:=false;
							ship.hpC<- 0;
						end;
					health_bar ( ) ;
					shield_bar ();
					draw_score () ;		
					draw_wave nWave;		
					
					synchronize ()
				end;
			
			if (!deltaTW -. !deltaTW/.3.) <= !dtW then fire:=false; (* cesser le feu avant la fin de la vague *)
			
			
			
			if !deltaTW <= !dtW then		(* gestion des vagues *)
				begin 
					set_color red;
					moveto ((size_x())/8 ) ((size_y())/2);
					set_text_size 100;
					draw_string "NEXT WAVE INCOMING";
					if !deltaTW +. !deltaTW/. 5. <= !dtW then begin 
						fire:=true;
						deltaTW:= !deltaTW +. Random.float 2.;
						set_color black;
						fill_rect 0 0 (size_x()) (size_y()) ;
						nWave:= !nWave + 1;
						damageM:= !damageM + 1;
						deltaTP:= !deltaTP /. 1.1;
						if !nWave mod 5 =0 then begin speed:= !speed +2; ship.spM<- ship.spM +10 end;
						tW :=  Sys.time()
						end ;
					
					end; 		
			end 
			else if button_down () then begin  (*Lancement du jeu*)
				start:=true ;
				set_color black;
				fill_rect 0 0 (size_x()) (size_y()) ;
				tW:=Sys.time()
				end
			else if key_pressed() then if keyboard_handler (read_key()) then begin 
				pause:=false;			(*pause*)
				tP:=Sys.time()-. !dtP;
				tW:=Sys.time()-. !dtW;
				end
	done 
	end;;		

let lance_jeu()=begin
	depart();				(* permet de rejouer *)
	while true do
		jeu();
		while not (button_down()) do 
			if key_pressed() then if read_key()='h' || read_key()='H' then
				display_score();
		done;
	done
	end;;


lance_jeu();;		


