open Graphics;;
		
let show_point mi rayon color =(*
	Affiche le point mi de la couleur color avec le bon rayon *)
	(set_color color;
	fill_circle (coord_x mi) (coord_y mi) rayon);;

let show_point_str mi color = (*
	affiche le nombre du point mi de la couleur color *)
	(set_text_size 3;
	moveto (coord_x mi) (coord_y mi); 
	draw_string (string_of_int mi));;
	
let show_debut_fin ci color = (*
	affiche debut ci et fin ci aux points mdebut et mfin du client ci *)
	let mdebut = debut_client ci and
	mfin = arrivee_client ci in
	(set_text_size 10;
	set_color color;
	moveto (coord_x mdebut) (coord_y mdebut);
	draw_string ("debut " ^ (string_of_int ci));
	moveto (coord_x mfin) (coord_y mfin);
	draw_string ("fin " ^ (string_of_int ci)));;

let show_points tab rayon color montrer_str = 
	(* affiches les points de la couleur et au rayon demandé contenus dans un tableau tab
	avec tab = int*int array
	affiche le numero de mi ssi montrer_str *)
	let n = Array.length tab in 
	(set_text_size 3;
	set_color color;
	for mi = 0 to n -1 do
		show_point mi rayon color;
		if montrer_str
		then show_point_str mi color
	done );;

let show_route mi mk color = (*
	 Affiche la route ie le segment allant de mi a mk de la couleur color *)
	(set_color color ;
	moveto (coord_x mi) (coord_y mi);
	lineto (coord_x mk) (coord_y mk));;
	
let rec show_trajet (trajet : int list) color = 
	(* Affiche les routes pour mi et mj consecutifs dans trajet
	Permet d'afficher un ensemble de segments donc un trajet *)
	match trajet with 
	|[] -> ()
	|m2::[] -> ()
	|m1::m2::reste_trajet -> 
		show_route m1 m2 color;
		show_trajet (m2::reste_trajet) color;;

let show_routes color  = (*
	affiche en color toutes les routes contenues dans la matrice route *)
	let n = Array.length route in 
	(set_color color;
	for mi = 0 to n-1 do	
		for mk = 0 to n-1 do
			if route.(mi).(mk) < infinity
			then
			   show_route mi mk color
		done
	done);;
	
let show_ponts color width  = (*
	Affiches les routes contenues dans pont de la couleur color et la largeur width *)
	let n = Array.length route in 
	(set_color color;
	set_line_width width;
	for mi = 0 to n-1 do	
		for mk = 0 to n-1 do
			if pont.(mi).(mk) < infinity
			then
			   show_route mi mk color
		done
	done);;
	
let show_connexe mi color = (*
	Affiche les points accessibles depuis mk de la couleur color et de rayon 2*)
	for mj = 0 to !nb_points -1 do
		if tab_trajets.(mi).(mj) < infinity 
		then show_point mj 3 color
	done;;
	
let show_dijkstra mdebut mfin color = (*
	Affiche le trajet obtenus par Dijkstra *)
	show_trajet (dijkstra mdebut mfin) color;;

let show_a_etoile mdebut mfin color = (*
	Affiche le trajet obtenus par A* *)
	show_trajet (a_etoile mdebut mfin) color;;
	
let show_traitement_dijkstra mdebut mfin rayon color = (*
	Affiche les points parcourus par Dijkstra *)
	let deja_parcourus = Array.make !nb_points false and
	parent = Array.make !nb_points (-1) and
	distance_reelle = Array.make !nb_points (infinity) in
	(dijkstra_aux mdebut mfin deja_parcourus parent distance_reelle;
	for mi = 0 to !nb_points - 1 do
		if deja_parcourus.(mi)
		then show_point mi rayon color
	done);;
	
let show_traitement_a_etoile mdebut mfin rayon color =(*
	Affiche les points parcourus par A* *)
	let deja_parcourus = Array.make !nb_points false and
	parent = Array.make !nb_points (-1) and
	distance_reelle = Array.make !nb_points (infinity) and 
	distance_totale = Array.make !nb_points (infinity) in
	(a_etoile_aux mdebut mfin deja_parcourus parent distance_reelle distance_totale;
	for mi = 0 to !nb_points - 1 do
		if deja_parcourus.(mi)
		then show_point mi rayon color
	done);;
	
let show_traitement_a_etoile2 mdebut mfin rayon color =(*
	Affiche les points parcourus par A* facteur 2*)
	let deja_parcourus = Array.make !nb_points false and
	parent = Array.make !nb_points (-1) and
	distance_reelle = Array.make !nb_points (infinity) and 
	distance_totale = Array.make !nb_points (infinity) in
	(a_etoile_aux2 mdebut mfin deja_parcourus parent distance_reelle distance_totale;
	for mi = 0 to !nb_points - 1 do
		if deja_parcourus.(mi)
		then show_point mi rayon color
	done);;

if affichage_graphe then
	open_graph "1500x1000";
	show_points coord_points 3 black affichage_str;
	show_routes red ;;
	
let show_client ci rayon color debut_fin = (*
	Affiche les points de debut et d'arrive du client ci
	Affiche debut et fin ssi debut_fin = true *)
	set_color color;
	let mdebut = debut_client ci and
	mfin = arrivee_client ci in
	(show_point mdebut rayon color;
	show_point mfin rayon color;
	if debut_fin
	then show_debut_fin ci color);;
	
let show_clients rayon color debut_fin =(*
	Affiche tous les clients de tab_cleints *)
	for ci = 0 to !nb_clients -1 do
		show_client ci rayon color debut_fin
	done;;
	
let show_trajet_client ci color = (*
	Affiche le trajet obtenus par A* du client ci *)
	let trajet = a_etoile (debut_client ci) (arrivee_client ci) in
	show_trajet trajet color;;
	
let show_parcours parcours color =(*
	Affiche le parcours de la couleur demandé *)
	let mcourant = ref 0 in
	set_color color;
	for indice_client = 0 to !nb_clients -1 do
		let ci = parcours.(indice_client) in
		(let mdebut = debut_client ci and
		mfin = arrivee_client ci in
		show_trajet (a_etoile !mcourant mdebut) color;
		show_trajet (a_etoile mdebut mfin) color;
		mcourant := mfin)
	done;
	show_trajet (a_etoile !mcourant 0) color;;
	
let show_clients_classique color = (*
	Affiche les clients avec les prereglages classique
	Affiche le point 0 de rayon 7 et en magenta*)
	set_line_width 3;
	for ci = 0 to !nb_clients -1 do
		(show_client ci 5 color true ;
		show_trajet_client ci color)
	done;
	show_point 0 7 magenta;;
	
	
let nouvelle_clientelle () = (*
	Efface l'interface graphique et regenere une clientele
	Affiche ensuite le graphe et les clients selon les prereglages classiques *)
	clear_graph ();
	show_points coord_points 1 black affichage_str;
	set_line_width 0;
	show_routes red ;
	set_line_width 3;
	for i = 0 to !nb_clients -1 do
			tab_clients.(i) <- (Random.int !nb_points, Random.int !nb_points)
		done;
	show_clients_classique blue ;;

