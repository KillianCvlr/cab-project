(*Mode de configuration :
	-simpliste : voies à doubles sens
	-realiste :	voies à sens unique, un mj peut avoir 1 seul voisin
auquel cas la voie serait en double sens*)
let configuration = "realiste";;
let calcul_trajet = true;;
let affichage_graphe = true;;
let affichage_str = false;;
let generation = true ;;

(*PARTIE 1 : INITIALISATION *)
  
let nb_points = ref 3000;; 
let nb_clients = ref 20;;
  
(*chaque point mi est distingué par ses coordonnées xi,yi positives *)
(*coord_points : (int * int) array 
coord_points.(mi) renvoie le couple xi,yi*)
let coord_points = Array.make !nb_points (0,0);;

(*route : float array array
matrice !nb_points * !nb_points tel que route.(mi).(mj) renvoie
la distance de la route entre mi et mj si elle existe et infinty sinon *)
let route = Array.make_matrix !nb_points !nb_points (infinity);; 


let pont = (* pont : float matrix
Stockage des routes ajoutées par l'algorithme de connexité
utilisé pour l'affichage *)
Array.make_matrix !nb_points !nb_points (infinity);;

let mat_distance = (* float matrix
Stock la distance euclidienne entre mi et mj *)
Array.make_matrix !nb_points !nb_points (infinity);;

let nombre_de_voisins = (* int array
Stock le nombre de voisin = point accessible depuis mi *)
	Array.make !nb_points 0;;

let tab_clients =(* tab_client : (int *int) array
  tab_clients.(ci) = couple (debut,arrivee) du client ci *)
  Array.make !nb_clients (0,0);; 

let coord_x mi = match coord_points.(mi) with
		|xi,yi -> xi ;;
		
let coord_y mi = match coord_points.(mi) with
		|xi,yi -> yi;;

let couple_egale (x,y) = (x = y);;

let debut_client ci = 
  match tab_clients.(ci) with 
  |x,y  -> x;;

let arrivee_client ci = 
  match tab_clients.(ci) with 
  |x,y  -> y;;
  
let distance_euclidienne mi mj = (*
	int -> int -> float : distance entre deux points
	Va chercher les coordonnées de mi et mj dans coord_points*)
	match coord_points.(mi),coord_points.(mj) with
	|(xi,yi),(xj,yj) -> sqrt (float_of_int ((xi-xj)*(xi-xj) + (yi-yj)*(yi-yj))) ;; 

let proches_voisins nb mi = (* O(nb*nb_point)
	int -> mi:int -> int array : renvoie les nb + proches points de mi dans un tableau*)
	let tab_voisins = Array.make (nb) (-1) and
	est_voisin = Array.make !nb_points false in 
  
	(est_voisin.(mi) <- true;
	for indice_tableau = 0 to (nb -1) do
		(for mj = 0 to (!nb_points -1) do
			if (not est_voisin.(mj))
			then 
				if (tab_voisins.(indice_tableau) == -1) 
				  then
					(tab_voisins.(indice_tableau) <- mj;
					 est_voisin.(mj) <- true)
				  else 
					if (distance_euclidienne mi mj < distance_euclidienne mi tab_voisins.(indice_tableau))
					then
						(est_voisin.( tab_voisins.(indice_tableau) ) <- false ;
						 tab_voisins.(indice_tableau) <- mj;
						 est_voisin.(mj) <- true)
		done)
	done; 
	tab_voisins);; 
	

let ajouter_route mi mj = (*
	int -> int -> unit : cree une route de mi a mj dans route dont la distance est comprise
	entre 1 et 2 fois la distance a vol d'oiseaux*)
	let poids_route = ((Random.float 1.) +. 1.) *. (distance_euclidienne mi mj) in
	route.(mi).(mj) <- poids_route;
	nombre_de_voisins.(mi) <- nombre_de_voisins.(mi) + 1;;

let ajouter_route_double_sens mi mj = (* O(1)
  int -> int -> unit : cree deux routes entre mi et mj et inversement dans route dont la distance est comprise
  entre 1 et 2 fois la distance a vol d'oiseaux*)
  let poids_route = ((Random.float 1.) +. 1.) *. (distance_euclidienne mi mj) in
  (route.(mi).(mj) <- poids_route;
   route.(mj).(mi) <- poids_route;
   nombre_de_voisins.(mi) <- nombre_de_voisins.(mi) + 1;
   nombre_de_voisins.(mj) <- nombre_de_voisins.(mj) + 1);; 
   
let i_min_distance tab tab_ignore = (* O(len tab) 
  float array-> bool array -> int :renvoie l'indice mi pour lequel tab.(mi) est minimal
    en ignorant les indice donnant infinity et ceux tels que tab_ignore.(mi) = true *)
  let n = Array.length tab and
  mi_minimale = ref 0 in 
  (for mj = 0 to n-1 do
		if (tab.(mj) < tab.(!mi_minimale) && tab.(mj) < infinity && not tab_ignore.(mj)) || tab_ignore.(!mi_minimale)
		then 
			(mi_minimale := mj)
   done;
   !mi_minimale);;
   
let rendre_connexe mdebut = (* O(nb_points^2)
	int -> unit : modifie la carte pour que l'on puisse atteindre tous les points depuis mdebut
	Principe : on parcours le graphe le + profondément possible jusqu au moment ou ce n'est plus possible
	alors soit tous les points sont parcourus auquel cas le programme s'arrete, 
	soit on cree un pont (ie une route) de mi a mj avec mi deja parcourus mais pas mj tel que la distance soit minimale
	pour le graphe, on reitere le parcours depuis mj*)
	let tous_points_parcourus = ref false and
	deja_parcourus = Array.make !nb_points false and
	liste_attente = ref [mdebut] in
	
	while not !tous_points_parcourus do
		(match !liste_attente with
		|[] ->((*tous les points connexes a mdebut ont été parcourus dans cette configuration*)
			if Array.exists ( fun a -> not a) deja_parcourus (* il existe un point non parcourus *)
			then (*on cree un pont avec une longueur minimale*)
				let m_debut_pont = ref 0 and
				m_fin_pont = ref 0 and
				longueur_pont = ref infinity in
				(for mi = 0 to !nb_points-1 do
					if deja_parcourus.(mi)
					then (*on cherche mj non parcourus tel que la distance soit minimale*)
						let mj = i_min_distance mat_distance.(mi) deja_parcourus in
						if mat_distance.(mi).(mj) < !longueur_pont
						then( (*on change de pont, il sera de mi a mj*)
							m_debut_pont := mi;
							m_fin_pont := mj;
							longueur_pont := mat_distance.(mi).(mj))
				done;
				ajouter_route_double_sens !m_debut_pont !m_fin_pont;
				pont.(!m_debut_pont).(!m_fin_pont) <- route.(!m_debut_pont).(!m_fin_pont);
				pont.(!m_fin_pont).(!m_debut_pont) <- route.(!m_fin_pont).(!m_debut_pont);
				(*on reitere le parcours depuis m_fin_pont*)
				liste_attente := [!m_fin_pont] )
				
			
			else (*tous les points ont été parcourus*) 
				tous_points_parcourus := true)
				
		|mtete::suite_points ->( (*on continue le parcours du graphe*)
			liste_attente := suite_points ;
			deja_parcourus.(mtete) <- true;
			for mj = 0 to !nb_points-1 do
				if route.(mtete).(mj) < infinity && not deja_parcourus.(mj)
				(* mj non parcourus et relie a mtete*)
				then liste_attente := mj::!liste_attente
			done;))
		
	done;;

let creation_carte_simpliste xmax ymax = (* Au pire : O((nb_point)^3)
  int -> int -> unit : modifie les coord_points, route, mat_distance pour donner un graphe 
  dont on ne connait pas la connexité*)
                                    
	(* ETAPE 1 : CREATION DE LA NUEE ET INITIALISATION DES DISTANCES *)
	(for mi = 0 to !nb_points -1 do
		coord_points.(mi) <- ((Random.int xmax) + 30, (Random.int ymax) + 30);
	done);
	
	for mi = 0 to !nb_points -1 do
		for mj = 0 to !nb_points-1 do
			if mat_distance.(mi).(mj) = infinity 
			then let distance_mi_mj = distance_euclidienne mi mj in
				mat_distance.(mi).(mj) <- distance_mi_mj;
				mat_distance.(mj).(mi) <- distance_mi_mj
		done
	done;
	
  
  (*ETAPE 2 : CREATION DES ROUTES
    pour chaque point on choisis un chiffre entre 1 et 8 qui sera
  le nombre de voisin (nb_voisins) = le nombre de depart possible depuis ce point *)
 	for mi = 0 to !nb_points -1 do
		let nb_voisins = 1 + Random.int 4 in 
		let tab_voisins = proches_voisins nb_voisins mi in 
		for voisin_k = 0 to nb_voisins -1 do
			if  route.(mi).(tab_voisins.(voisin_k)) = infinity
			then ajouter_route_double_sens mi tab_voisins.(voisin_k) 
		done
	done;
	  
	(*ETAPE 3 : RENDRE CONNEXE *)
	(* Pour configuration simpliste si un point est connexe, tous les points le sont
	Pour configuration realiste il faut que chaque point devienne connexe*)
	rendre_connexe 0 ;;
	
let rendre_connexe_voie_simple mdebut = (* O(nb_point^2)
	int -> unit : modifie la carte pour que l'on puisse atteindre tt les points depuis mdebut
	Principe : on parcours le graphe le + profondément possible jusqu au moment ou ce n'est plus possible
	alors soit tous les points sont parcourus auquel cas le programme stop, 
	soit on cree un pont (ie une route) de mi a mj avec mi deja parcourus mais pas mj tel que la distance soit minimale
	pour le graphe, on reitere le parcours depuis mj*)
	let tous_points_parcourus = ref false and
	deja_parcourus = Array.make !nb_points false and
	liste_attente = ref [mdebut] in
	
	while not !tous_points_parcourus do
		(match !liste_attente with
		|[] ->((*tous les points connexes a mdebut ont été parcourus dans cette configuration*)
			if Array.exists ( fun a -> not a) deja_parcourus (* il existe un point non parcourus *)
			then (*on cree un pont avec une longueur minimale*)
				let m_debut_pont = ref 0 and
				m_fin_pont = ref 0 and
				longueur_pont = ref infinity in
				(for mi = 0 to !nb_points-1 do
					if deja_parcourus.(mi)
					then (*on cherche mj non parcourus tel que la distance soit minimale*)
						let mj = i_min_distance mat_distance.(mi) deja_parcourus in
						if mat_distance.(mi).(mj) < !longueur_pont
						then( (*on change de pont, il sera de mi a mj*)
							m_debut_pont := mi;
							m_fin_pont := mj;
							longueur_pont := mat_distance.(mi).(mj))
				done;
				ajouter_route !m_debut_pont !m_fin_pont;
				pont.(!m_debut_pont).(!m_fin_pont) <- route.(!m_debut_pont).(!m_fin_pont);
				(*on reitere le parcours depuis m_fin_pont*)
				liste_attente := [!m_fin_pont] )
				
			
			else (*tous les points ont été parcourus*) 
				tous_points_parcourus := true)
				
		|mtete::suite_points ->( (*on continue le parcours du graphe*)
			liste_attente := suite_points ;
			deja_parcourus.(mtete) <- true;
			for mj = 0 to !nb_points-1 do
				if route.(mtete).(mj) < infinity && not deja_parcourus.(mj)
				(* mj non parcourus et relie a mtete*)
				then liste_attente := mj::!liste_attente
			done;))
		
	done;;

let creation_carte_realiste xmax ymax = (*O((nb_point)^3)
  int -> int -> unit : modifie les coord_points, route, mat_distance pour donner un graphe 
  dont on ne connait pas la connexité*)
                                    
	(* ETAPE 1 : CREATION DE LA NUEE ET INITIALISATION DES DISTANCES *)
	(for mi = 0 to !nb_points -1 do
		coord_points.(mi) <- (Random.int xmax, Random.int ymax);
	done);
	
	for mi = 0 to !nb_points -1 do
		for mj = 0 to !nb_points-1 do
			if mat_distance.(mi).(mj) = infinity 
			then let distance_mi_mj = distance_euclidienne mi mj in
				mat_distance.(mi).(mj) <- distance_mi_mj;
				mat_distance.(mj).(mi) <- distance_mi_mj
		done
	done;
	print_string "fin etape 1";
	
  
  (*ETAPE 2 : CREATION DES ROUTES
    pour chaque point on choisis un chiffre entre 2 et 8 qui sera
  le nombre de voisin (nb_voisins) = le nombre de depart possible depuis ce point *)
	(for mi = 0 to !nb_points -1 do
		let nb_voisins = 1 + Random.int 4 in 
		let tab_voisins = proches_voisins nb_voisins mi in 
		if nb_voisins = 1 
			then
				for voisin_k = 0 to nb_voisins -1 do
					if  route.(mi).(tab_voisins.(voisin_k)) = infinity
					then ajouter_route_double_sens mi tab_voisins.(voisin_k) 
				done
			else
				for voisin_k = 0 to nb_voisins -1 do
					if  route.(mi).(tab_voisins.(voisin_k)) = infinity
					then ajouter_route mi tab_voisins.(voisin_k) 
				done
	done);
	print_string "fin etape 2";
	  
	(*ETAPE 3 : RENDRE CONNEXE *)
	(* Pour configuration realiste il faut que chaque point devienne connexe*)
	for mi = 0 to !nb_points-1 do
		rendre_connexe_voie_simple mi;
		print_string (string_of_int mi);
	done;;


let creation_clients () = 
	(* unit -> unit : change tab_clients pour que les clients soient generes aleatoirement*)
	if generation 
	then
		for i = 0 to !nb_clients -1 do
			tab_clients.(i) <- (Random.int !nb_points, Random.int !nb_points)
		done;; 


(*PARTIE 2 : ALGORITHMIQUE*)



let rec poids_trajet (trajet : int list) = 
	match trajet with 
	|[] -> 0.
	|m2::[] -> 0.
	|m1::m2::reste_trajet -> route.(m1).(m2) +. (poids_trajet (m2::reste_trajet));;

let i_min_existe tab tab_bool = 
	let existence = ref false and
	mj = ref 0 in 
	(while not !existence && !mj < (!nb_points) do
		if ( tab.(!mj) < infinity && not tab_bool.(!mj))
		then 
			(existence := true)
		else incr mj
   done;
   !existence);;




let a_etoile_aux mdebut mfin deja_parcourus parent distance_reelle distance_totale = (* O(nb_points^2)
    int -> int -> bool array -> int array -> float array -> float array -> unit :
    modifie les tableaux pris en compte de tel maniere que en remontant parent
    depuis l'indice mfin on ait la plus courte route jusqu'a mfin
    on a parent.(mfin) = -1 en cas d'echec*)
	let m_courant = ref mdebut and
	nb_traitements = ref 0 in 
	distance_reelle.(mdebut) <- 0. ;
	distance_totale.(mdebut) <- 0. ;
	while !m_courant != mfin && !nb_traitements < !nb_points do
		deja_parcourus.(!m_courant) <- true;
		(for mi = 0 to !nb_points - 1 do 
		   (if route.(!m_courant).(mi) < infinity && not deja_parcourus.(mi)
			then 
				let distance_mi_mfin = distance_euclidienne mi mfin and
				nouvelle_distance_mi = distance_reelle.(!m_courant) +. route.(!m_courant).(mi) in 
				if distance_reelle.(mi) > nouvelle_distance_mi 
				then
					(distance_reelle.(mi) <- nouvelle_distance_mi ;
					distance_totale.(mi) <- nouvelle_distance_mi +. distance_mi_mfin;
					parent.(mi) <- !m_courant ) )
		done;
		incr nb_traitements;
		m_courant := i_min_distance distance_totale deja_parcourus )
	done ;;

let a_etoile mdebut mfin =(*O(nb_points^2)
    int -> int -> int list : revoie la liste des points consecutifs a 
    parcourir pour arriver a mfin depuis mdebut le + rapidement possible *)
	let deja_parcourus = Array.make !nb_points false and
	parent = Array.make !nb_points (-1) and
	distance_reelle = Array.make !nb_points (infinity) and 
	distance_totale = Array.make !nb_points (infinity) in
	(a_etoile_aux mdebut mfin deja_parcourus parent distance_reelle distance_totale; 
	let i = ref mfin and
    liste_route = ref [] in
   (if not (parent.(mfin) = -1)
    then
		while !i != mdebut do
			liste_route := !i::!liste_route;
			i := parent.(!i)
		done);
	mdebut::!liste_route);;





let dijkstra_aux mdebut mfin deja_parcourus parent distance_reelle =(* O(nb_points^2)
    int -> int -> bool array -> int array -> float array -> unit :
    modifie les tableaux pris en compte de tel maniere que en remontant parent
    depuis l'indice mfin on ait la plus courte route jusqu'a mfin
    on a parent.(mfin) = -1 en cas d'echec*)
	let m_courant = ref mdebut and
	nb_traitements = ref 0 in 
	distance_reelle.(mdebut) <- 0. ;
	while not (!m_courant = mfin) && !nb_traitements < !nb_points do
		deja_parcourus.(!m_courant) <- true;
		(for mi = 0 to !nb_points - 1 do 
			(if route.(!m_courant).(mi) < infinity && not deja_parcourus.(mi)
			then 
				let nouvelle_distance_mi = distance_reelle.(!m_courant) +. route.(!m_courant).(mi) in 
				if distance_reelle.(mi) > nouvelle_distance_mi 
				then
					(distance_reelle.(mi) <- nouvelle_distance_mi ;
					parent.(mi) <- !m_courant ) )
		done;
		incr nb_traitements;
		if i_min_existe distance_reelle deja_parcourus
		then
			m_courant := i_min_distance distance_reelle deja_parcourus
		else 
			m_courant := mfin)
	done ;;
	
let dijkstra mdebut mfin =(*O(nb_points^2)
    int -> int -> int list : revoie la liste des points consecutifs a 
    parcourir pour arriver a mfin depuis mdebut le + rapidement possible *)
	let deja_parcourus = Array.make !nb_points false and
	parent = Array.make !nb_points (-1) and
	distance_reelle = Array.make !nb_points (infinity) in
	(dijkstra_aux mdebut mfin deja_parcourus 
    parent distance_reelle; 
	let i = ref mfin and
    liste_route = ref [] in
	(if not (parent.(mfin) = -1) 
	then
		while !i != mdebut do
			liste_route := !i::!liste_route;
			i := parent.(!i)
		done);
	mdebut::!liste_route);;
	
	
	
  
let tab_trajets = (* float matrix
	Stockera le poids du trajet minimal de mi à mj
	Initialisé à l'infini*)
	Array.make_matrix !nb_points !nb_points (infinity);;

let trajets_possibles mdebut = (* O(nb_points^2)
	int -> unit :
	modifie tab_trajets tel que tab_trajet.(mdebut).(mj) renvoie le trajet le plus court de mdebut a mj*)
	let m_courant = ref mdebut and
	nb_traitements = ref 0 and 
	deja_parcourus = Array.make !nb_points false and
	traitement_faisable = ref true in 
	print_string (string_of_int mdebut) ;
	tab_trajets.(mdebut).(mdebut) <- 0. ;
	while !traitement_faisable do
		deja_parcourus.(!m_courant) <- true;
		(for mi = 0 to !nb_points - 1 do 
		   (if route.(!m_courant).(mi) < infinity && not deja_parcourus.(mi)
			then 
				let nouvelle_distance_mi = tab_trajets.(mdebut).(!m_courant) +. route.(!m_courant).(mi) in 
				if tab_trajets.(mdebut).(mi) > nouvelle_distance_mi 
				then
					tab_trajets.(mdebut).(mi) <- nouvelle_distance_mi )
		done);
		incr nb_traitements;
		if i_min_existe tab_trajets.(mdebut) deja_parcourus
		then 
			m_courant := (i_min_distance tab_trajets.(mdebut) deja_parcourus)
		else 
			traitement_faisable := false
	done ;;

let est_connexe mj = (*O(nb_point^2)
	int -> bool : renvoie true ssi tous les points sont accessibles depuis mj*) 
	(trajets_possibles mj;
	not( Array.exists (fun a -> a = infinity) tab_trajets.(mj)));; 
   

let tous_trajets_possibles () = (*O(nb_points^3)
	unit -> unit : modifie l'entiereté de tab_trajet si calcul_trajet=true*)
	if calcul_trajet
	then
		for mi = 0 to !nb_points-1 do  
			(print_string ((string_of_int mi) ^ "\n");
			trajets_possibles mi )
		done;;

(* GENERATION CARTE*)

if generation
then
	if configuration = "simpliste"
		then creation_carte_simpliste 1300 800
	else
		creation_carte_realiste 1000 500;
	creation_clients ();
	tous_trajets_possibles () ;; 

(* DETERMINATION DU PARCOURS LE PLUS COURT*)

(* Algorithme genetique*)

(* Porcessus de selection naturelle :
Depuis une population généré aleatoirement on selectionne les 45% les plus performants qui se croiseront, 
donnant 90% de la nouvelle generation qui aura un certain taux de mutation. 
On injecte ensuite 10% d'individus généré aleatoirement
On répete le processus un grand nombre de fois.*)

let creer_tableau_croissant nb_case = (*O(nb_case)
	int -> int array : creer un tableau tel que tab.(i) = i *)
	let tab = Array.make nb_case 0 in
	for i = 0 to nb_case -1 do
		tab.(i) <- i
	done;
	tab;;

Random.self_init ();;

let couple_inegal max = (*
	renvoie un couple (a,b) tel que a != b, que a et b soient inférieurs à max *)
	let a = Random.int max and
	b = ref (Random.int max) in
		(while a = !b do
			b:= (Random.int max)
		done; 
		(a,!b));;

let echange_tab tab (i,j) = (*O(1)
	échange le contenu du tableau tab entre les indice i et j *)
	let tab_i = tab.(i) in
	tab.(i) <- tab.(j);
	tab.(j) <- tab_i;;

(*Parcours : ensemble de trajets definis par l'ordre de prise en charge des clients
Correspond à un tableau de longueur !nb_clients comportant tous les ci tel que l'indice correspond a l'ordre de passage *)

let generer_parcours nb_repetition =(* O(nb_repetition)
	Genere un parcours en partant d'un tableau croissant et
	en intervertissant des indices i et j aleatoire un nb_repetitions definis
	On préconise nb_repetition = K*nb_client avec 20>K>5*)
	let tab = creer_tableau_croissant !nb_clients in
	(for k = 0 to nb_repetition do
		let couple_echange = couple_inegal !nb_clients in
		echange_tab tab couple_echange
	done;
	tab);;
	
let poids_course ci = (* 
	renvoie le poids de la course du client ci 
	Passe par tab_trajets *)
	tab_trajets.(debut_client ci).(arrivee_client ci);;

let poids_parcours parcours = (* O(len parcours)
	(parcours : int array) -> float :
	donne le poids de ce parcours *)
	let distance_parcourue = ref 0. and 
	mcourant = ref 0 in
	for indice_client = 0 to !nb_clients -1 do
		let ci = parcours.(indice_client) in
		(let trajet_jusquau_prochain_client = tab_trajets.(!mcourant).(debut_client ci) in
		(distance_parcourue := !distance_parcourue +. trajet_jusquau_prochain_client  +. (poids_course ci);  
		mcourant := (arrivee_client ci)))
	done;
	!distance_parcourue +. tab_trajets.(!mcourant).(0);;

(*Population : int array array
tableau de parcours modélisé par un tableau de taille nb_individus *)

let generer_population nb_individus nb_repetition =(* O(nb_individus*nb_repetition)
	int -> int -> int array array :
	genere une population en itérant nb_individus fois genere_parcours nb_repetition*)
	let population_aleatoire = Array.make nb_individus [|1|] in
	for pi = 0 to nb_individus -1 do
		population_aleatoire.(pi) <- generer_parcours nb_repetition
	done;
	population_aleatoire;;

let i_max_population population = (* O(len population)
	int array array -> int :
	Donne l'indice du parcours le plus long de la population*)
	let i_parcours_max = ref 0 in
	for parc_i = 1 to (Array.length population) -1 do
		if (poids_parcours population.(parc_i)) > (poids_parcours population.(!i_parcours_max))
		then 
			i_parcours_max := parc_i
	done;
	!i_parcours_max;;



let selection_population population_initiale nb_survivants = (* O(len population_initiale * nb_survivant) 
	int array array -> int -> int array array :
	renvoie la population des nb_survivants individus les plus performants de population_initiale *)
	let population_survivants = Array.sub population_initiale 0 nb_survivants and
	indice_survivants_dans_pop_initiale = creer_tableau_croissant nb_survivants in
	let longueur_max_survivant = ref (poids_parcours population_survivants.(i_max_population population_survivants)) in
	(for parc_i = nb_survivants -1 to (Array.length population_initiale) -1 do
	(*On test le reste des parcours de la population_initiale *)
		(if poids_parcours population_initiale.(parc_i) < !longueur_max_survivant
		then (* On introduit parc_i dans les survivants *)
			let ancien_i_survivant_max = i_max_population population_survivants in
			indice_survivants_dans_pop_initiale.(ancien_i_survivant_max) <- parc_i;
			population_survivants.(ancien_i_survivant_max) <- population_initiale.(parc_i);
			longueur_max_survivant := (poids_parcours population_survivants.(i_max_population population_survivants)))
	done;
	population_survivants);;

let meilleur_des_deux population (candidat1, candidat2) = (*
	int array array -> int*int -> int :
	Retourne l'indice du meilleur candidat du couple dans population *)
	if poids_parcours population.(candidat1) > poids_parcours population.(candidat2)
	then 
		candidat2
	else
		candidat1;;
		
let selection_tournoi population nb_match = (*
	int array array -> int -> int :
	Renvois l'indice dans population du meilleur candidat apres nb_match *)
	let nb_individus = Array.length population and
	meilleur_candidat = ref (meilleur_des_deux population (couple_inegal nb_individus)) in 
	for numero_match = 1 to nb_match do
		meilleur_candidat := meilleur_des_deux population (!meilleur_candidat,Random.int nb_individus)
	done;
	!meilleur_candidat;;
	
	
let croisement_individus population (pere,mere) len = (* O(nb_clients^2)
	int array array -> int * int -> int -> int array array :
	Croise population.(pere) et population.(mere) : le parcours resultant aura alors le debut jusqua len de parcours_a et
	on lui introduira le reste des clients dans l'ordre d'apparition de population.(mere) 
	renvoie les deux enfants sous forme d'une population de 2*)
	let parcours_enfant_1 = ref( Array.sub population.(pere) 0 len) and
	parcours_enfant_2 = ref (Array.sub population.(mere) 0 len) in
	(for indice_dans_mere = 0 to !nb_clients -1 do 
	(* On parcours tous les indices de population.(mere) *)
		let ci = population.(mere).(indice_dans_mere) in
		if not (Array.exists (fun ck -> ck = ci) !parcours_enfant_1)
		(* Si il n'existe pas deja le client ci dans parcours_enfant_1 *)
		then (* on rajoute ci a parcours_enfant_1 *)
			parcours_enfant_1 := Array.append !parcours_enfant_1 [|ci|]
	done;
	for indice_dans_pere = 0 to !nb_clients -1 do 
	(* On parcours tous les indices de population.(pere) *)
		let ci = population.(mere).(indice_dans_pere) in
		if not (Array.exists (fun ck -> ck = ci) !parcours_enfant_2)
		(* Si il n'existe pas deja le client ci dans parcours_enfant_2 *)
		then (* on rajoute ci a parcours_enfant_2 *)
			parcours_enfant_2 := Array.append !parcours_enfant_2 [|ci|]
	done;
	[| !parcours_enfant_1; !parcours_enfant_2 |]);;

let generer_mutation population nb_mutations = (*O(nb_mutation)
	int array array -> int -> unit :
	genere une mutation (echange de la pos de 2 clients) dans nb_mutations parcours de population*)
	for numero_mutant = 1 to nb_mutations do
		let indice_mutant_dans_population = Random.int (Array.length population) and
		genes_a_echanger = couple_inegal !nb_clients in
		echange_tab population.(indice_mutant_dans_population) genes_a_echanger
	done;;		

let creation_nouvelle_generation population_initiale fraction_selection fraction_mutation =(*
	O(nb_individus_initial * nb_survivants) 
	int array array -> int -> float -> int array array :
	renvoie la nouvelle generation issue de population_initiale en fonction des attributs de
	selection genetique fraction_selection et fraction_mutation tout deux compris entre 0 et 1 *)
	let nb_individus_initial = Array.length population_initiale in
	let nb_enfants = int_of_float (fraction_selection *. (float_of_int nb_individus_initial)) and
	nb_mutations = int_of_float (fraction_mutation *. (float_of_int nb_individus_initial)) and
	nb_matchs = (int_of_float (0.005 *. (float_of_int nb_individus_initial))) + 1 in
	
	(* On initialise la nouvelle population aux 3 meilleurs individus*)
	let nouvelle_generation = ref (selection_population population_initiale 3) in
	
	(*les champions se croisent afin de d'avoir une population de nb_enfants, on a 2 enfants par croisement *)
	while Array.length !nouvelle_generation < nb_enfants do
		(*on selectionne un couple de parent parmis les survivants*)
		let couple_parents = (selection_tournoi population_initiale nb_matchs, selection_tournoi population_initiale nb_matchs) and
		position_croisement = Random.int !nb_clients in
		nouvelle_generation := Array.append !nouvelle_generation (croisement_individus population_initiale couple_parents position_croisement)
	done;
	(*on genere des mutations a la population existante *)
	generer_mutation !nouvelle_generation nb_mutations;
	
	(*on insere de nouveaux arrivants en les selectionnant depuis une population aleatoire*)
	let nb_arrivants = nb_individus_initial - (Array.length !nouvelle_generation) in
	nouvelle_generation := Array.append !nouvelle_generation (generer_population nb_arrivants (!nb_clients *10));
	
	(* on retourne la nouvelle generation *)
	!nouvelle_generation;;
	
let population_apres_n_generation population_initiale nb_generation fraction_selection fraction_mutation  = 
(* 	O(nb_individus_initial * nb_survivants * nb_generation) 
	int array array -> int -> float -> float -> int array array :
	renvoie la population apres nb_generation iterations de l'algorithme de selection genetique *)
	let population_courante = ref population_initiale in
	for i = 1 to nb_generation do
		population_courante := creation_nouvelle_generation !population_courante fraction_selection fraction_mutation;
		(* On affiche le meilleur parcours pour tracer l'evolution de l'algorithme *)
		print_string ("score " ^ (string_of_float (poids_parcours (Array.get (selection_population !population_courante 1) 0)))^"\n")
	done;
	!population_courante;;

let parcours_minimale_genetique nb_individus nb_generation fraction_selection fraction_mutation =
(* 	O(nb_individus_initial * nb_survivants * nb_generation)
	int -> int -> float -> float -> int array :
	renvoie le meilleur parcours apres nb_generation avec les attributs genetiques associes *)
	(*on initialise la population aleatoirement *)
	let population_aleatoire = generer_population nb_individus (10* !nb_clients) in
	
	(*on utilise l'algorithme genetique *)
	let population_finale = population_apres_n_generation population_aleatoire nb_generation fraction_selection fraction_mutation in
	
	(*on cherche le plus petit parcours et on le renvoi *)
	let tab_meilleur_parcours = selection_population population_finale 1 in
	tab_meilleur_parcours.(0);;

#use "fonctions_comparatives.ml";;

#use "visu.ml";;
	
let algo_genetique_classique utiliser_a_etoile = (*
	unit -> int array array :
	Procédure facilitant l'interpretation*)
	let nb_individus = 50000 and
	nb_generation = 200 and
	fraction_selection = 0.9 and
	fraction_mutation = 0.5 in 
	if utiliser_a_etoile
	then 
		parcours_minimale_genetique_a_etoile nb_individus nb_generation fraction_selection fraction_mutation
	else
		parcours_minimale_genetique nb_individus nb_generation fraction_selection fraction_mutation;;

(* fraction_selection doit etre proche de 1 -> sinon convergence rapide et le parcours ne change pas 
nb_generation assez grand aussi
Plus la population est grande et mieux c'est
fraction_mutation*)