let a_etoile_aux2 mdebut mfin deja_parcourus parent distance_reelle distance_totale = (*
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
					distance_totale.(mi) <- nouvelle_distance_mi +. ( 2. *. distance_mi_mfin);
					parent.(mi) <- !m_courant ) )
		done;
		print_string  ((string_of_bool (!m_courant != mfin)) ^ "   " ^ (string_of_int !m_courant) ^ "   " ^ (string_of_int !nb_traitements) ^ "\n") ;
		incr nb_traitements;
		m_courant := i_min_distance distance_totale deja_parcourus )
	done ;;

let a_etoile2 mdebut mfin =(*
    int -> int -> int list : revoie la liste des points consecutifs a 
    parcourir pour arriver a mfin depuis mdebut le + rapidement possible *)
	let deja_parcourus = Array.make !nb_points false and
	parent = Array.make !nb_points (-1) and
	distance_reelle = Array.make !nb_points (infinity) and 
	distance_totale = Array.make !nb_points (infinity) in
	(a_etoile_aux2 mdebut mfin deja_parcourus parent distance_reelle distance_totale; 
	let i = ref mfin and
    liste_route = ref [] in
   (if not (parent.(mfin) = -1)
    then
		while !i != mdebut do
			liste_route := !i::!liste_route;
			i := parent.(!i)
		done);
	mdebut::!liste_route);;


let a_etoile_aux10 mdebut mfin deja_parcourus parent distance_reelle distance_totale = (*
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
					distance_totale.(mi) <- nouvelle_distance_mi +. ( 10. *. distance_mi_mfin);
					parent.(mi) <- !m_courant ) )
		done;
		print_string  ((string_of_bool (!m_courant != mfin)) ^ "   " ^ (string_of_int !m_courant) ^ "   " ^ (string_of_int !nb_traitements) ^ "\n") ;
		incr nb_traitements;
		m_courant := i_min_distance distance_totale deja_parcourus )
	done ;;

let a_etoile10 mdebut mfin =(*
    int -> int -> int list : revoie la liste des points consecutifs a 
    parcourir pour arriver a mfin depuis mdebut le + rapidement possible *)
	let deja_parcourus = Array.make !nb_points false and
	parent = Array.make !nb_points (-1) and
	distance_reelle = Array.make !nb_points (infinity) and 
	distance_totale = Array.make !nb_points (infinity) in
	(a_etoile_aux10 mdebut mfin deja_parcourus parent distance_reelle distance_totale; 
	let i = ref mfin and
    liste_route = ref [] in
   (if not (parent.(mfin) = -1)
    then
		while !i != mdebut do
			liste_route := !i::!liste_route;
			i := parent.(!i)
		done);
	mdebut::!liste_route);;

let trajet_client = Array.make !nb_clients [-1];;

let calcul_client () = (* unit -> unit :
calcule le trajet (liste de points) optimal pour la course de ci et la stock dans trajet_client *)
	for ci = 0 to !nb_clients -1 do
		trajet_client.(ci) <- (a_etoile (debut_client ci) (arrivee_client ci))
	done;;

let poids_parcours_a_etoile parcours = (* (parcours : int array) -> float :
	donne le poids ie la longueur de ce parcours *)
	let distance_parcourue = ref 0. and 
	mcourant = ref 0 in
	for indice_client = 0 to !nb_clients -1 do
		let ci = parcours.(indice_client) in
		(let trajet_jusquau_prochain_client = poids_trajet (a_etoile (!mcourant) (debut_client ci)) in
		(distance_parcourue := !distance_parcourue +. trajet_jusquau_prochain_client  +. (poids_trajet trajet_client.(ci)));  
		mcourant := (arrivee_client ci))
	done;
	!distance_parcourue +. poids_trajet (a_etoile (!mcourant) (0));;

(*Population : int array array
tableau de parcours modélisé par un tableau de taille nb_individus *)

let i_max_population_a_etoile population = (* int array array -> int :
Donne l'indice du parcours le plus long de la population*)
	let i_parcours_max = ref 0 in
	for parc_i = 1 to (Array.length population) -1 do
		if (poids_parcours_a_etoile population.(parc_i)) > (poids_parcours_a_etoile population.(!i_parcours_max))
		then 
			i_parcours_max := parc_i
	done;
	!i_parcours_max;;

let selection_population_a_etoile population_initiale nb_survivants = (*  int array array -> int -> int array array :
renvoie la population des nb_survivants individus les plus performants de population_initiale *)
	let population_survivants = Array.sub population_initiale 0 nb_survivants and
	indice_survivants_dans_pop_initiale = creer_tableau_croissant nb_survivants in
	let longueur_max_survivant = ref (poids_parcours_a_etoile population_survivants.(i_max_population_a_etoile population_survivants)) in
	(for parc_i = nb_survivants -1 to (Array.length population_initiale) -1 do
	(*On test le reste des parcours de la population_initiale *)
		(if poids_parcours_a_etoile population_initiale.(parc_i) < !longueur_max_survivant
		then (* On introduit parc_i dans les survivants *)
			let ancien_i_survivant_max = i_max_population_a_etoile population_survivants in
			indice_survivants_dans_pop_initiale.(ancien_i_survivant_max) <- parc_i;
			population_survivants.(ancien_i_survivant_max) <- population_initiale.(parc_i);
			longueur_max_survivant := (poids_parcours_a_etoile population_survivants.(i_max_population_a_etoile population_survivants)))
	done;
	population_survivants);;
	
let creation_nouvelle_generation_a_etoile population_initiale fraction_selection fraction_mutation =
(* int array array -> float -> float -> int array array :
renvoie la nouvelle generation issue de population_initiale en fonction des attributs de
selection genetique fraction_selection et fraction_mutation tout deux compris entre 0 et 1 *)
	let nb_individus_initial = Array.length population_initiale in
	let nb_survivants = int_of_float (fraction_selection *. (float_of_int nb_individus_initial)) and
	nb_mutations = int_of_float (fraction_mutation *. (float_of_int nb_individus_initial)) in
	
	(* On initialise la nouvelle population à ses survivants*)
	let nouvelle_generation = ref (selection_population_a_etoile population_initiale nb_survivants) in
	
	(*les survivants se croisent afin de doubler leur population, on a 2 enfants par croisement *)
	while Array.length !nouvelle_generation < 2* nb_survivants do
		(*on selectionne un couple de parent parmis les survivants*)
		let couple_parents = couple_inegal nb_survivants and
		position_croisement = Random.int !nb_clients in
		nouvelle_generation := Array.append !nouvelle_generation (croisement_individus !nouvelle_generation couple_parents position_croisement)
	done;
	(*on genere des mutations a la population existante *)
	generer_mutation !nouvelle_generation nb_mutations;
	
	(*on insere de nouveaux arrivants*)
	let nb_arrivants = nb_individus_initial - (Array.length !nouvelle_generation) in
	nouvelle_generation := Array.append !nouvelle_generation (generer_population nb_arrivants (!nb_clients *10));
	
	(* on retourne la nouvelle generation *)
	!nouvelle_generation;;
	
let population_apres_n_generation_a_etoile population_initiale nb_generation fraction_selection fraction_mutation  = 
(* int array array -> int -> float -> float -> int array array :
renvoie la population apres nb_generation iterations de l'algorithme de selection genetique *)
	let population_courante = ref population_initiale in
	for i = 1 to nb_generation do
		population_courante := creation_nouvelle_generation_a_etoile !population_courante fraction_selection fraction_mutation;
		print_string ("score " ^ (string_of_float (poids_parcours_a_etoile (Array.get (selection_population_a_etoile !population_courante 1) 0)))^"\n")
	done;
	!population_courante;;

let parcours_minimale_genetique_a_etoile nb_individus nb_generation fraction_selection fraction_mutation =
(* int -> int -> float -> float -> int array :
renvoie le meilleur parcours apres nb_generation avec les attributs genetiques associes *)
	(*on initialise la population aleatoirement *)
	let population_aleatoire = generer_population nb_individus (10* !nb_clients) in
	
	(*on utilise l'algorithme genetique *)
	let population_finale = population_apres_n_generation_a_etoile population_aleatoire nb_generation fraction_selection fraction_mutation in
	
	(*on cherche le plus petit parcours et on le renvoi *)
	let tab_meilleur_parcours = selection_population_a_etoile population_finale 1 in
	tab_meilleur_parcours.(0);;

calcul_client ();;

let algo_plus_proche_voisin () = (* O(nb_clients *log(nb_clients))
	recherche un parcours selon le plus proche clients apres avoir finis sa course *)
	let m_courant = ref 0 and
	client_satisfait = Array.make !nb_clients false and
	parcours_proche_client = Array.make !nb_clients (-1) in
	for indice_client = 0 to !nb_clients -1 do
		let proximite_min = ref infinity and
		client_proche = ref (-1) in
		(for ci = 0 to !nb_clients -1 do
			if not client_satisfait.(ci)
			then 
				let proxi_ci = poids_trajet (a_etoile !m_courant (debut_client ci)) in
				if proxi_ci < !proximite_min
				then 
					(proximite_min := proxi_ci;
					client_proche := ci)
		done;
		parcours_proche_client.(indice_client) <- !client_proche;
		client_satisfait.(!client_proche) <- true;
		m_courant := arrivee_client !client_proche)
	done;
	parcours_proche_client;;

let poids_route_max () = 
	let poids_max = ref 0. in
	for mi = 0 to !nb_points -1 do
		for mj = 0 to !nb_points -1 do
			if (route.(mi).(mj) > !poids_max)&&(route.(mi).(mj) < infinity)
			then
				poids_max := route.(mi).(mj)
		done
	done;
	!poids_max;;
	
let normaliser () = 
	let poids_max = poids_route_max () in
	(for mi = 0 to !nb_points-1 do
		(for mj = 0 to !nb_points-1 do
			if (mat_distance.(mi).(mj) < 0.5 *. poids_max )&& (tab_trajets.(mi).(mj)  > 10. *. poids_max) && (pont.(mi).(mj) = infinity)
			then 
				if (Random.bool () )
				then
					(ajouter_route_double_sens mi mj;
					pont.(mi).(mj) <- route.(mi).(mj);
					pont.(mj).(mi) <- route.(mj).(mi);
					print_string (string_of_int mi))
		done)
	done);
	tous_trajets_possibles () ;;

let pop_depuis_algo_genetique taille = (*
	int -> int array array :
	Cree une population de taille issus d'une selection genetique *)
	let population_retour = Array.make taille [|-1|] and
	nb_generation = 35 and
	nb_individus2 = 2000 and
	fraction_selection = 0.4 and
	fraction_mutation = 0.005 in
	for indice_parcours = 0 to taille -1 do
		population_retour.(indice_parcours) <- parcours_minimale_genetique nb_individus2 nb_generation fraction_selection fraction_mutation
	done;
	population_retour;;

let popu = pop_depuis_algo_genetique 300 ;;

let parcours_proche_voisin = poids_parcours (algo_plus_proche_voisin ());;

let distance_parcourue_client () = (*
	unit -> float:
	retourne la distance totale parcourure par les courses des clients *)
	let dist_trajet = ref 0. in
	for ci = 0 to !nb_clients -1 do
		dist_trajet := !dist_trajet +. (poids_trajet trajet_client.(ci) )
	done ;
	!dist_trajet;;