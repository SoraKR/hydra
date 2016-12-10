(** Hydra Battles *)

(* Message utilisé lorsqu'un bug est détecté – par exemple, dans les fonctions de réplication *)

let the_msg = "ouah, le bug!"

(* Une hydre est représentée sous la forme d'un arbre enraciné dont les noeuds peuvent avoir un nombre quelconque mais néanmoins fini de filles. *)

type hydra = Node of hydra list

(* Quelques abréviations  simples *)

(* Hydre à une seule tête *)
let head = Node []
let is_head : hydra -> bool = fun h -> h = head

(* Nœuds à 1, 2 ou 3 filles *)
let single h = Node [h]
let bi h1 h2 = Node [h1;h2]
let tri h1 h2 h3 = Node [h1;h2;h3]

(* Idem, lorsque les filles sont identiques *)
let bisame h = bi h h
let trisame h = tri h h h

(* Liste des filles d'un nœud *)
let les_filles (Node hs) = hs

(* Exemples d'hydres *)

let baby_hydra = single head
let very_small_hydra = bi head head
let small_hydra = single (bi head head)
let my_hydra = tri head head  (single (single (tri head head head)))
let another_hydra = single (tri head head head)
let yet_another_hydra = single small_hydra
let goodstein_hydra = tri head head (tri head head head)

let my_h=tri (bi head head)(single(tri head head head))head

(* Exemple du sujet, page 1 *)
let example_hydra = tri head (bi (single (tri head head head)) head) head
(* Exemple du sujet après 2 coups d'Hercule et réplication en surface *)
let example_shallow = bi (bi (trisame very_small_hydra) head) head
(* Exemple du sujet après 2 coups d'Hercule et réplication en profondeur *)
let example_deep =
  let one = bisame very_small_hydra in
  let two = tri one one head in
  tri two head two
(* L'hydre qu'on aurait obtenue si la duplication en profondeur avait fait 2 copies *)
let example_deep_two_copies =
  let one = trisame very_small_hydra in
  let two = Node[one; one; one; head] in
  Node [two; two; two; head]

(* Les hydres pouvant être assez grosses, il est utile de fournir quelques mesures  *)

(* Écrire une fonction donnant la taille d'une hydre (nombre total de noeuds) *)
let rec size : hydra -> int = fun h ->
  match h with
   Node [] -> 1
  |Node (t::q) -> if q = [] then  1 + size t else  size t + size (Node(q)) 
                                                                          
let _ = size  my_h         
(* Écrire une fonction donnant la hauteur d'une hydre (longueur maximale d'un  chemin partant du pied) *)

    
let rec height : hydra -> int = fun h ->
  match h with
    Node [] -> 1
  |Node(t::q) -> if q = [] then 1 + (height t) else max (height t) (height (Node(q)))
                      
let _ = height small_hydra
  let _=height my_h
          
(*retourn une liste*)
  let rec reverse l acc=match l with
      []->acc
    |t::q->reverse q (t::acc)
(*fxonction histo_lvl qui calcule le nombre de noeuds d'un niveau*)
let rec histo_lvl : hydra -> int = fun h ->
  match h with
    Node []->0
  |Node(t::q)->1+(histo_lvl (Node (q)))
(*fonction apply_son applique une fonction f à chaque hydre de la hydra list et retourne une liste des éléments modifiés par f *)
let rec apply_son : hydra->(hydra->int list)->int list =fun h f ->
  match h with
    Node []->[]
  |Node(t::q)->(f t)@(apply_son(Node(q)) f)
(* Écrire une fonction qui calcule l'histogramme d'une hydre, nombre de noeuds à chaque niveau *)



let rec find_stage i n h f=match h with
    Node[]->0
  |Node(t::q)->if i=n then (f (Node(t::q))) else find_stage (i+1) n t f+find_stage i n (Node q) f
let _ =find_stage 0 0 my_h histo_lvl

let rec histo_aux :hydra->int->int->int list=fun h i j->if i=j then find_stage i j h histo_lvl::[] else find_stage (i) j h histo_lvl::histo_aux h (i+1) j
  
let histogramme:hydra ->int list=fun h-> reverse(histo_aux h 0 ((height h)-1)) []    
(*get_head retourne 1 si on a une tête*)
let _=histogramme my_h

let get_head :hydra ->int = fun h->
  match h with
    Node []->1
  |_->0
let rec get_head_lvl:hydra->int=fun h->
  match h with
    Node[]->0
  |Node(t::q)->get_head t+get_head_lvl (Node q)

let rec histo_head_aux :hydra->int->int->int list=fun h i j->if i=j then find_stage i j h get_head_lvl::[] else find_stage (i) j h get_head_lvl::histo_head_aux h (i+1) j
let _=find_stage 0 1 my_h get_head_lvl
let histogram_heads :hydra->int list=fun h->reverse(histo_head_aux h 0 ((height h)-1)) []
(* Écrire une fonction qui compte le nombre de têtes à chaque niveau. *)
let _=histogram_heads my_h  

(*
   Écrire une fonction qui retourne une liste triée d'arêtes de l'hydre, avec 
   les contraintes décrites dans le sujet.
*)
let rec find_stage_list h i j=match h with
    Node[]->[]
  |Node(t::q)->if i=j then [histo_lvl (Node(t::q))] else find_stage_list t (i+1) j@find_stage_list (Node (q)) i j
let list_node_level =fun h i j->find_stage_list h i j@find_stage_list h (i+1) j 
let _=list_node_level my_h 0 0
let _=find_stage_list my_h 0 2

  
let rec sum_find h i j= find_stage i j h histo_lvl+find_stage (i+1) j h histo_lvl
let _ =sum_find my_h 0 1
let rec find_stage_edge i n o x a h h2 f=match h with
    Node[]->[]
  |Node(t::q)->if i=n then (f (Node(q)) o x) else find_stage_edge (i+1) n  (o+1+a) ((sum_find h2 0 (n-1))+1+) () t h2 f@find_stage_edge i n (o+1) (0+2) (Node q) h2 f

let _=find_stage_edge 0 ((height my_h)-1) 0 1 my_h my_h name_hydra
(*nomme la branche de l'hydre aun niveau actuel seulement*)
let rec name_hydra:hydra->int->int->(int*int)list=fun h o x->
  match h with
    Node []->[(o,x)]
  |Node(t::q)->(o,x)::name_hydra (Node q) o (x+1)
  
(*compte la valeur du dernier hydre d'un niveau en fonction de (o:int)*)
let rec count_hydra:hydra->int->int=fun h o->match h with
    Node []->o
  |Node (t::q)->1+count_hydra(Node q) o

(*appliquer f qui prend une fonction g à réiterer n fois*)
let many_apply_int_list (n:int) (f:(hydra->int->int->(int*int)list)) =fun h->
  match(n,h) with
    (0,Node [])->
let hydra_edges : hydra -> (int * int) list = fun h ->
  failwith "A écrire  "

(*
   Affiche une hydre h.
   Prérequis : la fonction hydra_edges doit avoir été écrite.
*)
let show_hydra h =
  (* Translates the list of edges in dot format, and outputs it to filename *)
  let hydra_to_dot h filename =    
    let rec edges_to_dot edges channel =
      match edges with
        [] -> ()
      | (a,b)::r -> Printf.fprintf channel "%d -- %d\n" a b; edges_to_dot r channel
    in
    let dot_preamble = "graph hydra {\n" ^
                       "\trankdir=BT;\n" ^
                       "\tnode [label=\"\" shape=point style=filled fixedsize=true];\n"
    in
    let dot_postamble = "\n}" in
    let edges = hydra_edges h in
    let channel = open_out filename in
    Printf.fprintf channel "%s\n" dot_preamble;
    edges_to_dot edges channel;
    Printf.fprintf channel "%s\n" dot_postamble;
    close_out channel
  in
  (* Get uname of the system to properly set the png viewer *)
  let uname() =
    let (inchannel, outchannel) = Unix.open_process "uname" in
    let name = input_line inchannel in
    close_in inchannel;
    close_out outchannel;
    name
  in
  (* Set viewer to Imagemagick "display" under Linux, or "open" under OSX, otherwise fail :)  *)
  let viewer = let uname = uname() in
    if uname = "Linux" then " display "
    else if uname = "Darwin" then " open "
    else failwith "Viewer not set under windows" in 
  (* Set style to view hydra's heads *)
  let style = "{style=\"invisible\",$.shape=\"none\",height=0.2,width=0.2,image=\"head.png\",label=\"\"}" in
  (* Prepare command *)
  let command = "gvpr -c 'N[$.outdegree==0] " ^ style ^ "' tmp.dot" (* post-process dot file to set style and view hydra's heads *)
                ^ "|" ^ "dot -T png -o tmp.png "                    (* Launch dot on resulting file *)
                ^ "&&" ^ viewer ^ " tmp.png" ^ "&"                  (* Launch viewer in bg *)
  in
  let _ = hydra_to_dot h "tmp.dot" in
  Unix.system command

(*
   Pour désigner un noeud ou une tête, on utilise une notation dite "de Dewey" : le chemin d'accés à un noeud
   est une liste d'indices qui représente le chemin à suivre depuis la racine ("le pied", si on préfère).
   un 0 signifie "aller vers la fille la plus à gauche", etc.
*)

type path = int list

(*
   Réactions de l'Hydre.
   Quand la tête de l'Hydre donnée par le chemin p est supprimée, l'Hydre
   effectue son algorithme de réplication.
*)

let rec repeat_concat n a l =
  if n <= 0
  then l
  else repeat_concat (n-1) a (a::l)

(* Supprime le i-ème élément de hs (si c'est une tête) *)

let rec remove_head i hs =
  match i,hs with
  | 0,(Node []) ::hs' -> hs'
  | i, h::hs' when i> 0 -> h :: remove_head (i-1) hs'
  |  _,_  -> failwith the_msg

(* Un tour de base : 
   - Hercule coupe une tête de l'Hydre h donnée par le chemin p.
   - L'Hydre se réplique n fois.
*)

type replication_fun = path -> hydra -> int -> hydra

(* Version en profondeur *)
let rec deep_replication : replication_fun = fun  p h n ->
  match p,h with
    [i], Node l -> Node (remove_head i l)
  | (_::_), Node l -> Node (deep_replication_list p l n)
  | _,_ -> failwith the_msg
and deep_replication_list p l n =
  match p,l with
    0::p', h::lh -> repeat_concat (1+n) (deep_replication p' h n) lh
  | i::p', h::lh when i> 0 -> h :: deep_replication_list (i-1::p') lh n
  | _,_ -> failwith the_msg

(* Version en surface *)
let rec shallow_replication : replication_fun = fun p h  n ->
  match p,h with
    [i], Node l -> Node (remove_head i l)
  | (_::_), Node l -> Node (shallow_replication_list p l n)
  | _,_ -> failwith the_msg
and shallow_replication_list p l n =
  match p,l with
    [0;i], Node l :: lh -> repeat_concat (1+n) (Node (remove_head  i l)) lh
  | 0::p',  h::lh -> shallow_replication p' h n :: lh
  | i::p', h::lh when i> 0 -> h :: shallow_replication_list (i-1::p') lh n
  | _,_ -> failwith the_msg

(* Les stratégies: Hercule et l'Hydre suivent chacun une stratégie *)

(*
   Une stratégie d'Hercule est, à partir d'une Hydre, de choisir une tête.
   Le programmeur qui définit une stratégie doit s'assurer qu'elle retourne
   toujours un chemin vers une tête.
*)

type hercules_strat =  hydra -> path

(*
   Suggestion: avant la fonction check_hercules_strategy, écrire une fonction sub_hydra
   telle que sub_hydra path h renvoie la sous-hydre de h donnée par le chemin path.
*)

let rec sub_hydra : path -> hydra -> hydra = fun path h ->
  match path with
  |[] -> h
  |t::q -> match h with
    |Node [] -> h
    |Node(a::x) -> if t = 0 then sub_hydra q a else if x = [] then h  else sub_hydra ((t-1)::q) (Node(x))

(* Écrire la fonction suivante qui teste si une stratégie choisit bien une tête  *)
let check_hercules_strategy : hercules_strat -> hydra -> bool = fun strat  h  ->
  if is_head(sub_hydra (strat h) h) then true else false


let rec left_head h x = match h with
  |Node [] -> x@[0]
  |Node(t::q) -> left_head t (0::x)
(* Écrire la stratégie choisissant la tête la plus à gauche *)
                   
let leftmost_head_strat : hercules_strat = fun  h  -> left_head h []   

let rec highest h x = match h with
  |Node [] -> x@[0]
  |Node(t::q) -> if 1+height (t) > height (Node(q)) then highest t (0::x) else match x with
    |[] -> highest (Node(q)) [1]
    |a::s -> highest (Node(q)) (a+1::s)
(* Écrire la stratégie choisissant une tête de hauteur maximale *)
                                                        
let highest_head_strat : hercules_strat = fun h ->
  highest h []

(* Écrire une stratégie visant à choisir une tête le plus près du sol possible *)
let closest_to_ground_strat : hercules_strat = fun h  ->
  failwith "A écrire"

(* En apprenant à utiliser la bibliothèque Random, écrire une stratégie pour choisir une tête au hasard *)
let random_strat : hercules_strat = fun h ->
  failwith "A écrire"

(* Étant donnée une date, l'Hydre peut calculer un nombre de réplications >= 1 *)

type time = Time of int

type hydra_strat =  time -> int

let check_hydra_strategy : hydra_strat -> time -> bool = fun st t -> st t >= 1

(* Une stratégie classique (celle de la vidéo): à chaque tour, le nombre de réplications est incrémenté. *)

let original_hydra_strat : hydra_strat = function Time  t -> t + 1

(* Une stratégie plus amusante : attention à l'explosion de pile ! *)

let boum : hydra_strat = function (Time t) ->
  let rec exp2 i =
    if i = 0 then 1 else 2 * exp2 (i-1)
  in exp2 t

(* Genre de bataille *)

type genre_de_bataille = Battle_kind of replication_fun * hercules_strat * hydra_strat

(*  Le score final d'une bataille *)
type result =
    Hercules_wins of time       (* Nombre de tours effectués *)
  | Hercules_gives_up of hydra  (* Hydre restante *)

(* Écrire la fonction de simulation *)
let simulation : genre_de_bataille -> hydra -> time -> result =
  fun (Battle_kind(replication,hercules_strat, hydra_strat)) initial_hydra (Time(duration)) ->
  failwith "A écrire"

(*
   Écrire une fonction make_trace telle que make_trace measure bat h_init (Time t) donne la suite 
   des valeurs de la fonction measure sur les hydres obtenues en partant de l'hydre h_init et 
   en effectuant t tours de la bataille de genre bat.
*)

let make_trace : (hydra -> 'a) -> genre_de_bataille -> hydra -> time -> 'a list =
  fun measure (Battle_kind(replication,hercules_strat, hydra_strat)) initial_hydra (Time duration) ->
  failwith "A écrire"

(* Écrire ici vos tests *)
