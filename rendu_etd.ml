type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
               | Libre 
               | Code of string (*une chaine restreinte a 3 caracteres*);;

type case_option = None | Some of case;;

type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let test_configuration = ([(0,0,0),Jaune;(2,0,-2),Jaune;(-3,1,2),Jaune;(-2,1,1),Jaune;(0,1,-1),Vert],[Jaune;Vert],3);;
let test_configuration2 = ([(0,0,0),Jaune;(2,0,-2),Jaune;(-2,0,2),Jaune;(0,-1,1),Vert;(2,-1,-1),Vert],[Jaune;Vert],2);;
let test_configuration3 = ([(0,-1,1),Jaune;(-1,1,0),Jaune;(1,0,-1),Jaune;(0,1,-1),Vert],[Jaune;Vert],2);;
(*Pour tester la fonction devant_case_occu et l'auto détection de saut multiples dans différentes configurations*)
(* Au tour des Jaunes de jouer *)

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
 (i+j+k=0);;

let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = defaut;;

(*A MODIFIER en Q2*)
let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
abs(j)<dim+1 && abs(k)<dim+1;;          

(*A MODIFIER en Q3*) 
let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool = 
i+j+k = 0
;;
(*Q4*)
let rec tourner_case(m:int)((i,j,k):case):case = 
if m = 0 then (i,j,k)
else tourner_case (m-1) (-k,-i,-j)
;;
(*Q5*)
let translate ((c1,c2,c3):case) ((v1,v2,v3) : vecteur): case = 
(c1+v1,c2+v2,c3+v3)
;;
(*Q6*)
let diff_case ((c1,c2,c3):case) ((d1,d2,d3):case) : vecteur = 
(d1-c1,d2-c2,d3-c3)
;;
(*Q7*)
let sont_cases_voisines ((c1,c2,c3):case) ((d1,d2,d3):case) : bool = abs(d1-c1)+abs(d2-c2)+abs(d3-c3) = 2
;;
assert(sont_cases_voisines (1,0,-1) (0,0,0) = true);;
assert(sont_cases_voisines (0,0,0) (3,2,2) = false);;
(*Q8*)
let pt_ent (x:float) : float =
  float_of_int(truncate x);;

let midDist_2pts (c1,c2,c3:case) (d1,d2,d3:case) : float*float*float =
  (((float_of_int(c1+d1))/.2.),((float_of_int(c2+d2))/.2.),((float_of_int(c3+d3))/.2.));;

let coord_pivot (c1,c2,c3:case) (d1,d2,d3:case) : case_option =
  let i,j,k = midDist_2pts (c1,c2,c3:case) (d1,d2,d3:case) in
  if (c1=d1 || c2=d2 || c3=d3) && ((pt_ent i)=i && (pt_ent j)=j && (pt_ent k)=k)
  then Some((int_of_float(i),int_of_float(j),int_of_float(k)))
  else None
;;

(*Q9*)
let vec_et_dist ((c1,c2,c3):case)((d1,d2,d3):case):vecteur*int= match (c1,c2,c3,d1,d2,d3) with
|c1,c2,c3,d1,d2,d3 when d1-c1 <> 0 -> let d = abs(d1-c1) in (((c1-d1)/d,(c2-d2)/d,(c3-d3)/d),d)
|c1,c2,c3,d1,d2,d3 when d2-c2 <> 0 -> let d = abs(d2-c2) in (((c1-d1)/d,(c2-d2)/d,(c3-d3)/d),d)
|c1,c2,c3,d1,d2,d3 when (c1,c2,c3)=(d1,d2,d3) -> (0,0,0),0;;(*Cas cases égalent requis pour 22*)
(*Q10*)
let rec insere(liste : 'a list) (x : 'a): 'a list = 
match liste with 
|[] -> [x]
|pr::fin -> pr::insere fin x
;; 
let tourner_liste(liste1:'a list):'a list =  
match liste1 with 
|[] -> []
|pr::fin -> insere fin pr
;;
let rec der_list(liste : 'a list):'a = 
match liste with 
|[pr] -> pr
|pr::fin -> der_list fin
[@@warning "-8"]
;;

(*Q11*)
let rec remplir_segment(m:int) ((i,j,k):case) : case list =
match m with 
|1 -> [(i,j,k)]
|_ -> (i,j,k):: remplir_segment (m-1) (i,j+1,k-1)
;;

(*Q12
REAL.: eq rec. :
rempl...bas 1 (i,j,k) -> [i,j,k]
rempl...bas m (i,j,k) -> (remplir_segment m (i,j,k))@remplir_triangle_bas (m-1) (i-1,j+1,k)
*)
let rec remplir_triangle_bas (m:int) ((i,j,k) : case) : case list = 
match m with
|1 -> [(i,j,k)]
|_ -> (remplir_segment m (i,j,k))@remplir_triangle_bas (m-1) (i-1,j+1,k)
;;

(* Q13 
REAL.: eq. rec. : 
rempl...haut 0 (i,j,k) -> []                  
rempl...haut 1 (i,j,k) -> [(i,j,k)]                
rempl...haut m (i,j,k) -> remplir_segment m (i,j,k)@rempl...haut (m-1) (i+m-1*p,j,k-m+1*p) avec p un entier decroissant a chaque appel
*)
let rec remplir_triangle_haut (m:int) ((i,j,k):case) : case list =
  match m with
  |0 -> []
  |1 -> [(i,j,k)]
  |_ -> remplir_segment m (i,j,k)@(let p = m-1 in remplir_triangle_haut(m-1) (i+m-1*p,j,k-m+1*p));;

(* Q14 
REAL. : eq. rec. :
colorie cl [] -> []
colorie cl -> [(i,j,k),cl]
colorie cl pr::fin -> [pr,cl]@colorie cl fin
*)

let rec colorie (color:couleur) (cse:case list) : case_coloree list =
  match cse with
  | [] -> []
  | [(i,j,k)] -> [((i,j,k),color)]
  | pr::fin -> [(pr,color)]@colorie color fin;;

(* Q15 *)

(* Q16 *)

(*Q17*)
let quelle_couleur (c:case) ((ccl_liste,cl_lis,dim):configuration) : couleur =
  associe c ccl_liste Libre;;

(*18*)
let rec suppr_dans_conf ((ccl_liste,cl_lis,dim):configuration) (c:case) : configuration =
  match ccl_liste with
  |[] -> (ccl_liste,cl_lis,dim)
  |[(i,j,k),_] when (i,j,k)!=c -> (ccl_liste,cl_lis,dim)
  |[(i,j,k),_]-> [],cl_lis,dim
  |pr::fin -> let (i,j,k),color = pr in if (i,j,k) = c then (fin,cl_lis,dim) else suppr_dans_conf (fin,cl_lis,dim) c;;
  (* Fonctionne/ Peut être enlever la couleur de la liste des couleurs si plus aucune case de cette couleur n'est sur le plateau *)

(*19
d = départ
a = arrivée*)
let tri_quad (color_liste:case_coloree list) = List.fold_left (fun fin deb -> let (i,j,k),cl = deb in fin@[(i,j,k)]) [] color_liste;;
(* tri_quad [(1,1,2),Vert;(1,0,0),Jaune];;
   list = [(1, 1, 2); (1, 0, 0)] *)

let est_coup_valide ((ccl_liste,cl_lis,dim):configuration) (Du(d,a):coup) : bool =
  let case_list= tri_quad ccl_liste in
  sont_cases_voisines d a && 
  (List.mem (d,List.hd cl_lis) ccl_liste) && 
  est_dans_losange a dim &&
  not(List.mem a case_list);;
  (*Ici on ne traite pas le cas du constructeur Sm de coup car on fait les coups unitaires*)

assert(est_coup_valide (test_configuration) (Du((0,0,0),(0,-1,1))) = true);;
assert(est_coup_valide (test_configuration) (Du((0,0,0),(1,-1,0))) = true);;
assert(est_coup_valide (test_configuration) (Du((0,0,0),(2,-1,-1))) =false);;

(* Q20 *)
let appliquer_coup (conf:configuration) (Du(d,a):coup) : configuration =
  if est_coup_valide conf (Du(d,a))
    then let (ccl_liste,cl_lis,dim) = (suppr_dans_conf conf d) in ((a,List.hd cl_lis)::ccl_liste,cl_lis,dim)
  else conf;;
(*utop # appliquer_coup (test_configuration) (Du((0,0,0),(0,-1,1)));;
- : configuration = ([((0, -1, 1), Jaune); ((0, 1, 0), Vert)], [Jaune; Vert], 2)

Q21 ?? Meme fonction que la Q20*)
let mise_aJour_conf (conf:configuration) (Du(d,a):coup) : configuration =
  if est_coup_valide conf (Du(d,a))
    then let (ccl_liste,cl_lis,dim) = (suppr_dans_conf conf d) in ((a,List.hd cl_lis)::ccl_liste,cl_lis,dim)
  else failwith "Ce coup n'est pas valide, le joueur doit rejouer";;
(*utop # mise_aJour_conf (test_configuration) (Du((0,0,0),(2,-1,-1)));;
Exception: Failure "Ce coup n'est pas valide, le joueur doit rejouer"

Q22 Verifier que les cases en entrée sont bien valide ???*)
let rec est_libre_seg (c1:case) ((c2):case) ((ccl_liste,cl_lis,dim):configuration) : bool =
  let (i,j,k),coef = vec_et_dist c1 c2 and case_list = tri_quad ccl_liste in
  match c1,c2 with
  |c1,c2 when c1=c2-> not(List.mem c1 case_list)
  |(x,y,z),c2-> 
    not(List.mem (x,y,z) case_list) &&
    est_libre_seg (x-i,y-j,z-k) c2 (ccl_liste,cl_lis,dim);;

assert(est_libre_seg (1,0,-1) (3,0,-3) (test_configuration) = false);;
assert(est_libre_seg (0,-1,1) (0,3,-3) (test_configuration) = false);;
(*Fonctionne bien
   
Q23*)

let est_saut ((x,y,z):case) (c2:case) ((ccl_liste,cl_lis,dim):configuration) : bool =
  let (i,j,k),coef = vec_et_dist (x,y,z) c2 and case_list = tri_quad ccl_liste in
  coef = 2 && est_dans_losange c2 dim && not(List.mem c2 case_list) && 
  (List.mem (x-i,y-j,z-k) case_list);;

assert(est_saut (0,0,0) (0,2,-2) (test_configuration) = true);;

(* Q24 *)

let rec saut_multiple (case_li:case list) (conf:configuration) : bool =
  match case_li with
  |[c1;c2] -> est_saut c1 c2 conf
  |[] -> true
  |pr::fin -> 
    let c2 = List.hd fin in est_saut pr c2 conf && 
    saut_multiple fin conf;;
(*utop # saut_multiple [(1,-1,0);(-1,1,0);(1,1,-2)] (test_configuration);;
- : bool = true

Q25*)
let est_coup_valide_f ((ccl_liste,cl_lis,dim):configuration) (c:coup) : bool =
  let case_list= tri_quad ccl_liste in 
  match c with
  |Du(c1,c2) ->
  sont_cases_voisines c1 c2 && 
  (List.mem (c1,List.hd cl_lis) ccl_liste) && 
  est_dans_losange c2 dim &&
  not(List.mem c2 case_list)
  |Sm(c_li) -> (List.mem (List.hd c_li) case_list) && saut_multiple c_li (ccl_liste,cl_lis,dim);;

assert(est_coup_valide_f (test_configuration) (Sm([(1,-1,0);(-1,1,0);(1,1,-2)]))= true);;
(* Revient finalement à tester la fonction de la question 24*)

let (<<) f g = fun x -> f(g x);;
let dernier = (<<) List.hd List.rev ;;

let maj_conf_f (conf:configuration) (c:coup) : configuration =
  if est_coup_valide_f conf c then
    match c with
    |Du(d,a) -> let (ccl_liste,cl_lis,dim) = (suppr_dans_conf conf d) in 
      ((a,List.hd cl_lis)::ccl_liste,cl_lis,dim)
    |Sm(case_lis) -> let (ccl_liste,cl_lis,dim) = (suppr_dans_conf conf (List.hd case_lis)) in 
      ((dernier case_lis,List.hd cl_lis)::ccl_liste,cl_lis,dim)
  else failwith "Ce coup n’est pas valide, le joueur doit rejouer";;
(* La fonction appliquer coup et maj_conf sont similaire autant implémenter directement maj_conf
TEST : ...*)

(* Q26 *)
let score ((ccl_liste,cl_lis,dim):configuration) : int =
 let joueur = List.hd cl_lis in 
 List.fold_left (fun fin deb -> let (i,j,k),color = deb in if color = joueur then abs(i) + fin else fin) 0 ccl_liste;;

assert(score ([(1,0,-1),Jaune;(0,0,0),Jaune;(0,1,-1),Vert],[Jaune;Vert],2) =1);;

let score_gagnant (dim:dimension) : int =
  let sommet_bas = (-1-dim,1,dim) in let trian_li = remplir_triangle_bas dim sommet_bas in
  List.fold_left (fun fin deb -> let i,j,k = deb in abs(i) + fin) 0 trian_li;;
assert(score_gagnant 3 = 28);;
assert(score_gagnant 2 = 10);;

(* Q27 *)
let gagne ((ccl_liste,cl_lis,dim):configuration) : bool =
  score (ccl_liste,cl_lis,dim) = score_gagnant dim;;
assert(gagne test_configuration = false);;
assert(gagne ([(-3,1,2),Jaune;(-3,2,1),Jaune;(-4,2,2),Jaune],[Jaune],2)=true);;

(* Q28  Pas bien compris l'énoncé*)
let est_partie ((ccl_liste,cl_lis,dim):configuration) (cp_li:coup list) : couleur =
  if gagne (ccl_liste,cl_lis,dim) && List.for_all (est_coup_valide_f (ccl_liste,cl_lis,dim)) cp_li
    then List.hd cl_lis else Libre;;
(* A tester et compléter *)

(*Q29*)

let autour_case ((i,j,k):case) (dim:dimension) : case list = (*Renvoie la liste des cases autours d'une case c*)
  let x,y,z = i,j-1,k+1 in let seg = remplir_segment 2 (x,y,z) in
  let a_trier = List.fold_left (fun fin deb -> (remplir_triangle_bas 2 deb)@(remplir_triangle_haut 2 deb)@fin) [] seg in
  (x,y,z)::(i,j+1,k-1)::(List.filter (fun m -> m<>(x,y,z) && m<>(i,j,k) && m<>(i,j+1,k-1)) a_trier);;
  (*autour_case (0,0,0) 2 
    - : case list = [(0, -1, 1); (0, 1, -1); (-1, 1, 0); (1, 0, -1); (-1, 0, 1); (1, -1, 0)]*)

let devant_case_occu ((i,j,k):case) ((ccl_liste,cl_lis,dim):configuration) : case list = (*renvoie la liste des cases devant une case c*)
  let around = autour_case (i,j,k) dim and liste_case_conf = tri_quad ccl_liste in 
  List.filter (fun x -> List.exists (fun y -> y=x) liste_case_conf && let (m,n,o) = x in m>i) around;;

(*Petite fonction pour supprimer des doublons dans une liste*)
let cons_uniq fin pr = if List.mem pr fin then fin else pr::fin 
let suppr_dbl fin = List.rev (List.fold_left cons_uniq [] fin);;
(* fin *)

let rec auto_sm1 (c:case) (coqp:case list) (conf:configuration) : coup =
  match coqp with
  |[]-> Sm([])
  |(i,j,k)::fin-> 
    let (x,y,z),coef = vec_et_dist c (i,j,k) in if est_saut c (i-x,j-y,k-z) conf && x<0 then let coqp2 = devant_case_occu (i-x,j-y,k-z) test_configuration in
      let Sm(c_li) = auto_sm1 (i-x,j-y,k-z) coqp2 conf in Sm(suppr_dbl ([c;(i-x,j-y,k-z)]@c_li)) else Sm([]);;

let auto_sm_final (c:case) (coqp:case list) (conf:configuration) : coup list=
  let rec auto_sm2 c coqp conf =
    match coqp with
    |[]-> [Sm([])]
    |(i,j,k)::fin-> 
      let (x,y,z),coef = vec_et_dist c (i,j,k) in if est_saut c (i-x,j-y,k-z) conf then let coqp2 = devant_case_occu (i-x,j-y,k-z) test_configuration in
        let Sm(c_li) = auto_sm1 (i-x,j-y,k-z) coqp2 conf in let Sm(branche) = (auto_sm1 (i-x,j-y,k-z) (List.rev coqp2) conf) in
        [Sm(c::branche)]@[Sm(suppr_dbl ([c;(i-x,j-y,k-z)]@c_li))]@auto_sm2 c fin conf else [Sm([])] in
  List.filter (fun x -> let Sm(li) = x in List.length li>1) (auto_sm2 c coqp conf);;
(* Cette question est mal optimiser par soucis de temps et de complexciter, mais reste fonctionnelle
   
utop # auto_sm2 (-3,1,2) [(-2, 1, 1)] test_configuration;;
- : coup list =
[Sm [(-3, 1, 2); (-1, 1, 0); (1, -1, 0)];
 Sm [(-3, 1, 2); (-1, 1, 0); (1, 1, -2); (3, -1, -2)]; Sm []]*)

(* Méthode arbre 
   //////////////*)
type 'a arbca = V | N of 'a arbca*'a arbca*'a*'a arbca*'a arbca;;

let direction (c1:case) (c2:case) : int = (*Pas besoin*)
  match vec_et_dist c1 c2 with
  |(i,j,k),co when i=0 && j>0 && k<0-> 1 (*Gauche*)
  |(i,j,k),co when i<0 && j>0 && k=0-> 2 (*Hautg*)
  |(i,j,k),co when i<0 && j=0 && k>0-> 3 (*Hautd*)
  |(i,j,k),co when i=0 && j<0 && k>0-> 4 (*Droite*)
  |_ -> 0;; (*Case vide*)

let rec cmplt_l (l:case list): case list = (*Complete une liste de case jusqu'à 4 elements*)
  match List.length l with 
  |4-> l 
  |_-> cmplt_l ((1,1,1)::l);;

let rec tri_devant_case c l = (*tri une liste de case de 4 elements, en fonction de la direction entre la case de départ et les cases de la liste*)
  match l with
  |[] -> []
  |[x] -> [x]
  |pr::fin -> let suiv = List.hd fin in let dir_suiv = direction c suiv in
  if dir_suiv>direction c pr then pr::suiv::tri_devant_case c (List.tl fin) else suiv::tri_devant_case c (List.tl fin);;

let rec cons_arb (c:case) (conf:configuration) : 'a arbca =
  match devant_case_occu c conf with (*La liste Devant case doit avoir tt le temps 4 element, type Some option si y a pas de case c'est NONE*)
  |[] -> V
  |(i,j,k)::fin -> 
    let (x,y,z),coef = vec_et_dist c (i,j,k) in
    N((if z<0 then cons_arb (i-x,j-y,k-z) conf else V),
      (if x<0 && z=0 then cons_arb (i-x,j-y,k-z) conf else V),
      c,
      (if z<0 && y=0 then cons_arb (i-x,j-y,k-z) conf else V),
      if y<0 then cons_arb (i-x,j-y,k-z) conf else V);;

(* /////////////
   Fin méthode Mais non achevé *)

let coup_possible ((ccl_liste,cl_lis,dim):configuration) (c:case) : (case*coup) list =
  let around = autour_case c dim in let coqp = devant_case_occu c (ccl_liste,cl_lis,dim) in let saut_mult = (auto_sm_final c coqp (ccl_liste,cl_lis,dim)) in
  List.fold_right (fun deb fin -> if est_coup_valide_f (ccl_liste,cl_lis,dim) (Du(c,deb)) then 
    (deb,(Du(c,deb)))::fin else fin) around []@
  (List.fold_right (fun deb fin -> if est_coup_valide_f (ccl_liste,cl_lis,dim) deb then 
    let Sm(li)=deb in let arrive = dernier li in (arrive,deb)::fin else fin) saut_mult []);;
(*utop # coup_possible test_configuration (-3,1,2);;
- : (case * coup) list =
[((-3, 0, 3), Du ((-3, 1, 2), (-3, 0, 3)));
 ((-3, 2, 1), Du ((-3, 1, 2), (-3, 2, 1)));
 ((-4, 2, 2), Du ((-3, 1, 2), (-4, 2, 2)));
 ((-4, 1, 3), Du ((-3, 1, 2), (-4, 1, 3)));
 ((-2, 0, 2), Du ((-3, 1, 2), (-2, 0, 2)));
 ((1, -1, 0), Sm [(-3, 1, 2); (-1, 1, 0); (1, -1, 0)]);
 ((3, -1, -2), Sm [(-3, 1, 2); (-1, 1, 0); (1, 1, -2); (3, -1, -2)])]
 
 Q30*)

 

(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;


(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)