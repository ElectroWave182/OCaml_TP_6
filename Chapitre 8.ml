Exercice 1 :
------------


1)

type couleur =
	Blanc
	| Bleu
	| Cyan
	| Jaune
	| Magenta
	| Noir
	| Rouge
	| Vert
;;



2)

fontion couleur -> bool


let est_colore = fun

Noir -> false

| Blanc -> false

| _ -> true
;;


est_colore (Blanc) ;;
-> false

est_colore (Bleu) ;;
-> true


3)

fonction couleur -> couleur


let complementaire = fun

Blanc -> Noir
| Bleu -> Jaune
| Cyan -> Rouge
| Jaune -> Bleu
| Magenta -> Vert
| Noir -> Blanc
| Rouge -> Cyan
| Vert -> Magenta
;;


complementaire (Noir) ;;
-> Blanc

complementaire (Magenta) ;;
-> Vert



Exercice 2 :
------------


1)

type solution =
	(* Discriminant < 0 *)
	Complexes of float * float

	(* Discriminant = 0 *)
	| Identieques of float

	(* Discriminant > 0 *)
	| Reelles of float * float
;;


2)

type polynome =
	Plein of int * float list
	| Monomes of (int * float) list
;;



Exercice 3 :
------------


1)

type reel =
	Entier of int
	| Flottant of float
;;


1.1)

fonction reel * reel -> reel


let somme = fun

(Entier a, Entier b) ->
	Entier (a + b)

| (Flottant a, Flottant b) ->
	Flottant (a +. b)

| (Entier a, Flottant b) ->
	Flottant (float_of_int (a) +. b)

| (Flottant a, Entier b) ->
	Flottant (a +. float_of_int (b))
;;


somme (Entier (-5), Entier (7)) ;;
-> Entier 2

somme (Flottant (-5.), Flottant (7.)) ;;
-> Flottant 2.

somme (Flottant (-5.), Entier (7)) ;;
-> Flottant 2.

somme (Entier (-5), Flottant (7.)) ;;
-> Flottant 2.


1.2)

fonction reel * reel -> reel


let prod = fun

(Entier a, Entier b) ->
	Entier (a * b)

| (Flottant a, Flottant b) ->
	Flottant (a *. b)

| (Entier a, Flottant b) ->
	Flottant (float_of_int (a) *. b)

| (Flottant a, Entier b) ->
	Flottant (a *. float_of_int (b))
;;


prod (Entier (-5), Entier (7)) ;;
-> Entier 2

prod (Flottant (-5.), Flottant (7.)) ;;
-> Flottant 2.

prod (Flottant (-5.), Entier (7)) ;;
-> Flottant 2.

prod (Entier (-5), Flottant (7.)) ;;
-> Flottant 2.


2)

type complexe =
	Reel of reel
	| Parties of reel * reel
;;


3)

3.1)

fonction complexe * complexe -> complexe


let somme = fun

(Reel a, Reel b) ->

	Reel (somme (a, b))


| (Parties a, Parties b) ->

	Parties (
	(* Partie réelle *)
	(somme (fst (a), fst (b)),

	(* Partie imaginaire *)
	somme (snd (a), snd (b)))
	)


| (Parties a, Reel b) ->

	Parties (
	(* Partie réelle *)
	(somme (fst (a), b),

	(* Partie imaginaire *)
	snd (a))
	)


| (Reel a, Parties b) ->

	Parties (
	(* Partie réelle *)
	(somme (a, fst (b)),

	(* Partie imaginaire *)
	snd (b))
	)
;;


somme (Reel (Entier (-5)), Reel (Flottant (7.))) ;;
-> Reel (Flottant 2.)

somme (Parties (Entier (-5), Flottant (7.)), Parties (Flottant (-2.), Entier (3))) ;;
-> Parties (Flottant -7., Flottant 10.)

somme (Parties (Entier (-5), Flottant (7.)),  Reel (Entier (3))) ;;
-> Parties (Entier -2, Flottant 7.))

somme (Reel (Flottant (3.)), Parties (Entier (-5), Flottant (7.))) ;;
-> Parties (Flottant -2., Flottant 7.)


3.2)

fonction complexe * complexe -> complexe


let prod = fun

(Reel a, Reel b) ->

	Reel (prod (a, b))


| (Parties a, Parties b) ->

	Parties ((
	(* Partie réelle *)
	somme (
        prod (fst (a), fst (b)),
        prod (
            Entier (-1),
            prod (snd (a), snd (b))
        )
    ),

	(* Partie imaginaire *)
    somme (
        prod (fst (a), snd (b)),
        prod (snd (a), fst (b))
    )
	))


| (Parties a, Reel b) ->

	Parties ((
	(* Partie réelle *)
	prod (fst (a), b),

	(* Partie imaginaire *)
	prod (snd (a), b)
	))


| (Reel a, Parties b) ->

    Parties ((
    (* Partie réelle *)
    prod (a, fst (b)),

    (* Partie imaginaire *)
    prod (a, snd (b))
    ))
;;


prod (Reel (Entier (-5)), Reel (Flottant (7.))) ;;
-> Reel (Flottant -35.)

prod (Parties (Entier (-5), Flottant (7.)), Parties (Flottant (-2.), Entier (3))) ;;
-> Parties (Flottant 31., Flottant -29.)

prod (Parties (Entier (-5), Flottant (7.)),  Reel (Entier (3))) ;;
-> Parties (Entier -15, Flottant 21.))

prod (Reel (Flottant (3.)), Parties (Entier (-5), Flottant (7.))) ;;
-> Parties (Flottant -15., Flottant 21.)



Exercice 4 :
------------


type assertion =
    Vrai
    | Faux
    | Non of assertion
    | Et of assertion * assertion
    | Ou of assertion * assertion
;;


1)

let exp = Ou (Et (Vrai, Vrai), Et (Non (Ou (Faux, Non (Vrai))), Vrai)) ;;


2)

fonction assertion -> assertion


let rec echange = fun


(* Cas de base : assertion élémentaire *)

Vrai -> Faux

| Faux -> Vrai


(* Cas récursif : opérateur logique *)

| (Non a) ->
    Non (echange (a))

| (Et a) ->
    Et (
        echange (fst (a)),
        echange (snd (a))
    )

| (Ou a) ->
    Ou (
        echange (fst (a)),
        echange (snd (a))
    )
;;


echange (exp) ;;
-> Ou (Et (Faux, Faux), Et (Non (Ou (Vrai, Non (Faux))), Faux))


3)

fonction assertion -> assertion


let rec evalue = fun


(* Cas de base : assertion élémentaire *)

Vrai -> Vrai

| Faux -> Faux


(* Cas récursifs : opérateur logique *)

| (Non a) ->
    let appel = evalue (a)
    in
        if appel = Vrai
        then Faux
        else Vrai

| (Et a) ->
    let appelGauche = evalue (fst (a))
    and appelDroite = evalue (snd (a))
    in
        if appelGauche = Vrai
        & appelDroite = Vrai
        then Vrai
        else Faux

| (Ou a) ->
    let appelGauche = evalue (fst (a))
    and appelDroite = evalue (snd (a))
    in
        if appelGauche = Vrai
        or appelDroite = Vrai
        then Vrai
        else Faux
;;


evalue (exp) ;;
-> Vrai



Exercice 5 :
------------


type arbre =
    Feuille of int
    | Noeud of int * arbre * arbre
;;

let t = Noeud (4, Noeud (2, Noeud (3, Feuille 0, Feuille 1), Feuille 2), Noeud (1, Feuille 2, Noeud (3, Feuille 1, Feuille 2))) ;;


1)

1.1)

fonction int * int -> int


let max = fun

(a, b)
when a > b -> a

| (_, b) -> b
;;


1.2)

fonction arbre -> int


let rec profondeur = fun

(* Cas de base : feuille *)
(Feuille _) -> 1

(* Cas récrusif : embranchement *)
| (Noeud (_, gauche, droite)) ->
    max (profondeur (gauche), profondeur (droite)) + 1
;;


profondeur (t) ;;
-> 4


2)

2.1)

fonction int * int -> arbre


let rec completValable = fun

(* Cas de base : fond atteint *)
(1, valeur) -> Feuille (valeur)

(* Cas récrusif *)
| (profond, valeur) ->
    let sousArbre = completValable (profond - 1, valeur)
    in
        Noeud (valeur, sousArbre, sousArbre)
;;


2.2)

fonction int * int -> arbre


let complet = fun

(* Cas d'erreur *)
| (profond, valeur)
when profond < 1 ->
    failwith "La profondeur d'un arbre est positive."

| (profond, valeur) ->
    completValable (profond, valeur)
;;


complet (-1, 2) ;;

complet (0, 2) ;;

complet (3, 2) ;;



Exercice 6 :
------------


type Point = {
	abs : float ;
	ord : float
} ;;

let p1 = {
	abs = 0.0 ;
	ord = 0.0
}
and p2 = {
	abs = 2.0 ;
	ord = 0.0
}
and p3 = {
	abs = 1.0 ;
	ord = 2.0
}
and p4 = {
	abs = 0.0 ;
	ord = 1.0
} ;;


type Forme =
	Cercle of Point * float
	| Polygone of Point list
;;

let p = Polygone [
	p1 ;
	p2 ;
	p3 ;
	p4 ;
	p1
] ;;


1)

1.1)

fonction float * float -> float


let variance = fun

(reelA, reelB) ->
	let ecart = reelA -. reelB
	in
		ecart *. ecart
;;


1.2)

fonction Point * Point -> float


let distance = fun

(
{abs = x1 ; ord = y1},
{abs = x2 ; ord = y2}
) ->
	sqrt (
	variance (x1, x2)
	+. variance (y1, y2)
	)
;;


distance (p3, p2) ;;
-> 2.2360679775


2)

fonction Point list -> float


let rec longueur = fun

(* Cas récursif *)
(tete :: cou :: queue) ->
	distance (tete, cou)
	+. longueur (cou :: queue)

(* Cas de base : un seul point *)
| _ -> 0.
;;


longueur ([p1 ; p2 ; p3]) ;;
-> 4.2360679775


3)

3.1)

fonction '_a list -> bool


let rec dernier = fun

(* Cas de base : un seul élémént *)
[element] -> element

(* Cas récursif *)
| (_ :: queue) -> dernier (queue)

(* Cas d'erreur : [] *)
| _ -> failwith "Erreur de la part de bonPoly"
;;


3.2)

fonction '_a list -> float


let bonPoly = fun

(* Cas à 4 éléments ou plus *)
[premier ; _ ; _ ; der] -> premier = der
| (premier :: _ :: _ :: _ :: reste) -> premier = dernier (reste)

(* Cas à moins de 3 éléments *)
| _ -> false
;;


bonPoly ([p1 ; p2 ; p3 ; p4 ; p1]) ;;
-> true
bonPoly ([p1 ; p2 ; p3 ; p4 ; p3]) ;;
-> false
bonPoly ([p1 ; p2 ; p3]) ;;
-> false


4)

fonction Forme -> float


let perimetre = fun

(Polygone points) ->
	if bonPoly (points)
	then longueur (points)
	else failwith "Ce n'est pas un polygone."

| (Cercle (_, rayon)) ->
	6.28318 *. rayon
;;


perimetre (p) ;;
-> 6.65028153987

perimetre (Polygone [p1 ; p2 ; p3]) ;;
-> Erreur

perimetre (Cercle (p1, 1.)) ;;
-> 6.28318



Exercice 7 :
------------


1)

1.1)

type codeRVB =
{
	rouge : int;
	vert : int;
	bleu : int
} ;;


let yellow =
{
    rouge = 127;
    vert = 127;
    bleu = 0
} ;;

let white =
{
    rouge = 255;
    vert = 255;
    bleu = 255
} ;;

let black =
{
    rouge = 0;
    vert = 0;
    bleu = 0
} ;;


1.2)

type couleur =
	Melange of couleur * couleur
	| Codage of int * int * int
;;


let magenta = Codage (127, 0, 127) ;;

let grey = Melange (
    Codage (255, 255, 255),
    Codage (0, 0, 0)
) ;;


2)

2.1)

fonction codeRVB -> couleur


let rvbToCoul = fun

{
    rouge = r;
    vert = v;
    bleu = b
}
->
	Codage (r, v, b)
;;


rvbToCoul (yellow) ;;
-> Codage (200, 200, 0)

rvbToCoul (white) ;;
-> Codage (255, 255, 255)

rvbToCoul (black) ;;
-> Codage (0, 0, 0)


2.2)

2.2.1)

fonction int * int -> int


let moyenne = fun

(a, b) -> (a + b) / 2
;;


2.2.2)

fonction couleur -> codeRVB


let rec coulToRvb = fun

(* Cas récursif *)
(Melange (coulA, coulB)) ->
    let codeA = coulToRvb (coulA)
    and codeB = coulToRvb (coulB)
    in
        {
            rouge = moyenne (codeA.rouge, codeB.rouge);
            vert = moyenne (codeA.vert, codeB.vert);
            bleu = moyenne (codeA.bleu, codeB.bleu)
        }

(* Cas de base : Codage *)
| (Codage (r, v, b)) ->
    {
        rouge = r;
        vert = v;
        bleu = b
    }
;;


coulToRvb (magenta) ;;
-> {rouge = 127; vert = 0; bleu = 127}

coulToRvb (grey) ;;
-> {rouge = 127; vert = 127; bleu = 127}


2.3)

fonction int * int * int -> couleur


let tripletToCoul = fun

(r, v, b) ->
    Codage (r, v, b)
;;


let green = tripletToCoul (0, 255, 0) ;;

let blue = tripletToCoul (0, 0, 255) ;;

let cyan = Melange (green, blue) ;;


2.4)

fonction couleur -> int * int * int


let coulToTriplet = fun

coul ->
    let code = coulToRvb (coul)
    in
        (code.rouge, code.vert, code.bleu)
;;


coulToTriplet (magenta) ;;
-> (127, 0, 127)

coulToTriplet (cyan) ;;
-> (0, 127, 127)

coulToTriplet (grey) ;;
-> (127, 127, 127)


3)

include "D:/Downloads/couleurs" ;;


3.1)

fonction couleur -> unit


let peindre = fun

coul ->
    let triplet = coulToTriplet (coul)
    in
        bandes ([triplet])
;;


peindre (magenta) ;;

peindre (cyan) ;;

peindre (grey) ;;


3.2)

fonction couleur list -> (int * int * int) list


let rec coulToTriplets = fun

(* Cas récursif *)
(coulTete :: queue) ->
    let triplet = coulToTriplet (coulTete)
    in
        triplet :: coulToTriplets (queue)

(* Cas de base : [] *)
| _ -> []
;;


3.3)

fonction couleur list -> unit

let drapeau = fun

coulListe ->
    bandes (coulToTriplets (coulListe))
;;


drapeau ([cyan; grey; magenta]) ;;



Exercice 8 :
------------


type monome =
{
    coeff : float;
    deg : int
} ;;

type polynome =
    Plein of int * float list
    | Creux of monome list
;;


let p = Plein (4, [1.; 0.; 3.; 0.; 5.]) ;;

let c = Creux [
    {deg = 2; coeff = 3.};
    {deg = 4; coeff = 1.};
    {deg = 0; coeff = 5.}
] ;;


1)

1.1)

fonction monome * polynome -> monome list


let rec ajoutConverse = fun

(monome, Creux (tete :: queue)) ->
    if monome.deg = tete.deg

    (* Cas de base : degré trouvé *)
    then
        {
            coeff = monome.coeff +. tete.coeff;
            deg = tete.deg
        }
        :: queue

    (* Cas récursif *)
    else
        let appel = ajoutConverse (monome, Creux (queue))
        in
            tete :: appel

(* Cas de base : polynôme creux = [] *)
| (monome, Creux (_)) ->
    [monome]

(* Cas d'erreur : polynôme plein *)
| _ -> failwith "Le polynôme entré doit être sous forme creuse."
;;


1.2)

fonction monome * polynome -> polynome


let ajout = fun

(* Cas d'erreur : monôme de degré négatif *)
(monome, _)
when monome.deg < 0
->
    failwith "Le degré d'un monôme doit être positif."

| (monome, polynome) ->
    Creux (ajoutConverse (monome, polynome))
;;


ajout ({coeff = -4.5; deg = 2}, c) ;;
-> Creux [
{coeff = -1.5; deg = 2};
{coeff = 1.0; deg = 4};
{coeff = 5.0; deg = 0}
]

ajout ({coeff = -4.5; deg = 7}, c) ;;
-> Creux [
{coeff = 3.0; deg = 2};
{coeff = 1.0; deg = 4};
{coeff = 5.0; deg = 0};
{coeff = -4.5; deg = 7}
]

ajout ({coeff = -4.5; deg = 0}, c) ;;
->  Creux [
{coeff = 3.0; deg = 2};
{coeff = 1.0; deg = 4};
{coeff = 0.5; deg = 0}
]

ajout ({coeff = -4.5; deg = 2}, p) ;;
-> Erreur

ajout ({coeff = -4.5; deg = -2}, c) ;;
-> Erreur


2)

2.1)

fonction polynome -> monome list


let rec listeMonomes = fun

(* Cas récursifs *)
(Plein (degre, 0. :: queue)) ->
    listeMonomes (Plein (degre - 1, queue))

| (Plein (degre, tete :: queue)) ->
    let appel = listeMonomes (Plein (degre - 1, queue))
    in
        {
            deg = degre;
            coeff = tete
        }
        :: appel

(* Cas de base : [] *)
| (Plein (_)) -> []

(* Cas d'erreur : polynôme creux *)
| _ -> failwith "Le polynôme entré doit être sous forme pleine."
;;


2.2)

fonction polynome -> polynome


let pleinVersCreux = fun

(* Cas d'erreur : polynôme de degré négatif *)
(Plein (0, _)) -> failwith "Le degré d'un monôme doit être positif."

| poly ->
    Creux (listeMonomes (poly))
;;


pleinVersCreux (p) ;;
-> c
