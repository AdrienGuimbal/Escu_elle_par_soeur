type lexeme =
  (* Mots-clefs: *)
  | Select
  | From
  | Where
  | As
  | And | Or | Not
  | OrderBy | Asc | Desc
  | IsNull | IsNotNull
  | Limit | Offset
  | Union | Intersect | Except
  | Join | On
  | Min | Max | Sum | Avg
  | Count | Distinct
  | GroupBy | Having
  (* Autres lexemes *)
  | Asterisque
  | ParG | ParD
  | Virgule
  | Nom of string
  | Point
  | Egal | NonEgal
  | PlusGrand | PlusPetit
  | PlusGrandEgal | PlusPetitEgal
  | String of string
  | Valeur of int
  | Fois | Plus | Moins | Div



(*******************************)
(* I.Expressions Arithmétiques *)
(*******************************)

(* Q1 *)
let colonne_parser (l : lexeme list) : lexeme list = (*Adrien*)
  match l with
  | Nom(_)::Point::Nom(_)::l -> l
  | Nom(_)::l -> l
  | _ -> failwith "Mauvais nom de colone"

(* Q4 *)
let rec expression_parser (l : lexeme list) : lexeme list = (*Adrien*)
  begin match l with
  | Valeur(_)::l
  | String(_)::l -> l
  | Nom(_)::_ -> colonne_parser l
  | ParG::l ->
    let l = expression_parser l in
    (match l with
    | ParD::l -> l
    | _ -> failwith "Mal Parenthèsé"
    )
  | _ -> failwith "Expression invalide"
  end |> operator_parser
and operator_parser (l : lexeme list) : lexeme list = (*Adrien*)
  match l with
  | Div::l
  | Fois::l
  | Plus::l
  | Moins::l -> expression_parser l
  | _ -> l


(**********************************)
(* II.Formule logique : Condition *)
(**********************************)

let logical_operator_parser (l : lexeme list) : lexeme list = [](*Adrien*)

let comparateur = function
  Egal
| NonEgal
| PlusGrand
| PlusPetit
| PlusGrandEgal
| PlusPetitEgal -> true
| _ -> false


let rec condition_parser (l : lexeme list) : lexeme list = (*Adrien, Niveau 3*)
  match l with
  | Not::l -> condition_parser l
  | ParG::l -> begin
    match condition_parser l with
    | ParD::l -> l
    | _ -> failwith "Condition mal parenthèsée"
    end
  | _ -> begin let l = expression_parser l in
    match l with
    | [] -> failwith "Comparaison sans second terme"
    | IsNull::l
    | IsNotNull::l -> l
    | h::l -> (*"LeftExpression"*)
      if comparateur h
        then left_expression_parser l
        else failwith "Comparaison invalide"
    end
and left_expression_parser l =
    match l with
    | ParG::l -> begin
      let l = requete_parser l in
      match l with
      | ParD::l -> l
      | _ -> failwith "Expression gauche mal parenthèsée"
      end
    | _ -> expression_parser l


(**********************************)
(* III. Clauses optionnelles      *)
(**********************************)
(*     Made by Lancelot del fuego *)

(* Q8 *)
and where_clause_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  match l with
    | Where::cond -> condition_parser cond
    | _ -> l

(* Q9 *)
and having_clause_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  match l with
      | Having::cond -> condition_parser cond
      | _ -> l

(* Q10 *)
and liste_colonne_parser (l: lexeme list) : lexeme list = (* Gregoire *)
  let l' = colonne_parser l in
  liste_colonne_second_parser l'

and liste_colonne_second_parser (l: lexeme list) : lexeme list = (* Gregoire *)
  match l with
  | Virgule::t -> liste_colonne_parser t
  | _ -> l

(* Q11 *)
and group_by_clause_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  match l with
    | GroupBy::reste -> having_clause_parser (liste_colonne_parser reste)
    | _ -> l

(* Q12 *)
and order_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  match l with
    | Asc::reste -> reste
    | Desc::reste -> reste
    | _ -> l
    
(* Q13 *)
and liste_order_colonne_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  let under_exp (c' : lexeme list) : lexeme list =
    match c' with
      | Virgule::loc -> liste_order_colonne_parser loc
      | _ -> c'
  in
  colonne_parser (order_parser l)
  |> under_exp

and orderby_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  match l with
  | OrderBy::reste -> liste_order_colonne_parser reste
  | _ -> l
  
(* Q14 *)
and offset_clause_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  match l with
  | Offset::Valeur (n)::reste -> reste
  | _ -> l
  
(* Q15 *)
and limit_clause_parser (l : lexeme list) : lexeme list = (* Made by Lancelot del fuego *)
  match l with
    | Limit::Valeur (n)::offset_clause -> offset_clause_parser offset_clause
    | _ -> l
  
  
(********************************)
(*  NOW ENTER INTEGRATION HELL  *)
(********************************)

and requete_parser (l : lexeme list) : lexeme list = l (** TODO. Q26 **)
let parser (l : lexeme list)  = (* Soso *)
match requete_parser l with 
| [] -> ()
| _ -> failwith "Lexème inattendu." 


(* let test (i : int) (l : lexeme list) (correct : bool) =
  let result = 
    try
      Printf.printf "Test no %d: " i;
      parser l;
      Printf.printf "Success\n";
      true
    with
    | Failure s -> (Printf.printf "Erreur : %s\n" s; false)
  in
  if correct <> result then failwith "Echec du test"

let _ =
  test 1 [Select ; Nom("A") ; From ; Nom("T")] true;
  test 2 [Select ; Nom("A") ; From] false;
  test 3 [Select ; Nom("A") ; Point ; Nom("A") ; From ; Nom("T")] true ;
  test 4 [Select ; Nom("A") ; Point ; Nom("A") ; From ; Nom("T") ; Where ; Nom("A") ; Egal ; Valeur(5)] true *)
  
let test_parser (i : int) (parser_to_test) (l : lexeme list) (correct : bool) =
  let result = 
    try
      Printf.printf "Test no %d: " i;
      (match parser_to_test l with
      | [] -> ()
      | _ -> failwith "Lexème inattendu");
      Printf.printf "Success\n";
      true
    with
    | Failure s -> (Printf.printf "Erreur : %s\n" s; false)
  in
  if correct <> result then failwith "Echec du test"

let _ =
  test_parser 1 colonne_parser [Nom("A") ; Point ; Nom("T")] true;
  test_parser 2 colonne_parser [Nom("A") ; From] false;
  test_parser 3 colonne_parser [Nom("A")] true;
  test_parser 1 expression_parser [Nom("A") ; Point ; Nom("T")] true;
  test_parser 2 expression_parser [Nom("A") ; From] false;
  test_parser 3 expression_parser [Nom("A")] true;
  test_parser 4 expression_parser [Nom("A") ; Plus ; ParG ; Valeur(3) ; Fois ; String("nom"); ParD] true;
  test_parser 5 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; Valeur 1; ParD; ParG ; Valeur(3) ; ParD] false;
  test_parser 6 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; ParD; ParG ; Valeur(3) ; ParD] false;
  test_parser 7 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; Valeur 1; ParD; Fois ; ParG ; Valeur(3) ; ParD] true