(*
  Maire Grégoire
  MPI*
  g.maire118@gmail.com
  
  D'ans l'entièreté du code, j'ai pris la liberté de faire en sorte que les erreurs des failwith internes
  soient ajoutés aux failwith externes, créant ainsi une stack d'erreur.

  J'ai été obligé, de par la présence d'un appel interne à requete_parser en partie 2, ce dernier n'étant défini qu'à la toute fin,
  de mettre des `and` entre chaque définition de parser pour permettre les appels mutuels. J'aurais peut-être pu trouver un autre agencement
  des fonctions qui ne forçait pas à mettre autant de `and`, mais la structure logique du code (suivant l'ordre des questions) en aurait été
  grandement impacté, j'ai donc fait le choix de laisser tel quel.
*)

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


(*I] Expression arithmétique*)


let rec colonne_parser (l: lexeme list) : lexeme list =
  match l with
  | Nom(s)::Point::Nom(s')::t -> t
  | Nom(s)::t -> t
  | _ -> failwith "Nom de colonne attendu"

and expression_parser (l: lexeme list) : lexeme list =
  begin 
    match l with
    | Nom(s)::t -> colonne_parser l
    | Valeur(n)::t -> t
    | String(s)::t -> t
    | ParG::t -> (
      let l' = expression_parser t in
      match l' with
      | ParD::t' -> t'
      | _ -> failwith "Expression mal parenthésée"
    )
    | _ -> failwith "Expression attendue"
  end |> expression_second_parser

and expression_second_parser (l: lexeme list) : lexeme list =
  match l with
  | Plus::t | Moins::t | Fois::t | Div::t -> expression_parser t 
  | _ -> l


(*II] Formule logique: Condition*)

(*Niveau 3*)
and condition_parser (l: lexeme list) : lexeme list =
  match l with
  | ParG::t -> (
    let l' = condition_parser t in
    match l' with
    | ParD::t' -> t'
    | _ -> failwith "Condition mal parenthésée"
  )
  
  | Not::t -> condition_parser t
  
  | _ -> try
    l |> expression_parser |> left_expression_parser |> suite_condition_parser
    with Failure e -> failwith (e ^ " |> " ^ "Condition attendue")

and comparateur_parser (l: lexeme list) : lexeme list =
  match l with
  | Egal::t | NonEgal::t | PlusGrand::t | PlusPetit::t | PlusGrandEgal::t | PlusPetitEgal::t -> t
  | _ -> failwith "Comparateur attendu"

and left_expression_parser (l: lexeme list) : lexeme list =
  match l with
  | IsNull::t -> t
  | IsNotNull::t -> t

  (*Les autres cas commencent forcément par un comparateur, puis soit une requête parenthésée, soit une expression.*)
  | _ -> (
    let l' = comparateur_parser l in
    match l' with
  
    (*Cas où on commence par une parenthèse: soit `(Expr)`, soit `(Requête)`*)
    | ParG::t -> (
      let l' = try requete_parser l with Failure _ -> expression_parser t in
      match l' with
      | ParD::t' -> t'
      | _ -> failwith "Requête/Expression mal parenthésée"
    )

    (*Autre cas: forcément une expression*)
    | _ ->
      try expression_parser l'
      with Failure e -> failwith (e ^ " |> " ^ "`Comparateur Expr` ou `IsNull Expr` ou `IsNotNull Expr` ou `Comparateur Expr` ou `Comparateur (Requete)` attendu")
  )


and suite_condition_parser (l: lexeme list) : lexeme list =
  match l with
  | And::t -> condition_parser t
  | Or::t -> condition_parser t
  | _ -> l


(*III] Clauses optionnelles*)

and where_clause_parser (l: lexeme list) : lexeme list =
  match l with
  | Where::t -> condition_parser t
  | _ -> l

and having_clause_parser (l: lexeme list) : lexeme list =
  match l with
  | Having::t -> condition_parser t
  | _ -> l

(* Version LL(1) de liste_colonnes: on a les règles `ListeColonnes -> Colonne ListeColonnesSecond et `ListeColonnesSecond -> Epsilon | Virgule ListeColonnes`*)
and liste_colonne_parser (l: lexeme list) : lexeme list =
  let l' = colonne_parser l in
  liste_colonnes_second_parser l'

and liste_colonnes_second_parser (l: lexeme list) : lexeme list =
  match l with
  | Virgule::t -> liste_colonne_parser t
  | _ -> l

and groupby_clause_parser (l: lexeme list) : lexeme list =
  match l with
  | GroupBy::t -> t |> liste_colonne_parser |> having_clause_parser
  | _ -> l

and order_parser (l: lexeme list) : lexeme list =
  match l with
  | Asc::t | Desc::t -> t
  | _ -> failwith "`Asc` ou `Desc` attendu"

(*Il n'était pas demandé dans les consignes de faire ces deux fonctions suivantes, mais elles étaient dans la grammaire donc je les aies écrites en supposant
  que c'était un oubli (sert à parser ListeOrderColonnes en LL(1))*)
and liste_order_colonnes_parser (l: lexeme list) : lexeme list =
  (try l |> colonne_parser |> order_parser |> liste_order_colonnes_second_parser
  with Failure e -> failwith (e ^ " |> " ^ "Liste (éventuelle, ou unique élément) de colonnes avec ordre attendu"))

and liste_order_colonnes_second_parser (l: lexeme list) : lexeme list =
    match l with
    | Virgule::t -> liste_order_colonnes_parser t
    | _ -> l

and orderby_parser (l: lexeme list) : lexeme list =
  match l with
  | OrderBy::t -> liste_order_colonnes_parser t
  | _ -> l

and offset_clause_parser (l: lexeme list) : lexeme list =
  match l with
  | Offset::Valeur(n)::t -> t
  | _ -> l

and limit_clause_parser (l: lexeme list) : lexeme list =
  match l with
  | Limit::Valeur(n)::t -> offset_clause_parser t
  | _ -> l


(*IV] Opérations sur les tables: Clause FROM*)

and alias_parser (l: lexeme list) : lexeme list =
  match l with
  | As::Nom(s)::t -> t
  | _ -> l

and tabledb_parser (l: lexeme list) : lexeme list =
  match l with
  | Nom(s)::t -> alias_parser t
  | _ -> failwith "Nom de table attendu"

and onoption_parser (l: lexeme list) : lexeme list =
  match l with
  | On::t -> condition_parser t
  | _ -> l

and jointable_parser (l: lexeme list) : lexeme list =
  match l with
  | Join::t -> t |> table_parser |> onoption_parser
  | _ -> l

and table_parser (l: lexeme list) : lexeme list =
  match l with
  | ParG::t -> (
    let l' = table_parser t in
    match l' with
    | ParD::t -> t |> jointable_parser |> alias_parser
    | _ -> failwith " table_parser: expression mal parenthésée"
  )

  | _ -> (
    try l |> tabledb_parser |> jointable_parser
    with Failure e -> failwith (e ^ " |> " ^ "Clause FROM mal formée") (*TODO reformuler l'erreur*)
  )


(*ListeTable -> version LL(1), comme les autres on sépare en 2 fonctions*)
and listetable_parser (l: lexeme list) : lexeme list =
  let l' = table_parser l in
  listetable_second_parser l'

and listetable_second_parser (l: lexeme list) : lexeme list =
  match l with
  | Virgule::t -> listetable_parser t
  | _ -> l


(*IV] Opérations sur les colonnes: Clause SELECT*)

and distinct_parser (l: lexeme list) : lexeme list =
  match l with
  | Distinct::t -> t
  | _ -> l

and colonne_expr_parser (l: lexeme list) : lexeme list =
  match l with
  
  | Avg::ParG::t | Sum::ParG::t | Min::ParG::t | Max::ParG::t -> (
    let l' = colonne_parser l in
    match l' with
    | ParD::t -> t
    | _ -> failwith "Clause SELECT: mal parenthésée"
  )

  | _ -> (
    try l |> colonne_parser |> alias_parser
    with Failure e -> failwith (e ^ " |> " ^ "Clause SELECT: Colonne ou opération sur colonne attendue")
  )

and colonnes_parser (l: lexeme list) : lexeme list =
  match l with
  | Nom(s)::Point::Asterisque::t -> t
  | _ -> colonne_expr_parser l

and select_parser (l: lexeme list) : lexeme list =
  match l with
  | Asterisque::t -> t
  | _ -> (
    try l |> colonnes_parser |> liste_select_parser
    with Failure e -> failwith (e ^ " |> " ^ "Clause SELECT: Astérisque ou colonne + liste_select attendu")
  )

and liste_select_parser (l: lexeme list) : lexeme list =
  match l with
  | Virgule::t -> liste_select_parser t
  | _ -> l


(*VI] Opérations ensemblistes sur les requêtes*)

and bloc_parser (l: lexeme list) : lexeme list = 
  match l with

  (*SELECT*)
  | Select::t -> begin
    let l' = t |> distinct_parser |> select_parser in

    (*FROM*)
    match l' with
    | From::t' -> (
      try t' |> listetable_parser |> where_clause_parser |> groupby_clause_parser
      with Failure e -> failwith (e ^ " |> " ^ "Bloc: `ListeTable WhereClause GroupByClause` attendu après `From`")
      )

    | _ -> failwith "Bloc: `From` attendu après mot-clé `Select`"
    end

  | _ -> failwith "Bloc: mot-clé `Select` attendu"

and liste_requete_parser (l: lexeme list) : lexeme list =
  match l with
  | Union::t | Intersect::t | Except::t -> (
    try t |> requete_parser |> liste_requete_parser
    with Failure e -> failwith (e ^ " |> " ^ "Requête attendue après Union/Intersect/Except")
  )
  | _ -> l

and requete_parser (l: lexeme list) : lexeme list =
  match l with

  (*1ère option: parenthèse en 1er, requete interne*)
  | ParG::t -> (
    let l' = requete_parser t in
    match l' with
    | ParD::t' -> l |> options_parser |> liste_requete_parser |> options_parser
    | _ -> failwith "Requête mal parenthésée"
  )

  (*Sinon: requete classique*)
  | _ -> (
    try l |> bloc_parser |> options_parser |> liste_requete_parser |> options_parser
    with Failure e -> failwith (e ^ " |> " ^ "Requête mal formée" (*cette erreur va être insupportable si je dois debug vu l'absence de stack d'erreurs... c'est la seule que je vais voir, oups*))
  )

and options_parser (l: lexeme list) : lexeme list =
  try l |> orderby_parser |> limit_clause_parser
  with Failure e -> failwith (e ^ " |> " ^ "Option bien formée attendue, c'est-à-dire une clause OrderBy puis une clause Limit")


(*Parser final*)
let parser (l : lexeme list)  =
  match requete_parser l with
  | [] -> ()
  | _ -> failwith "Lexeme inattendu."


(*TESTS*)

let test (i : int) (l : lexeme list) (correct : bool) =
  let result = 
    try
      Printf.printf "Test n° %d: " i;
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
  test 4 [Select ; Nom("A") ; Point ; Nom("A") ; From ; Nom("T") ; Where ; Nom("A") ; Egal ; Valeur(5)] true


(** Test un parseur donné sur une liste de lexèmes. i est le numéro du test, correct
  indique que la liste de lexèmes doit être acceptée par le parseur. Si la liste l
  est bien refusée par le parseur, alors l'erreur du parseur est affichée et le
  programme continue. Sinon un "Echec du test" est affiché et le programme s'arrête. *)
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
  test_parser 4 expression_parser [Nom("A") ; Point ; Nom("T")] true;
  test_parser 5 expression_parser [Nom("A") ; From] false;
  test_parser 6 expression_parser [Nom("A")] true;
  test_parser 7 expression_parser [Nom("A") ; Plus ; ParG ; Valeur(3) ; Fois ; String("nom"); ParD] true;
  test_parser 8 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; Valeur 1; ParD; ParG ; Valeur(3) ; ParD] false;
  test_parser 9 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; ParD; ParG ; Valeur(3) ; ParD] false;
  test_parser 10 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; Valeur 1; ParD; Fois ; ParG ; Valeur(3) ; ParD] true