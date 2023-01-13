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