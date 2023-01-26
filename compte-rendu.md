*Il s'agit d'un document Obsidian*

# Avant-propos


Groupe : Adrien GUIMBALL, Grégoire MAIRE, Lancelot VANRULLEN

**ATTENTION**, à la base on comptait se répartir toutes les parties, cependant le soir même du jour où le TP a été donné, Grégoire a fini le TP.. Donc on a la répartition suivante :

| Personne | Partie      |
| -------- | ----------- |
| Adrien   | I & II      |
| Lancelot | I & III     |
| Grégoire | I & Tout :) |

Vous trouverez donc dans l'archive :

- `parser_sql_gregoire.ml` : qui contient l'ENSEMBLE du TP fait par Grégoire
- `parser_sql.ml` : qui contient le début du TP avec les parties II & III faites par Lancelot et Adrien - ce code compile mais n'est pas complet.

Nous avons décidé de ne pas faire une chimère en mélangeant le code d'Adrien et de Lancelot avec celui de Grégoire puisque cela aurait juste rendu la lecture difficile de ce dernier puisqu'il a adopté certaines conventions qui le rendent difficilement modulable.

# I] Expression arithmétique

## Q2)

Les dérivation $$\displaylines{Expression \newline \implies Expression ; ArithOperateur ; \underline{Expression} \newline \implies Expression ; ArithOperateur ; \underline{Expression ;ArithOperateur ; Expression}}$$ $$\displaylines{Expression \newline \implies \underline{Expression} ; ArithOperateur ; Expression \newline \implies \underline{Expression ;ArithOperateur ; Expression} ; ArithOperateur ; Expression}$$ On un arbre difféerent mais donnent le même mot.

## Q3)

Pour rendre cette grammaire $LL(1)$, on doit juste faire en sorte que le cas $\text{Expression ArithOpérateur Expression}$ ne laisse pas le doute sur le nombre de caractères à lire en le rencontrant (tous les autres cas ne créent pas ce doute), en renommant $\text{ArithOpérateur} \rightarrow \square$ on obtient la grammaire suivante :
- $E \rightarrow$ Colonne E' | Valeur (n) E' | String (s) E' | ParG Expression ParD E'
- $E' \rightarrow \varepsilon$ | $\square$ Expression
- $\square \rightarrow$ Fois | Plus | Moins | Div

# III] Clauses optionnelles

## Q10)

Pour rendre cette grammaire $LL(1)$, on fait les transformations suivantes :
- ListeColonnes $\rightarrow$ Colonne C'
- C' $\rightarrow$ $\varepsilon$ | Virgule ListeColonne 

## Q13)

On va rendre la partie ListeOrderColonnes $LL(1)$, pour cela on pose :

- ListeOrderColonnes -> Colonne Order C'
- C' -> $\varepsilon$ | Virgule ListeOrderColonnes