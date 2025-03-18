' Déclaration d'un tableau 2D (3x3)
DIM mat(2, 2) AS INTEGER

' Remplissage du tableau avec des valeurs
FOR i = 0 TO 2
    FOR j = 0 TO 2
        mat(i, j) = i * 10 + j
    NEXT j
NEXT i

' Affichage sous forme de grille
FOR i = 0 TO 2
    FOR j = 0 TO 2
        PRINT mat(i, j); " ";
    NEXT j
    PRINT  ' Nouvelle ligne après chaque rangée
NEXT i

