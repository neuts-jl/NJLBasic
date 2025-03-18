DIM tab(5) AS INTEGER  ' Déclare un tableau de 6 éléments (de 0 à 5)

' Remplissage du tableau
FOR i = 0 TO 5
    tab(i) = i * 10
NEXT i

' Affichage des valeurs
FOR i = 0 TO 5
    PRINT "tab("; i; ") = "; tab(i)
NEXT i
