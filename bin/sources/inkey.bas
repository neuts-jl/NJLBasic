10 PRINT "Test de INKEY$"
20 PRINT "Appuyez sur une touche pour voir son code."
30 PRINT "Les touches spéciales afficheront leur code."
40 PRINT "Appuyez sur 'Q' pour quitter."

50 DO
60     A = INKEY()
70     IF A <> "" THEN
80         IF Mid(A,1,1) = CHR(0) THEN
90             ' Si c'est une touche étendue, on lit la touche étendue
100            A = Mid(A,2,1)
110            PRINT "Touche étendue pressée: "; A; " : "; asc(A)
120        ELSE
130            ' Sinon, c'est un caractère normal
140            PRINT "Caractère pressé: "; A; " : "; asc(A) 
150        END IF
160    END IF
170 LOOP UNTIL UCase(A) = "Q"  ' Quitte le programme si 'Q' est pressé

180 PRINT "Vous avez quitté le programme."
190 END
