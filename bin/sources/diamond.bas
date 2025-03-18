 CLS
 LINES=17
 FOR I=1 TO INT(LINES/2+1)
   FOR J=1 TO INT((LINES+1)/2-I+1)
     PRINT" ";
   NEXT
   FOR J=1 TO I*2-1
     PRINT"*";
   NEXT
 PRINT
 NEXT
 FOR I=1 TO INT(LINES/2)
   REM note misspelled variable is the same
   REM because variables are unique to only two characters
   FOR J=1 TO I+1
     PRINT" ";
   NEXT
   FOR J= 1 TO INT(((LINES+1)/2-I)*2-1)
     PRINT"*";
   NEXT
  PRINT
 NEXT
