SUB AddNumbers(A, B)
  PRINT A; " + "; B; " = "; A + B
END SUB

x=1
CALL AddNumbers(5, 10)
PRINT "Back from AddNumbers"
CALL AddNumbers(20, 30)
PRINT "Back from AddNumbers again"
print x
CALL AddNumbers(x,x*3)
print "error on next line"
print A
