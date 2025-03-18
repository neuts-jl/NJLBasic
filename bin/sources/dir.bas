print curdir()
filename = DIR("sources/*.bas")

WHILE fileName <> ""
    PRINT "Fichier : "; fileName
    fileName = DIR()
WEND
