CLS
PRINT "Load..."
CLEAR
SPRITE_COUNT = 10
DIM VX(SPRITE_COUNT)
DIM VY(SPRITE_COUNT)

FOR I = 1 TO SPRITE_COUNT

    ' Création du sprite avec une animation
    SPRITE_NAME = "SPRITE" + STR(I)
    SPRITE CREATE SPRITE_NAME, "monster.png", 2, 7

    ' Position initiale aléatoire sans dépasser les bords
    x = INT(RND() * (SCREENX()-SPRITE("WIDTH",SPRITE_NAME)))
    y = INT(RND() * (SCREENY()-SPRITE("HEIGHT",SPRITE_NAME)))

    SPRITE POSITION SPRITE_NAME,x,y

    VX(I) = INT(RND() * 6) + 1
    VY(I) = INT(RND() * 6) + 1

    ' Définition de l'animation "EYES" (ligne 1, frames 0 à 2, vitesse aléatoire)
    ' Définition de l'animation "EXPLODE" (ligne 2, frames 0 à 6, vitesse 0.1)
    ANIM_SPEED = RND() * 0.2 + 0.1
    SPRITE ANIMATION ADD SPRITE_NAME, "EYES", 0, 0, 2, ANIM_SPEED, TRUE
    SPRITE ANIMATION ADD SPRITE_NAME, "EXPLODE", 1, 0, 6, 0.1, FALSE
    SPRITE ANIMATION START SPRITE_NAME, "EYES"
NEXT

IMAGE LOAD "BG", "background.jpg"
COLOR 15
MEDIA PLAY "intro","GAMEON.WAV"

DO
    LIVES = 0
    IMAGE DRAW "BG", 0, 0, SCREENX(), SCREENY()
    DRAW TEXT 0,20,"Click on monster, Esc for EXIT","Arial",15,0

    FOR I = 1 TO SPRITE_COUNT
        SPRITE_NAME = "SPRITE" + STR(I)

        SPRITE MOVE SPRITE_NAME, VX(I), VY(I)

        IF SPRITE("EDGE_HIT",SPRITE_NAME) = "LEFT" THEN
            VX(I) = -VX(I)
        END IF
        IF  SPRITE("EDGE_HIT",SPRITE_NAME) = "RIGHT" THEN
            VX(I) = -VX(I)
            X = SCREENX() - SPRITE("WIDTH",SPRITE_NAME)
            Y = SPRITE("y",SPRITE_NAME)
            SPRITE POSITION SPRITE_NAME, X, Y
        END IF
        IF  SPRITE("EDGE_HIT",SPRITE_NAME) = "TOP" THEN
            VY(I) = -VY(I)
        END IF
        IF  SPRITE("EDGE_HIT",SPRITE_NAME) = "BOT" THEN
            VY(I) = -VY(I)
            X = SPRITE("x",SPRITE_NAME)
            Y = SCREENY() - SPRITE("HEIGHT",SPRITE_NAME)
            SPRITE POSITION SPRITE_NAME, X, Y
        END IF

        IF SPRITE("ANIM_END",SPRITE_NAME,"EXPLODE") THEN
           SPRITE HIDE SPRITE_NAME
        END IF
        IF SPRITE("VISIBILITY",SPRITE_NAME) THEN
          LIVES = LIVES + 1
        END IF

        IF SPRITE("MOUSE_BUTTON",SPRITE_NAME) = 1 THEN
            SPRITE ANIMATION START SPRITE_NAME, "EXPLODE"
            MEDIA STOP "explode"
            MEDIA PLAY "explode","BOOM3.WAV"
        END IF
    NEXT
    DRAW TEXT SCREENX()-50,20,STR((SPRITE_COUNT - LIVES) * 100)

    DRAW WAIT FRAME 50
LOOP UNTIL (INKEY() = CHR(27)) OR (LIVES = 0)
IF LIVES=0 THEN
    MEDIA PLAY "tada","TADABRS.WAV"
    DRAW TEXT INT(SCREENX()/2)-150,INT(SCREENY()/2)-40,"GAME OVER","ARIAL",40,1
    WHILE INKEY()=""
      DRAW WAIT FRAME 50
    WEND
END IF
MEDIA STOP "intro"
MEDIA STOP "explode"
MEDIA STOP "tada"
IMAGE CLEAR
'SPRITE CLEAR
CLS
