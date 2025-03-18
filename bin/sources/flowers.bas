SPRITE_COUNT = 20

DIM x(SPRITE_COUNT)
DIM y(SPRITE_COUNT)
DIM speed_x(SPRITE_COUNT)
DIM speed_y(SPRITE_COUNT)

IMAGE LOAD "sprite", "flower.png"

' Initialisation des positions et vitesses
FOR i = 0 TO SPRITE_COUNT - 1
    x(i) = INT(RND() * (SCREENX() - IMAGEX("sprite"))) 
    y(i) = INT(RND() * (SCREENY() - IMAGEY("sprite"))) 
    speed_x(i) = (RND() * 14 + 1) * (SGN(RND() - 0.5)) 
    speed_y(i) = (RND() * 14 + 1) * (SGN(RND() - 0.5))
NEXT

IMAGE LOAD "bg","background.jpg"

WHILE INKEY()=""
    WIDTH = SCREENX()
    HEIGHT = SCREENY()
    IMAGE DRAW "bg", 0, 0, WIDTH, HEIGHT
    
    FOR i = 0 TO SPRITE_COUNT - 1
        ' Dessine le sprite
        IMAGE DRAW "sprite", x(i), y(i)

        ' Met à jour la position
        x(i) = x(i) + speed_x(i)
        y(i) = y(i) + speed_y(i)

        ' Gestion des rebonds
        IF (x(i) < 0) OR (x(i) > (WIDTH - IMAGEX("sprite"))) THEN
            speed_x(i) = -speed_x(i)
            IF x(i) < 0 THEN x(i) = 0
            IF x(i) > (WIDTH - IMAGEX("sprite")) THEN x(i) = WIDTH - IMAGEX("sprite")
        END IF
        IF (y(i) < 0) OR (y(i) > (HEIGHT - IMAGEY("sprite"))) THEN
            speed_y(i) = -speed_y(i)
            IF y(i) < 0 THEN y(i) = 0
            IF y(i) > (HEIGHT - IMAGEY("sprite")) THEN y(i) = HEIGHT - IMAGEY("sprite")
        END IF
    NEXT

    DRAW WAIT FRAME 50
WEND
CLS
IMAGE CLEAR "sprite"
IMAGE CLEAR "bg"
  