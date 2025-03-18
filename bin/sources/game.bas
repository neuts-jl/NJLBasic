WIDTH = 800
HEIGHT = 500

' Définition de la raquette
paddle_x = 300
paddle_y = 450
paddle_width = 100
paddle_height = 20 
paddle_speed = 800

' Définition de la balle
ball_x = WIDTH/2
ball_y = HEIGHT/4
ball_radius = 10
ball_speed_x = 300
ball_speed_y = 300

DO
    CLS
    ' Gestion des touches pour la raquette
    key=INKEY()    
     
    IF mid(key,1,1)=CHR(0) THEN 
      KeyX=mid(key,2,1)
      IF (keyX = CHR(37)) THEN paddle_x = paddle_x - (paddle_speed * 0.02)
      IF (keyX = CHR(39)) THEN paddle_x = paddle_x + (paddle_speed * 0.02)
    END IF

    ' Empêcher la raquette de sortir de l'écran
    IF (paddle_x < 0) THEN 
       paddle_x = 0
    END IF   
    IF ((paddle_x + paddle_width) > WIDTH) THEN 
       paddle_x = WIDTH - paddle_width
    END IF
    ' Mise à jour de la position de la balle
    ball_x = ball_x + (ball_speed_x * 0.02)
    ball_y = ball_y + (ball_speed_y * 0.02)

    ' Vérifier les collisions avec les bords
    IF ((ball_x - ball_radius) < 0) OR ((ball_x + ball_radius) > WIDTH) THEN
        ball_speed_x = -ball_speed_x
    END IF

    IF ((ball_y - ball_radius) < 0) THEN
        ball_speed_y = -ball_speed_y
    END IF

    ' Vérifier la collision avec la raquette
    IF ((ball_y + ball_radius) >= paddle_y) AND (ball_x >= paddle_x) AND (ball_x <= (paddle_x + paddle_width)) THEN
        ball_speed_y = -ball_speed_y
    END IF

    IF ((ball_y + ball_radius) > HEIGHT) THEN
        ball_x = WIDTH/2
        ball_y = HEIGHT/4
    END IF

    ' Dessiner la raquette
    COLOR 1
    DRAW RECTANGLE paddle_x, paddle_y, (paddle_x + paddle_width), (paddle_y + paddle_height),1,0,0

    ' Dessiner la balle
    COLOR 4
    DRAW CIRCLE ball_x, ball_y, ball_radius,1,0,0
    
    DRAW WAIT FRAME 50
LOOP UNTIL key=chr(27)
CLS
