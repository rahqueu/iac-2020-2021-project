;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            VARIABLES AND CONSTANTS                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  
                ORIG    0000h

; ATUALIZAJOGO & GERACACTO 
VECTOR          TAB     80 
COUNTER         EQU     80 ; counter and size of the vector
HEIGHT          EQU     4
SEED            WORD    5 

; STRINGS FOR GAMEOVER
GAMEOVERSTR     STR     '■■■■■■■■■■■■■■■■■■■■■   G A M E  ·  O V E R   ■■■■■■■■■■■■■■■■■■■■■'
STUDENTSTR      STR     '                 jogo por INES JI e RAQUEL CARDOSO'

; VARIABLES FOR THE TIMER/SCORE
TIMER_COUNTVAL  WORD    TIMERCOUNT_INIT ; states the current counting period
TIMER_TICK      WORD    0               ; indicates the number of unattended
                                        ; timer interruptions
; VARIABLE TO START THE GAME
STARTGAME       WORD    0 ; (change to 1 to start)

; SCORE IN THE DISPLAY
D0              WORD    0
D1              WORD    0
D2              WORD    0
D3              WORD    0
D4              WORD    0
D5              WORD    0

; DINO VARIABLES
VAR_JUMP        WORD    0 ; dino status (0 - normal, 1 - going up, 
                          ; 2 - going down)
JUMP_POSITION   WORD    2406h ; dino position (address) when jumping
VAR_CRASH       WORD    1 ; number of rows dino's legs are above the floor

; Dino's height position
MAX_HEIGHT      EQU     1F06h
INITIAL_HEIGHT  EQU     2406h

; Terminal
TERM_READ       EQU     FFFFh	
TERM_WRITE      EQU     FFFEh	
TERM_STATUS     EQU     FFFDh	
TERM_CURSOR     EQU     FFFCh	
TERM_COLOR      EQU     FFFBh

; 7 segment display
DISP7_D0        EQU     FFF0h
DISP7_D1        EQU     FFF1h
DISP7_D2        EQU     FFF2h
DISP7_D3        EQU     FFF3h
DISP7_D4        EQU     FFEEh
DISP7_D5        EQU     FFEFh

; Timer
TIMER_CONTROL   EQU     FFF7h
TIMER_COUNTER   EQU     FFF6h
TIMER_SETSTART  EQU     1
TIMERCOUNT_INIT EQU     1

; Interruptions
INT_MASK        EQU     FFFAh
INT_MASK_VAL    EQU     0001h ; 0000 0000 0000 0001b ; only b0 is activated
INT_MASK_VAL2   EQU     80FFh ; 1000 0000 1111 1111b ; everything is activated



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      DINO GAME CODE                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                ORIG    0000h
                MVI     R6, 7000h
                
START:          MVI     R1,INT_MASK
                MVI     R2,INT_MASK_VAL
                STOR    M[R1],R2
                ENI
                
                ; waiting for b0 to be pressed
                MVI     R1, STARTGAME
                LOAD    R2, M[R1]
                CMP     R2, R0
                JMP.P   NEWGAME
                BR      START
                
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             MAIN FUNCTION                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              


MAIN:           JAL     FLOOR
                INC     R6
                JAL     DRAW_DINO
                
                MVI     R1,INT_MASK
                MVI     R2,INT_MASK_VAL2
                STOR    M[R1],R2
                ENI
                
                ; START TIMER
                MVI     R2,TIMERCOUNT_INIT
                MVI     R1,TIMER_COUNTER
                STOR    M[R1],R2          ; set timer to count 1x100ms
                MVI     R1,TIMER_TICK
                STOR    M[R1],R0          ; clear all timer ticks
                MVI     R1,TIMER_CONTROL
                MVI     R2,TIMER_SETSTART
                STOR    M[R1],R2          ; start timer
                
                ; WAIT FOR EVENT (TIMER)
.LOOP:          MVI     R5,TIMER_TICK
                LOAD    R1,M[R5]
                CMP     R1,R0
                BR.Z    .LOOP

                ; DEC TIMER TICK
                MVI     R2,TIMER_TICK
                DSI     ; critical region: if an interruption occurs, value 
                        ; might become wrong
                LOAD    R1,M[R2]
                DEC     R1
                STOR    M[R2],R1
                ENI
                
                ;CALL FUNCTIONS TO UPDATE THE GAME
                JAL     ATUALIZAJOGO
                JAL     DRAW_CACTUS
                INC     R6
                
                ;CHECK IF THERE IS A JUMP GOING ON
                MVI     R1, VAR_JUMP
                LOAD    R2, M[R1]
                MVI     R1, 1
                CMP     R2, R1
                JAL.Z   JUMPUP
                MVI     R1, 2
                CMP     R2, R1
                JAL.Z   JUMPDOWN
                
                JAL     CRASH

                ;UPDATE SCORE
                ;EACH TIME A SEGMENT REACHES 9, IN THE NEXT UPDATE CHANGES TO 0 
                ;AND THE NEXT SEGMENT INCREMENTS ONCE
.DISP0:         MVI     R1, D0
                LOAD    R3, M[R1]
                INC     R3
                STOR    M[R1], R3
                MVI     R2, 9
                CMP     R3, R2
                BR.P    .DISP1
                MVI     R2, DISP7_D0
                STOR    M[R2], R3
                
                BR      .LOOP
                
.DISP1:         STOR    M[R1], R0
                MVI     R2, DISP7_D0
                STOR    M[R2], R0
                
                MVI     R1, D1
                LOAD    R3, M[R1]
                INC     R3
                STOR    M[R1], R3
                MVI     R2, 9
                CMP     R3, R2
                BR.P    .DISP2
                MVI     R2, DISP7_D1
                STOR    M[R2], R3
                
                BR      .LOOP
                
.DISP2:         STOR    M[R1], R0
                MVI     R2, DISP7_D1
                STOR    M[R2], R0
                
                MVI     R1, D2
                LOAD    R3, M[R1]
                INC     R3
                STOR    M[R1], R3
                MVI     R2, 9
                CMP     R3, R2
                BR.P    .DISP3
                MVI     R2, DISP7_D2
                STOR    M[R2], R3
                
                BR      .LOOP

.DISP3:         STOR    M[R1], R0
                MVI     R2, DISP7_D2
                STOR    M[R2], R0
                
                MVI     R1, D3
                LOAD    R3, M[R1]
                INC     R3
                STOR    M[R1], R3
                MVI     R2, 9
                CMP     R3, R2
                BR.P    .DISP4
                MVI     R2, DISP7_D3
                STOR    M[R2], R3
                
                BR      .LOOP

.DISP4:         STOR    M[R1], R0
                MVI     R2, DISP7_D3
                STOR    M[R2], R0
                
                MVI     R1, D4
                LOAD    R3, M[R1]
                INC     R3
                STOR    M[R1], R3
                MVI     R2, 9
                CMP     R3, R2
                BR.P    .DISP5
                MVI     R2, DISP7_D4
                STOR    M[R2], R3
                
                BR      .LOOP

.DISP5:         STOR    M[R1], R0
                MVI     R2, DISP7_D4
                STOR    M[R2], R0
                
                MVI     R1, D5
                LOAD    R3, M[R1]
                INC     R3
                STOR    M[R1], R3
                MVI     R2, 9
                CMP     R3, R2
                JMP.P   .LOOP
                MVI     R2, DISP7_D5
                STOR    M[R2], R3
                
                JMP     .LOOP
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  DRAWING FLOOR                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

FLOOR:          DEC     R6
                STOR    M[R6], R7
                MVI     R1, TERM_COLOR
                MVI     R3, 00F1h
                STOR    M[R1], R3 

                MVI     R2, 2500h
                
                ;LOOP TO DRAW THE SAME CHARACTER IN EVERY POSITION UNTIL WE 
                ;REACH THE POSITION WE WANT
                
.DRAW_FLOOR:    MVI     R1, TERM_CURSOR
                
                MVI     R5, 3050h
                CMP     R5, R2
                LOAD    R7, M[R6]
                JMP.Z   R7
                
                STOR    M[R1], R2
                INC     R2
                MVI     R4, 178
                MVI     R1, TERM_WRITE
                STOR    M[R1], R4
                
                BR      .DRAW_FLOOR


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  DRAWING DINO                               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

DRAW_DINO:      DEC     R6
                STOR    M[R6], R7
                
                MVI     R1, TERM_COLOR
                MVI     R3, 0015h
                STOR    M[R1], R3 
                
                MVI     R2, 2406h
                MVI     R1, TERM_CURSOR

                STOR    M[R1], R2
                MVI     R1, TERM_WRITE
                MVI     R4, 202
                STOR    M[R1], R4
                
                MVI     R1, 0100h
                SUB     R2, R2, R1
                MVI     R1, TERM_CURSOR
                STOR    M[R1], R2
                MVI     R1, TERM_WRITE
                MVI     R4, 2
                STOR    M[R1], R4
                
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7
                
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  DRAWING CACTUS                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                
                
DRAW_CACTUS:    DEC     R6
                STOR    M[R6], R7
                MVI     R4, 0000h
                MVI     R2, 23FFh
                
                ;GO TO EACH POSITION IN THE VECTOR TO LOAD THE VALUE AND DRAW 
                ;THE CACTUS/BLANK IN THE ACCORDINGLY POSITION OF THE TERMINAL 
                
.DRAW:          MVI     R1, TERM_COLOR
                MVI     R3, 0015h
                STOR    M[R1], R3 

                INC     R2
                MVI     R1, TERM_CURSOR
                STOR    M[R1], R2
                
                ;DINO POSITION IS AN EXCEPTION
                MVI     R1, 2406h
                CMP     R1, R2
                BR.Z    .DRAW
                
                LOAD    R7, M[R6]
                MVI     R1, 2450h ; END OF THE ROW (80d POSITIONS)
                CMP     R1, R2
                JMP.Z   R7
                
                LOAD    R5, M[R4]
                INC     R4
                
                ;CHECK THE VALUE TO KNOW WHAT IS THE HEIGHT OF THE CACTUS TO 
                ;DRAW
                ;IF 0, THERE IS NO CACTUS IN THAT POSITION
                CMP     R5, R0
                BR.Z    .CLEAN
                MVI     R1, 1
                CMP     R5, R1
                BR.Z    .CACT_HEIGHT1
                MVI     R1, 2
                CMP     R5, R1
                BR.Z    .CACT_HEIGHT2
                MVI     R1, 3
                CMP     R5, R1
                BR.Z    .CACT_HEIGHT3
                MVI     R1, 4
                CMP     R5, R1
                BR.Z    .CACT_HEIGHT4
                
                
.CACT_HEIGHT1:  MVI     R1, TERM_WRITE
                MVI     R5, 182
                STOR    M[R1], R5
                
                BR      .DRAW
                
.CACT_HEIGHT2:  MVI     R1, TERM_WRITE
                MVI     R5, 182
                STOR    M[R1], R5
                MOV     R1, R2
                MVI     R5, 0100h
                SUB     R1, R1, R5
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R5, 210
                MVI     R1, TERM_WRITE
                STOR    M[R1], R5
                
                BR      .DRAW
                
.CACT_HEIGHT3:  MVI     R1, TERM_WRITE
                MVI     R5, 182
                STOR    M[R1], R5
                
                MOV     R1, R2
                MVI     R5, 0100h
                SUB     R1, R1, R5
                DEC     R6
                STOR    M[R6], R1
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R5, 199
                MVI     R1, TERM_WRITE
                STOR    M[R1], R5
                
                LOAD    R1, M[R6]
                INC     R6
                MVI     R5, 0100h
                SUB     R1, R1, R5
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R5, 210
                MVI     R1, TERM_WRITE
                STOR    M[R1], R5
                
                BR      .DRAW
                
.CACT_HEIGHT4:  MVI     R1, TERM_WRITE
                MVI     R5, 182
                STOR    M[R1], R5
                
                MOV     R1, R2
                MVI     R5, 0100h
                SUB     R1, R1, R5
                DEC     R6
                STOR    M[R6], R1
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R5, 199
                MVI     R1, TERM_WRITE
                STOR    M[R1], R5
                
                LOAD    R1, M[R6]
                MVI     R5, 0100h
                SUB     R1, R1, R5
                STOR    M[R6], R1
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R5, 182
                MVI     R1, TERM_WRITE
                STOR    M[R1], R5
                
                LOAD    R1, M[R6]
                INC     R6
                MVI     R5, 0100h
                SUB     R1, R1, R5
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R5, 210
                MVI     R1, TERM_WRITE
                STOR    M[R1], R5
                JMP     .DRAW
                
                ;TO CLEAN IN CASE OF THERE BEING CACTUS IN THIS POSITION 
                ;IN THE LAST UPDATE OF THE VECTOR (ATUALIZAJOGO)
.CLEAN:         MVI     R1, TERM_WRITE
                STOR    M[R1], R0
                
                MOV     R1, R2
                MVI     R5, 0100h
                SUB     R1, R1, R5
                DEC     R6
                STOR    M[R6], R1
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R1, TERM_WRITE
                STOR    M[R1], R0
                LOAD    R1, M[R6]
                
                MVI     R5, 0100h
                SUB     R1, R1, R5
                STOR    M[R6], R1
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R1, TERM_WRITE
                STOR    M[R1], R0
                LOAD    R1, M[R6]
                INC     R6
                
                MVI     R5, 0100h
                SUB     R1, R1, R5
                MVI     R5, TERM_CURSOR
                STOR    M[R5], R1
                MVI     R1, TERM_WRITE
                STOR    M[R1], R0
                
                JMP     .DRAW
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  DINO JUMP                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;JUMPUP/JUMPDOWN
;MOVES THE DINO ABOVE/BELOW ONCE (-0100h/+0100h) IN THE TERMINAL
;CLEANING THE LEGS/HEAD AND DRAWING EVERY CHARACTER OF THE DINO ONE POSITION 
;ABOVE/BELOW FROM WHERE IT WAS
                
JUMPUP:         MVI     R2, VAR_JUMP
                MVI     R5, 1 ; dino is jumping
                STOR    M[R2], R5

                DEC     R6
                STOR    M[R6], R7
                MVI     R2, JUMP_POSITION
                LOAD    R1, M[R2]
                MVI     R2, TERM_CURSOR
                STOR    M[R2], R1
                MVI     R2, TERM_WRITE
                STOR    M[R2], R0
                
                MVI     R5, 0100h
                SUB     R1, R1, R5
                MVI     R2, JUMP_POSITION
                STOR    M[R2], R1
                MVI     R2, TERM_CURSOR
                STOR    M[R2], R1
                MVI     R4, 202
                MVI     R2, TERM_WRITE
                STOR    M[R2], R4
                
                SUB     R1, R1, R5
                MVI     R2, TERM_CURSOR
                STOR    M[R2], R1
                MVI     R4, 2
                MVI     R2, TERM_WRITE
                STOR    M[R2], R4
                
                MVI     R2, VAR_CRASH
                LOAD    R4, M[R2]
                INC     R4
                STOR    M[R2], R4
                
                MVI     R2, JUMP_POSITION
                LOAD    R1, M[R2]
                MVI     R2, MAX_HEIGHT ; when the JUMP_POSITION == MAX HEIGHT, 
                ;it changes VAR_JUMP, so the dino goes down in the next update
                CMP     R1, R2
                BR.Z    .CHANGE_VAL
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7
                

.CHANGE_VAL:    MVI     R2, VAR_JUMP
                MVI     R4, 2 ; dino is going down (in the next update)
                STOR    M[R2], R4
                
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7
                
                
JUMPDOWN:       DEC     R6
                STOR    M[R6], R7
                
                MVI     R2, JUMP_POSITION
                LOAD    R1, M[R2]
                MVI     R5, 0100h
                SUB     R1, R1, R5
                MVI     R2, TERM_CURSOR
                STOR    M[R2], R1
                MVI     R2, TERM_WRITE
                STOR    M[R2], R0
                
                ADD     R1, R1, R5
                MVI     R2, TERM_CURSOR
                STOR    M[R2], R1
                MVI     R4, 2
                MVI     R2, TERM_WRITE
                STOR    M[R2], R4
                
                ADD     R1, R1, R5
                MVI     R2, JUMP_POSITION
                STOR    M[R2], R1
                MVI     R2, TERM_CURSOR
                STOR    M[R2], R1
                MVI     R4, 202
                MVI     R2, TERM_WRITE
                STOR    M[R2], R4
                
                MVI     R2, VAR_CRASH
                LOAD    R4, M[R2]
                DEC     R4
                STOR    M[R2], R4
                
                MVI     R2, INITIAL_HEIGHT
                CMP     R1, R2 ;if both addresses are the same, 
                        ;in the next update dino doesn't move
                BR.Z    .CHANGE_VAL
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7
                
.CHANGE_VAL:    MVI     R2, VAR_JUMP
                MVI     R5, 0 ; dino remains in the place (in the next update)
                STOR    M[R2], R5
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7        
                
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  ATUALIZAJOGO&GERACACTO                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                   
                
ATUALIZAJOGO:   DEC     R6
                STOR    M[R6], R7
                MVI     R1, VECTOR ; Initial address of the vector
                MVI     R2, COUNTER ; Counter and size of the vector
                MOV     R5, R2 
                DEC     R5

.loop:          INC     R1
                LOAD    R4, M[R1]
                DEC     R1 
                STOR    M[R1], R4  
                
                ; USING THE LOOP, EACH ELEMENT OF THE VECTOR GOES ONE POSITION 
                ; TO THE LEFT UNTIL IT REACHES THE END OF THE VECTOR
                
                DEC     R5 
                INC     R1 
                CMP     R5, R0
                BR.NZ   .loop 
                
                DEC     R6
                STOR    M[R6], R1
                
                MVI     R1, HEIGHT
                JAL     GERACACTO
                LOAD    R1, M[R6] ; load the last position of the vector
                STOR    M[R1], R3 ; store the result of the geracacto function 
                INC     R6
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7
                
GERACACTO:      DEC     R6
                STOR    M[R6], R7
                
                MVI     R2, SEED
                LOAD    R4, M[R2] 

                MVI     R3, 1
                AND     R5, R4, R3; AND bit a bit, bit in R5
                SHR     R4 ; Shift to the right, x >> 1
                
                ;if bit:
                MVI     R3, b400h
                CMP     R5, R0
                BR.Z    RETURN
                XOR     R4, R4, R3
                
                ;if x < 62258:
RETURN:         MVI     R3, 62258
                CMP     R4, R3
                BR.C    RETURNZERO
                
                ;;;;;;;;else:                
                DEC     R1 ; 
                AND     R3, R4, R1 
                INC     R3 ; R3 = (x & (height - 1)) + 1
                STOR    M[R2], R4 
                
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7 
                ; IN BOTH STOR M[R2], R4 IT SAVES THE UPDATED SEED
                
RETURNZERO:     STOR    M[R2], R4 
                MOV     R3, R0 
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7 
                
                
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  CRASH&GAMEOVER                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;COMPARE THE NUMBER OF ROWS DINO'S LEGS ARE ABOVE THE FLOOR 
;(VAR_CRASH) WITH THE HEIGHT OF THE CACTUS
;IF DINO'S LEGS AREN'T ABOVE THE HEIGHT OF THE CACTUS 
;(VAR_CRASH < CACTUS'HEIGHT), THERE WAS A CRASH

CRASH:          MVI     R1, 0006h
                LOAD    R4, M[R1]
                MVI     R1, VAR_CRASH
                LOAD    R5, M[R1]
                CMP     R5, R4
                BR.NP   .GAMEOVER
                JMP     R7
                
                ;IF THERE IS A CRASH, IT WRITES GAME OVER
.GAMEOVER:      MVI     R1, STARTGAME
                STOR    M[R1], R0
                
                MVI     R1, TERM_COLOR
                MVI     R2, 00FFh
                STOR    M[R1], R2
                
                MVI     R1, TERM_CURSOR
                MVI     R2, 1607H
                
                STOR    M[R1], R2
                MVI     R1, GAMEOVERSTR
                
.loop1:           
                LOAD    R2, M[R1]
                CMP     R2, R0
                BR.Z    .STUDENTS
                
                MVI     R5, TERM_WRITE
                STOR    M[R5], R2
                
                INC     R1
                BR      .loop1
                
.STUDENTS:      MVI     R1, TERM_CURSOR
                MVI     R2, 1707H
                
                STOR    M[R1], R2
                MVI     R1, STUDENTSTR
                
.loop2:           
                LOAD    R2, M[R1]
                CMP     R2, R0
                JMP.Z   START
                
                MVI     R5, TERM_WRITE
                STOR    M[R5], R2
                
                INC     R1
                BR      .loop2
                
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  RESTART: NEWGAME                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

;RESTART EVERYTHING: TERMINAL, DISPLAY, VECTOR (ATUALIZAJOGO) 
;AND SEED 
                
NEWGAME:        MVI     R1, TERM_CURSOR
                MVI     R2, FFFFh
                STOR    M[R1], R2
                
                MVI     R1, D0
                STOR    M[R1], R0
                MVI     R2, DISP7_D0
                STOR    M[R2], R0
                
                MVI     R1, D1
                STOR    M[R1], R0
                MVI     R2, DISP7_D1
                STOR    M[R2], R0
                
                MVI     R1, D2
                STOR    M[R1], R0
                MVI     R2, DISP7_D2
                STOR    M[R2], R0
                
                MVI     R1, D3
                STOR    M[R1], R0
                MVI     R2, DISP7_D3
                STOR    M[R2], R0
                
                MVI     R1, D4
                STOR    M[R1], R0
                MVI     R2, DISP7_D4
                STOR    M[R2], R0
                
                MVI     R1, D5
                STOR    M[R1], R0
                MVI     R2, DISP7_D5
                STOR    M[R2], R0
                
                MVI     R1, VECTOR
.LOOP:          STOR    M[R1], R0
                INC     R1
                MVI     R2, 0050h
                CMP     R1, R2
                BR.NZ   .LOOP
                
                MVI     R1, SEED
                MVI     R2, 5
                STOR    M[R1], R2                
                
                JMP     MAIN
                
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  AUXILIARY INTERRUPT SERVICE ROUTINES       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

AUX_TIMER_ISR:  DEC     R6
                STOR    M[R6], R7
                DEC     R6
                STOR    M[R6],R1
                DEC     R6
                STOR    M[R6],R2
                ; RESTART TIMER
                MVI     R1,TIMER_COUNTVAL
                LOAD    R2,M[R1]
                MVI     R1,TIMER_COUNTER
                STOR    M[R1],R2          ; set timer to count value
                MVI     R1,TIMER_CONTROL
                MVI     R2,TIMER_SETSTART
                STOR    M[R1],R2          ; start timer
                ; INC TIMER FLAG
                MVI     R2,TIMER_TICK
                LOAD    R1,M[R2]
                INC     R1
                STOR    M[R2],R1
                
                LOAD    R2,M[R6]
                INC     R6
                LOAD    R1,M[R6]
                INC     R6
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7             
    
                ;CHANGES THE VARIABLE STARTGAME, TO START THE GAME
AUX_START_GAME: DEC     R6
                STOR    M[R6], R7
                MVI     R1, STARTGAME
                MVI     R2, 1
                STOR    M[R1], R2
                LOAD    R7, M[R6]
                INC     R6
                JMP     R7
                
                ;IF THERE ISN'T A JUMP GOING ON (VAR_JUMP = 0) THEN IT CHANGES 
                ;THE VARIABLE TO 1, STARTING THE JUMP IN THE NEXT UPDATE 
AUX_TO_JUMP:    MVI     R1, VAR_JUMP
                LOAD    R4, M[R1]
                CMP     R4, R0
                BR.NZ   .return
                MVI     R2, 1
                STOR    M[R1], R2   
.return:        JMP     R7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  INTERRUPT SERVICE ROUTINES                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

                ORIG    7FF0h
                
TIMER_ISR:      DEC     R6
                STOR    M[R6],R7
                JAL     AUX_TIMER_ISR
                LOAD    R7,M[R6]
                INC     R6
                RTI


                ORIG    7F00h
                
KEYZERO:        DEC     R6
                STOR    M[R6],R1
                DEC     R6
                STOR    M[R6],R2
                JAL     AUX_START_GAME
                STOR    M[R1],R2
                LOAD    R2,M[R6]
                INC     R6
                LOAD    R1,M[R6]
                INC     R6
                RTI 


                ORIG    7F30h
                
KEYUP:          DEC     R6
                STOR    M[R6],R1
                DEC     R6
                STOR    M[R6],R7
                JAL     AUX_TO_JUMP
                LOAD    R7,M[R6]
                INC     R6
                LOAD    R1,M[R6]
                INC     R6
                RTI
                
                
                
                
