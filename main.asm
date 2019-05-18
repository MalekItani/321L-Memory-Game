    title	"Memory Game"
    list	p=16f84A
    radix	hex
    include "p16f84a.inc"

    
; COUNT registers: These are registers that will be used within delay functions.
COUNT1	EQU	d'12'
COUNT2	EQU	d'13'
COUNT3	EQU	d'14'
COUNT4	EQU	d'15'
COUNT5	EQU	d'16'
COUNT6	EQU	d'17'
COUNT7	EQU	d'22'
COUNT8	EQU	d'23'
COUNT9	EQU	d'24'
COUNT10	EQU	d'25'
SCORE	EQU	d'26'
NUMBER_OF_TENS_OF_SECONDS	EQU	d'26' ; Indicates the timer/10

CURSOR_POS  EQU	d'18' ; Register stores detail about the cursor's current position. Bits[3:0] specify column number, where as bit 4 specifies the row.
		      ; Since there are 16 columns and 2 rows, such an encoding is guaranteed to span all cells on the LCD.

FLAG_REG    EQU d'20' ; Register containing some flags to be used within the program. Structure: [-,-,MODE1_WEAK,MODE1_AVG/MODE2_RESULT,GAME_OVER,CONFIRM_STATUS,MODE_BIT2,MODE_BIT1] within the program. Note that one bit can serve multiple purposes depending on the game mode.

TMP	EQU d'21'  
LEFT_BTN    EQU	d'4'
RIGHT_BTN   EQU	d'5'
UP_DOWN_BTN EQU	d'6'
CONFIRM_BTN EQU	d'7'

; Character code: A character code is an eight bit number representing one of the possible printable characters on the LCD. The printing of a character encoded by a Character Code shall be handled completely by the WRITE_CHAR function.
CHAR_CODE   EQU	d'19' ; Register containing the Character Code of the character the user wishes to print.

; Predefined Character Codes defined for the sake of code readability.
LTR_A EQU b'01000001'
LTR_B EQU b'01000010' 
LTR_C EQU b'01000011' 
LTR_D EQU b'01000100'
LTR_E EQU b'01000101'
LTR_F EQU b'01000110'
LTR_G EQU b'01000111' 
LTR_H EQU b'01001000'
LTR_I EQU b'01001001' 
LTR_J EQU b'01001010' 
LTR_K EQU b'01001011'
LTR_L EQU b'01001100'
LTR_M EQU b'01001101' 
LTR_N EQU b'01001110'
LTR_O EQU b'01001111' 
LTR_P EQU b'01010000' 
LTR_Q EQU b'01010001' 
LTR_R EQU b'01010010'
LTR_S EQU b'01010011'  
LTR_T EQU b'01010100'  
LTR_U EQU b'01010101'  
LTR_V EQU b'01010110'  
LTR_W EQU b'01010111' 
LTR_X EQU b'01011000'  
LTR_Y EQU b'01011001'
LTR_Z EQU b'01011010'
LTR_0 EQU b'00110000'
LTR_1 EQU b'00110001'
LTR_2 EQU b'00110010'
LTR_3 EQU b'00110011'
LTR_4 EQU b'00110100'
LTR_5 EQU b'00110101'
LTR_6 EQU b'00110110'
LTR_7 EQU b'00110111'
LTR_8 EQU b'00111000'
LTR_9 EQU b'00111001'
LTR_ASTERISK EQU b'00101010'
LTR_SPACE   EQU b'10100000'
LTR_BOX   EQU b'11011011' 
LTR_SEPARATOR	EQU	b'10100011'
LTR_SHADE   EQU	b'11111111'
LTR_MINUS	EQU b'00101101'
LTR_PLUS	EQU	b'00101011'
	
; Explanation of cell register choice:
; Here we intend to use indirect addressing to access the elements inside the cells, given that our cursor pos is encoded in such a way where
; bit 4 corresponds to row number and bits[3:0] correspond to col number, and since the board is 2x6 cells wide, then the valid cursor positions
; in-game will range from 00000 to 00101 on (0 to 5) in row 1 and from 10000 to 10101 (16 to 21) in row 2. In order to make it simple to access,
; and in order to use valid registed to store the Letters in, we add some offset to the FSR, which is 42, chosen so that the last cell (with CURSOR_POS=21) occupies the
; last available register in memory (register 63).
CELL_11 EQU d'42'
CELL_12 EQU d'43'
CELL_13 EQU d'44'
CELL_14 EQU d'45'
CELL_15 EQU d'46'
CELL_16 EQU d'47'

CELL_21 EQU d'58'
CELL_22 EQU d'59'
CELL_23 EQU d'60'
CELL_24 EQU d'61'
CELL_25 EQU d'62'
CELL_26 EQU d'63'   
 
CARD1	EQU d'48' ; Registers that store the value of the two cards that are selected.
CARD2	EQU d'49'

WEAKNESS    EQU d'50'   ; Holds the weakness for mode1

NUM_LEFT EQU	d'53'  ; Holds the number of remaining card pairs that have not yet been matched.
NUM_ERRORS  EQU	d'54'   ; Holds the number of incorrect guesses the user has inputted.
NUM_CORRECT  EQU	d'55'  ; Holds the number of cards that have been correctly guessed. Although NUM_LEFT & NUM_CORRECT can be merged into one register, each register contains information
                                            ; in a more useful form depending on the Mode.
SCORE_REG	EQU	d'56'
	
CELL_ADDRESS	EQU d'57'  ; Temporary register that stores the address of the first card that is opened (so as not to overwrite it when opening the second)


	ORG	0x0
	GOTO	MAIN  ; Start progrm from the MAIN label
	ORG	0x04   ; Handle interrupts

	BTFSC	INTCON, d'0' ; If RB4 is set, then the interrupt is on one of the buttons.
	GOTO RB4_INT

	BTFSC	INTCON, d'2' ; If TMR0 is set, then the interrupt is on TMR0, i.e. due to the timer in Mode 2.
	GOTO	TMR0_INT

	
; Register Configuration
MAIN	CLRF	PORTA     ; Reset PORTA and PORTB, and correctly setup inputs & outputs.
	CLRF	PORTB
	BSF	STATUS,RP0
	MOVLW	b'11110000' ; Set   RB[7:4] as inputs (buttons), the rest are outputs (Speaker, LEDs, LCD Enable, etc...)
	MOVWF	TRISB
	
	CLRF	TRISA	    ; Set all pins of PORTA as outputs
	BCF	STATUS,RP0

; Initialize the LCD
; Simple routine to setup the LCD. No blinking, no cursor(yet), standard settings.
INIT	CALL 	DELAYFMS
	MOVLW	b'00010'
	MOVWF	PORTA
	CALL 	ET
	MOVLW	b'00010'
	MOVWF	PORTA
	CALL	ET
	MOVLW	b'01010'
	MOVWF	PORTA
	CALL 	ET
	
	MOVLW	b'00000'
	MOVWF	PORTA
	CALL 	ET
	MOVLW	b'01100'
	MOVWF	PORTA
	CALL	ET
	
	MOVLW	b'00000'
	MOVWF	PORTA
	CALL 	ET
	MOVLW	b'00001'
	MOVWF	PORTA
	CALL 	ET

	MOVLW	b'00000'
	MOVWF	PORTA
	CALL 	ET
	MOVLW	b'00110'
	MOVWF	PORTA
	CALL 	ET

; Print the words "Memory Game" on the screen, and wait for 3 seconds
WELCOME_SCREEN 
	CALL	CLEAR_SCREEN
	CLRF	FLAG_REG
	MOVLW b'00000010' ; Move the cursor to 0x04
	MOVWF CURSOR_POS
	
	CALL PRINT_M
	CALL PRINT_E
	CALL PRINT_M
	CALL PRINT_O
	CALL PRINT_R
	CALL PRINT_Y
	
	MOVLW b'00001010' ; Move the cursor to 0x0A (Column 10)
	MOVWF CURSOR_POS
	
	CALL PRINT_G
	CALL PRINT_A
	CALL PRINT_M
	CALL PRINT_E
	
	MOVLW b'00000000' ; Move the cursor to 0x00
	MOVWF CURSOR_POS
	
	CALL DELAY3S


; Present the user with the Main Menu or Mode Selection screen, enable interrupts only once the screen has finished loading.
MAIN_MENU   CALL    CLEAR_SCREEN    ; Reset everything, this makes sure nothing from previous games affects the next one. Note: Mode specific registers are cleared right after mode selection
	CLRF	PORTB
	CALL	HIDE_CURSOR
	CLRF	FLAG_REG
	MOVLW	0
	MOVWF	CURSOR_POS
	CALL PRINT_M
	CALL PRINT_O
	CALL PRINT_D
	CALL PRINT_E
	
	MOVLW d'3'   ; Move the cursor three units to the right
	ADDWF CURSOR_POS, 1
	
	CALL PRINT_1
	
	MOVLW d'2'   ; Move the cursor two units to the right
	ADDWF CURSOR_POS, 1
	
	CALL PRINT_2
	
	MOVLW d'2'   ; Move the cursor two units to the right
	ADDWF CURSOR_POS, 1
	
	CALL PRINT_3
	MOVLW	b'00000110'
	MOVWF	CURSOR_POS
	CALL PRINT_ASTERISK

	MOVLW	b'10001000' ; Properly setup the interrupts. Only GIE and RBIE should be set.
	MOVWF	INTCON

; Check which mode the user has selected, if any. Note that the FLAG_REG is changed in the interrupt when the user click on the the Confirm button.
INF BTFSC   FLAG_REG,1  ; Check if bit 1 is set, if it is then it the user either wants Mode 3 or 2, otherwise, we are either still in the Mode selection, or we need to go to Mode 1
    GOTO  MODE2_OR_3
    GOTO  MODE1_OR_INF

MODE2_OR_3 BTFSC    FLAG_REG,0  ; Check bit 0 to determine if we should go to Mode 2 or 3.
    GOTO  MODE3
    GOTO  MODE2

MODE1_OR_INF BTFSC  FLAG_REG,0  ; Check bit 0 to determine if we should go to Mode 1 or stay in the Main Menu.
    GOTO  MODE1_OR_3	; Here we check the state of the FLAG_REG one last time. Although this may seem redundant, it is necessary since
                                        ; This fixes problems when interrupt is called right before checking for MODE1 or INF, when it should be MODE3, but the controller would have already skipped that check.
    GOTO  INF

MODE1_OR_3 BTFSC    FLAG_REG,1  ; The fix for the small issue presented above.
    GOTO  MODE3
    GOTO  MODE1

MODE1 	CALL  SETUP  ; Show the cards and print the Scoreboard for Mode 1.
	CALL	MODE1_SCOREBOARD
	CALL	RESET_CURSOR
  	
LOOP_M1 BTFSS	FLAG_REG, 3	; Check if the game is over (pin 3 on the FLAG_REG is set)
	GOTO	LOOP_M1
	BCF INTCON, 7	; Disable all interrupts
	MOVLW	b'00011001'
	MOVWF	CURSOR_POS
	BTFSC	FLAG_REG, 5	;  Check if the WEAK flag is raised
	GOTO	MODE1_WEAK
	BTFSC	FLAG_REG, 4	;  Check if the AVG flag is raised
	GOTO	MODE1_AVG
	GOTO	MODE1_SUPER	;  Default Case the player should be shown SUPER
	
	
; Handle the different endings for Mode 1 as specified in the Manual.
MODE1_WEAK  CALL    PRINT_W
	CALL    PRINT_E
	CALL    PRINT_A
	CALL    PRINT_K
	BSF	PORTB, 2
	CALL	DELAY3S
	CALL	DELAY3S
	GOTO	MAIN_MENU

MODE1_AVG CALL    PRINT_A
	CALL    PRINT_V
	CALL    PRINT_G
	BSF	PORTB, 3
	CALL	DELAY3S
	CALL	DELAY3S
	GOTO	MAIN_MENU
	
MODE1_SUPER	CALL    PRINT_S
	CALL    PRINT_U
	CALL    PRINT_P
	CALL    PRINT_E
	CALL    PRINT_R
	MOVLW	d'3'
	MOVWF	COUNT1
	CALL	PATTERN_LOOP
	CLRF	FLAG_REG
	GOTO	MAIN_MENU

; Code for Mode 2

MODE2 
	BSF	STATUS,RP0
	MOVLW	b'10000111' ; Update the option register(TMR0 clk source is the internal instruction cycle clk)
	MOVWF	OPTION_REG	; TMR0 Rate = 1:256
	BCF	STATUS,RP0
	CALL  SETUP
	MOVLW	d'6'
	MOVWF	NUM_LEFT
	CLRF	NUM_CORRECT
	MOVLW	d'9'
	MOVWF	NUMBER_OF_TENS_OF_SECONDS
	MOVLW	d'152'
	MOVLW	b'00001000'
	MOVWF	CURSOR_POS	
	CALL	PRINT_R
	CALL	PRINT_E
	CALL	PRINT_M
	CALL	PRINT_SPACE
	CALL	PRINT_T
	MOVLW	b'00110000'	;Add a bias that corresponds to zero
	ADDWF	NUMBER_OF_TENS_OF_SECONDS,0
	CALL	WRITE_CHAR
	CALL	PRINT_0
	CALL	RESET_CURSOR
	MOVWF	COUNT10
		
	BSF INTCON, 5	; The TMR0 interrupt enable flag (T0IE  bit 5 of the INTCON register) must be set
	BSF	INTCON,	4
	CLRF	TMR0
	BSF	INTCON,	7
;Calculations:
;TMR0 overflows every 1us*256*256 = 0.065536 s
;To achieve a 10s delay TMR0 should overflow ~ 152 times

LOOP_M2	BTFSC	FLAG_REG,4	;If bit4 of this register is 1 then the time has finished -> go to result 
	GOTO	MODE2_RESULT
	BTFSC	FLAG_REG,3
	GOTO	MODE2_RESULT
	GOTO	LOOP_M2 
MODE2_RESULT	CLRF	INTCON	;Disable all interrupts	
	MOVLW	b'00011000'
	MOVWF	CURSOR_POS
	CALL	PRINT_S
	CALL	PRINT_C
	CALL	PRINT_O
	CALL	PRINT_R
	CALL	PRINT_E
	CALL	PRINT_SPACE
	MOVFW	NUM_CORRECT
	ADDWF	NUMBER_OF_TENS_OF_SECONDS,0 ; W contains the score
	MOVWF	SCORE
	MOVLW	d'10'
	SUBWF	SCORE,0 ;	W = SCORE - 10
 	BTFSC	STATUS,0 ;Set if result is negative i.e. score<10
	CALL	ONE												
	MOVLW	b'00110000'	;Add a bias that corresponds to zero		
	ADDWF	SCORE,0
	CALL	WRITE_CHAR

	CLRF	PORTB
	CLRF	FLAG_REG

 	CALL	DELAY3S                                 
	CALL	DELAY3S									
	CALL	DELAY3S
	CALL	DELAY3S
	
	CLRF	INTCON
	GOTO	MAIN_MENU	

ONE	CALL	PRINT_1
	MOVLW	d'10'
	SUBWF	SCORE,1
	RETURN

; Code for Mode 3
	
MODE3 CALL  SETUP
	CALL	MODE3_SCOREBOARD
	CLRF	NUM_ERRORS
	CLRF	NUM_CORRECT
	CALL	RESET_CURSOR
LOOP_M3	BTFSC	FLAG_REG, 3
	GOTO	MAIN_MENU
	GOTO    LOOP_M3


; SETUP: Function that loads the correct letters in the Cell registers  and prints boxes to "hide" them.
SETUP
	BCF	INTCON, 7
	
	CALL	CLEAR_SCREEN
	CALL 	POPULATE_TABLE
	CALL	HIDE_TABLE
	
	MOVLW	6
	MOVWF	NUM_LEFT ; There are initially 6 pairs to find.
	
	RETURN


; POPULATE_TABLE: In order to print the characters while also preserving space in the cell registers for flag bits, the letters are encoded according to the following:
; A -> 0
; B -> 1
; C -> 2
; D -> 3
; E -> 4
; F -> 5
; In order to print the characters correctly, it suffices only to add the bias (which is already defined in LTR_A) and call WRITE_CHAR.
POPULATE_TABLE MOVLW	2	; Encoded Letter C
    MOVWF  CELL_11
    MOVLW  0	; Encoded Letter A
    MOVWF  CELL_12
    MOVLW  4	; Encoded Letter E
    MOVWF  CELL_13
    MOVLW  5	; Encoded Letter F
    MOVWF  CELL_14
    MOVLW  1	; Encoded Letter B
    MOVWF  CELL_15
    MOVLW  3	; Encoded Letter D
    MOVWF  CELL_16
    MOVLW  1	; Encoded Letter B
    MOVWF  CELL_21
    MOVLW  3	; Encoded Letter D
    MOVWF  CELL_22
    MOVLW  5	; Encoded Letter F
    MOVWF  CELL_23
    MOVLW  2	; Encoded Letter C
    MOVWF  CELL_24
    MOVLW  0	; Encoded Letter A
    MOVWF  CELL_25
    MOVLW  4	; Encoded Letter E
    MOVWF  CELL_26
    RETURN 

; HIDE_TABLE: Print a 6x2 table of boxes.
HIDE_TABLE MOVLW    b'00000000' ; Move the cursor to 0x00
    MOVWF  CURSOR_POS

    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX

    MOVLW  b'00010000' ; Move cursor to 0x40
    MOVWF  CURSOR_POS

    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX
    CALL  PRINT_BOX
    RETURN

; Prints the scoreboard for Mode 1, resets weakness as well.
MODE1_SCOREBOARD	MOVLW	b'00001000'
    MOVWF	CURSOR_POS
    CALL	PRINT_S
    CALL	PRINT_SEPARATOR
    CALL	PRINT_SEPARATOR
    CALL	PRINT_SEPARATOR
    CALL	PRINT_SEPARATOR
    CALL	PRINT_SEPARATOR
	CALL	PRINT_SEPARATOR
    CALL	PRINT_W
    CLRF	WEAKNESS

    BSF INTCON, 7
    RETURN

; Prints the scoreboard for Mode 3, resets SCORE_REG as well to its initial value (13).
MODE3_SCOREBOARD	MOVLW	b'00001000'
    MOVWF	CURSOR_POS
	MOVLW	LTR_MINUS
	CALL	WRITE_CHAR

	CALL	PRINT_0

	MOVLW	d'2'
	ADDWF	CURSOR_POS, 1

	MOVLW	LTR_PLUS
	CALL	WRITE_CHAR

	CALL	PRINT_0

	CLRF	NUM_ERRORS
	CLRF	NUM_CORRECT

	MOVLW	b'00011000'
	MOVWF	CURSOR_POS
	
	CALL	PRINT_S
	CALL	PRINT_C
	CALL	PRINT_O
	CALL	PRINT_R
	CALL	PRINT_E

	MOVLW	d'13'
	MOVWF	SCORE_REG
	CALL	PRINT_SCORE
	
    BSF INTCON, 7
    RETURN


; ****************************************************************	    LCD INTERFACING	    ***********************************************************************

; ET: Writes data to LCD.
ET	BSF	PORTB,1
	NOP
	BCF	PORTB,1
	MOVLW	d'4'
	MOVWF	COUNT1
LOOP5MS	CALL	DELAYMS
	DECFSZ	COUNT1,F
	GOTO	LOOP5MS
	RETURN
	
; CLEAR_SCREEN: Erases all characters on the screen and sets the cursor back to row 0, col 0
CLEAR_SCREEN	MOVLW	b'00000'    ; Send to the LCD "00 0000 0001", which is the instruction to clear the display.
	MOVWF	PORTA
	CALL ET
	
	MOVLW	b'00001'
	MOVWF	PORTA
	CALL ET
	
	RETURN

; HIDE_CURSOR: Prevents cursor from being displayed on the screen
HIDE_CURSOR	MOVLW	b'00000'		
	MOVWF	PORTA
	CALL	ET
	MOVLW	b'01100'
	MOVWF	PORTA
	CALL	ET


; DELETE_BEFORE: Deletes the character right before the cursor and decrements the cursor position
DELETE_BEFORE	DECF	CURSOR_POS
	MOVLW	LTR_SPACE        ; Print space in order to erase the char
	CALL WRITE_CHAR
	RETURN 


;   WRITE_CHAR:	Automatically writes a character at address specified in CURSOR_POS and increments CURSOR_POS in anticipation of the next character
;  Note: In order to save lines, the W register is initially assumed to contain the character code.
WRITE_CHAR	MOVWF	CHAR_CODE  ; Load the Character Code into CHAR_CODE register.
	BTFSC	CURSOR_POS,4	; If line (encoded as bit 4) is not 0, i.e. line is 1 then specify the first bit in the address as a 4.
	MOVLW	b'01100'
	BTFSS	CURSOR_POS,4		; If line (encoded as bit 4) is not 1, i.e. line is 0 then specify the first bit in the address as a 0.
	MOVLW	b'01000'
	MOVWF	PORTA
	CALL ET
	
	MOVF	CURSOR_POS,0
	ANDLW	h'0f'	    ; Mask the first 4 bits (which correspond to the column number)
	MOVWF	PORTA
	CALL ET
	
	;   Handling character code: A character code is an 8-bit number representing a printable character. To print it to the screen, we should mask the upper four
	;   bits, shift left 4 times, set the first bit to 1 and write to the LCD. Then, mask the lower four bits, set the first bit to 1 and write to the LCD.
	
	SWAPF	CHAR_CODE, 0 ; Move contents of CHAR_CODE to W, swapping the nibbles.
	ANDLW	h'0f'	     ; Mask the first 4 bits (which are now in the lower bits after swapping)
	ADDLW	h'10'	     ; Set the first bit of the W reg (adding 16)
	
	MOVWF	PORTA
	CALL ET
	
	MOVF	CHAR_CODE, 0 ; Move contents of CHAR_CODE register to W register
	ANDLW	h'0f'	     ; Mask the last 4 bits
	ADDLW	h'10'	     ; Set the first bit of the W reg (adding 16)
	
	MOVWF	PORTA
	CALL ET
	INCF	CURSOR_POS,1	; Increment the column number, equivalent to moving the cursor to await the next character.
	RETURN


; SHOW_CURSOR:	Forces the LCD to display a dashed line denoting the position of the cursor.
SHOW_CURSOR
	MOVLW	b'00000'
	MOVWF	PORTA
	CALL	ET
	MOVLW	b'00000'
	MOVWF	PORTA

	MOVLW	b'01110'
	MOVWF	PORTA
	CALL	ET
	MOVLW	b'00000'
	MOVWF	PORTA
	RETURN
	
; MOVE_CURSOR: Forces the cursor on the LCD to move without printing anything, CURSOR_POS must be set right before calling this function
MOVE_CURSOR	BTFSC	CURSOR_POS,4	; If line (encoded as bit 4) is not 0, i.e. line is 1 then specify the first bit in the address as a 4.
	MOVLW	b'01100'
	BTFSS	CURSOR_POS,4		; If line (encoded as bit 4) is not 1, i.e. line is 0 then specify the first bit in the address as a 0.
	MOVLW	b'01000'
	MOVWF	PORTA
	CALL ET
	
	MOVF	CURSOR_POS,0
	ANDLW	h'0f'	    ; Mask the first 4 bits (which correspond to the column number)
	MOVWF	PORTA
	CALL ET
	RETURN

; RESET_CURSOR: Resets the position of the cursor to row 0, col 0 both on the LCD and in CURSOR_POS
RESET_CURSOR	MOVLW	0
	MOVWF	CURSOR_POS
	CALL	MOVE_CURSOR
	CALL	SHOW_CURSOR
	RETURN
	
; One-liners to print specific characters.
; Given that each function consists of 3 lines, and printing one character without a function takes at least 2 (Moving it to W and calling WRITE_CHAR), it is more
; Efficient to create a function for every letter that needs to be printed at least twice. This significantly reduces the amount of lines needed.
PRINT_A	MOVLW	LTR_A
	CALL WRITE_CHAR
	RETURN

PRINT_C	MOVLW	LTR_C
	CALL WRITE_CHAR
	RETURN
	
PRINT_D	MOVLW	LTR_D
	CALL WRITE_CHAR
	RETURN
	
PRINT_E MOVLW	LTR_E
	CALL WRITE_CHAR
	RETURN

PRINT_G	MOVLW	LTR_G
	CALL WRITE_CHAR
	RETURN
	
PRINT_K	MOVLW	LTR_K
	CALL WRITE_CHAR
	RETURN
	
PRINT_M	MOVLW	LTR_M
	CALL WRITE_CHAR
	RETURN

PRINT_O	MOVLW	LTR_O
	CALL WRITE_CHAR
	RETURN

PRINT_P	MOVLW	LTR_P
	CALL WRITE_CHAR
	RETURN

PRINT_R	MOVLW	LTR_R
	CALL WRITE_CHAR
	RETURN

PRINT_T	MOVLW	LTR_T
	CALL WRITE_CHAR
	RETURN

PRINT_S	MOVLW	LTR_S
	CALL WRITE_CHAR
	RETURN

PRINT_U	MOVLW	LTR_U
	CALL WRITE_CHAR
	RETURN
	
PRINT_V	MOVLW	LTR_V
	CALL WRITE_CHAR
	RETURN
	
PRINT_W	MOVLW	LTR_W
	CALL WRITE_CHAR
	RETURN

PRINT_Y	MOVLW	LTR_Y
	CALL WRITE_CHAR
	RETURN
	
PRINT_ASTERISK	MOVLW	LTR_ASTERISK
	CALL WRITE_CHAR
	RETURN
                                         
PRINT_SPACE	MOVLW	LTR_SPACE        ; print space in order to erase the char
	CALL WRITE_CHAR
	RETURN
	
PRINT_BOX MOVLW	LTR_BOX
    CALL  WRITE_CHAR
    RETURN
    
PRINT_SHADE MOVLW	LTR_SHADE
    CALL  WRITE_CHAR
    RETURN

PRINT_SEPARATOR MOVLW	LTR_SEPARATOR	; A separator is the small inverted 'L-shape' in Mode 1
    CALL  WRITE_CHAR
    RETURN

PRINT_0	MOVLW	LTR_0
	CALL WRITE_CHAR
	RETURN	

PRINT_1	MOVLW	LTR_1
	CALL WRITE_CHAR
	RETURN

PRINT_2	MOVLW	LTR_2
	CALL WRITE_CHAR
	RETURN
	
PRINT_3	MOVLW	LTR_3
	CALL WRITE_CHAR
	RETURN

; PRINT_DOUBLE_DIGITS: This is a function that prints a binary number as a potentially 2-digit number on the LCD.
; Note that the numbers will never exceed 19. i.e. the tens digit, if it exists, is always 1
; The number to print is expected to be in the W register.
PRINT_DOUBLE_DIGITS	
	MOVWF	TMP
	MOVLW	d'10'
	SUBWF	TMP, 0
	; SUBWF adds the 2s complement of the content of WREG to the contents of SCORE_REG
	; If the result generates a carry, then the MSB is 0, i.e. result is positive
	; If the result doesn't generate a carry, then MSB is 1, i.e. result is negative
	BTFSC	STATUS, 0	
	GOTO	DOUBLE_DIGITS
	MOVLW	LTR_0
	ADDWF	TMP, 0	; If there is only one digit, load the score directly, add offset of 1, and print to screen
	CALL	WRITE_CHAR
	MOVLW	LTR_SPACE        ; Clear the ones character
	CALL WRITE_CHAR
	RETURN

DOUBLE_DIGITS	CALL	PRINT_1	; If there are two digits, print 1 followed by the number offsetted from 0 by (score-10)
	MOVLW	d'10'
	SUBWF	TMP, 1
	MOVLW	LTR_0
	ADDWF	TMP, 0
	CALL	WRITE_CHAR
	RETURN


; PRINT_SCORE: Prints the score as a double digit to the screen, this assumes that the score is in SCORE_REG.
PRINT_SCORE	MOVLW	b'00011110'
	MOVWF	CURSOR_POS
	MOVF	SCORE_REG, 0
	CALL	PRINT_DOUBLE_DIGITS
	RETURN
	
; ***********************************************************************   INTERRUPTS	    **********************************************************************

; Handle the Button related interrupts, and check which button is pressed within the interrupt.
RB4_INT	CALL	DELAYFMS ;  Debounce
	BTFSS	PORTB,LEFT_BTN
	CALL	HANDLE_LEFT
	
	BTFSS	PORTB,RIGHT_BTN
	CALL	HANDLE_RIGHT
	
	BTFSS	PORTB,UP_DOWN_BTN
	CALL	HANDLE_UP_DOWN
	
	BTFSS	PORTB,CONFIRM_BTN
	CALL	HANDLE_CONFIRM
	
	BCF INTCON, 0  ; Make sure to reset the flag.
	RETFIE
	

TMR0_INT	BCF	INTCON, 2 ; Clear the TMR0 interrupt flag			
	DECFSZ	COUNT10,1
	RETFIE
	MOVLW	d'152'
	MOVWF	COUNT10 ; Set the counter back to its original value
	DECF	NUMBER_OF_TENS_OF_SECONDS,1
	MOVFW	CURSOR_POS
	MOVWF	TMP  ;TMP stores the initial position of the cursor 
	MOVLW	d'13'
	MOVWF	CURSOR_POS
	MOVLW	b'00110000'	;bias of zero
	ADDWF	NUMBER_OF_TENS_OF_SECONDS,0	
	CALL	WRITE_CHAR
	MOVFW	TMP
	MOVWF	CURSOR_POS	;Return the cursor to its position before the interrupt
	CALL	MOVE_CURSOR
	CALL	CHECK_IF_TIMER_IS_ZERO
	RETFIE	
	

CHECK_IF_TIMER_IS_ZERO	MOVFW	NUMBER_OF_TENS_OF_SECONDS	
	BTFSS	STATUS, 2
	RETURN
	BSF	FLAG_REG,4	; Indicates that the time has finished
	BCF	INTCON,7 ; Make sure to reset the flag.
	RETURN
		

; **************************************************************************************        BUTTON HANDLERS       ****************************************************************************

HANDLE_LEFT	MOVLW	b'00000000'
	XORWF	FLAG_REG, 0	; Check if you are in the Main Menu (i.e. Check if the last 2 bits of the FLAG_REG are 0s)
	BTFSC	STATUS, 2
	RETURN;GOTO	MOVE_AST_LEFT
	GOTO	IN_GAME_LEFT

IN_GAME_LEFT	MOVLW	b'00001111'	; Check if you are at column 0, if you are then buzz
	ANDWF	CURSOR_POS, 0	; Mask only the last 4 bits (corresponding to col number)
	MOVWF	TMP			; Store it in a tmp register
	MOVLW	0			; Load the number zero in W
	XORWF	TMP, 0		; Check if col number == 0
	BTFSC	STATUS, 2	; If it's true, Buzz, otherwise, decrement the cursor position
	GOTO	BUZZ
	DECF	CURSOR_POS
	CALL	MOVE_CURSOR
	CALL	SHOW_CURSOR
	RETURN

HANDLE_RIGHT	MOVLW	b'00000000'
	XORWF	FLAG_REG, 0	; Check if you are in the Main Menu (i.e. Check if the last 2 bits of the FLAG_REG are 0s)
	BTFSC	STATUS, 2
	GOTO	MOVE_AST_RIGHT
	GOTO	IN_GAME_RIGHT

IN_GAME_RIGHT	MOVLW	b'00001111'	; Check if you are at column 7, if you are then buzz
	ANDWF	CURSOR_POS, 0	; Mask only the last 4 bits (corresponding to col number)
	MOVWF	TMP
	MOVLW	d'5'
	XORWF	TMP, 0
	BTFSC	STATUS, 2
	GOTO	BUZZ
	INCF	CURSOR_POS
	CALL	MOVE_CURSOR
	CALL	SHOW_CURSOR
	RETURN


HANDLE_CONFIRM	; Unless you're in the Main Menu, toggle the value of the CONFIRM_STATUS flag (Bit 2 on FLAG_REG) and reveal the character underneath, otherwise go into desired mode.
	MOVLW	b'00000000'
	XORWF	FLAG_REG, 0	; Check if you are in the Main Menu (i.e. Check if the last 2 bits of the FLAG_REG are 0s)
	BTFSC	STATUS, 2
	GOTO	MAIN_MENU_CONFIRMATION
	
	MOVLW	d'42'	; Load Bias
	ADDWF	CURSOR_POS, 0	; Add Bias
	MOVWF	FSR	;   Move the register number into FSR
	MOVF	INDF, 0	; Obtain Contents (which should be an encoded CHAR_CODE + flag)
	
	MOVWF	TMP	; TMP has contents of the selected cell
	BTFSC	TMP, 7 ; Last bit is used as flag to determine if the card is open.
	GOTO	BUZZ
	BSF	INDF, 7
	
; Reveal the card
	BTFSS	FLAG_REG, 2 ; Load it into either CARD1 or CARD2 depending on if it's the first or second card opened
	MOVWF	CARD1
	BTFSC	FLAG_REG, 2
	MOVWF	CARD2
	MOVWF	TMP		; Store char_code in temporary reg
	MOVLW	b'00000111'
	ANDWF	TMP, 1
	MOVLW	b'01000001'		; Add bias equal to 'A'.
	ADDWF	TMP, 0
	CALL	WRITE_CHAR  ;	Write it to the character to the LCD.
	DECF	CURSOR_POS
	MOVF	CURSOR_POS, 0
	BTFSS	FLAG_REG, 2 ; Load it into either CARD1 or CARD2 depending on if it's the first or second card opened
	MOVWF	CELL_ADDRESS

	BSF	INDF, 6	; Bit 6 is to be used to determine if the card has ever been opened before.
				; Set this flag just after revealing the card.

	BTFSC	FLAG_REG, 2	; If CONFIRM_STATUS is 0, i.e. the user hasn't yet selected a card, reveal the first card
	CALL	CHECK_MODE
	MOVLW	b'00000100'	; Toggle the 3rd bit of the CONFIRM_STATUS register
	XORWF	FLAG_REG, 1
	CALL	RESET_CURSOR
	RETURN

MAIN_MENU_CONFIRMATION	MOVLW	b'00000111'; Check if the Mode selected is Mode 1(i.e. cursor is at pos 6)
	XORWF	CURSOR_POS, 0
	BTFSS	STATUS, 2
	GOTO	SKIP_MODE1_CONFIRMATION
	BSF		FLAG_REG, 0
	RETURN
SKIP_MODE1_CONFIRMATION	MOVLW	b'00001010'; Check if the Mode selected is Mode 2(i.e. cursor is at pos 9)
	XORWF	CURSOR_POS, 0
	BTFSS	STATUS, 2
	GOTO	SKIP_MODE2_CONFIRMATION
	BSF		FLAG_REG, 1
	RETURN
SKIP_MODE2_CONFIRMATION	BSF	FLAG_REG, 0	; Otherwise, Mode is 3 select so set the first two bits of the FLAG_REG
	BSF		FLAG_REG, 1
	RETURN


HANDLE_UP_DOWN	; Unless you're in the Main Menu, toggle the row that the cursor is at
	MOVLW	b'00000000'
	XORWF	FLAG_REG, 0	; Check if you are in the Main Menu (i.e. Check if the last 2 bits of the FLAG_REG are 0s)
	BTFSC	STATUS, 2
	RETURN
	MOVLW	b'00010000'
	XORWF	CURSOR_POS, 1
	CALL	MOVE_CURSOR
	CALL	SHOW_CURSOR
SKIP_HANDLE_UP_DOWN  RETURN	

; ***********************************************************************	GAME MECHANICS	     **********************************************************************
CHECK_MODE 
	MOVLW	b'00000011' ; Mask the first two bits
	ANDWF	FLAG_REG, 0
	MOVWF	TMP	; Store in a temporary register
	MOVLW   1		; Load the number one in W
	XORWF   TMP, 0	; Check if the user selected Mode 1
	BTFSC   STATUS, 2
	GOTO    CHECK_MATCH_MODE1
	MOVLW   2		; Load the number two in W
	XORWF   TMP, 0	; Check if the user selected Mode 2
	BTFSC   STATUS, 2
	GOTO    CHECK_MATCH_MODE2
	MOVLW   3		; Load the number three in W
	XORWF   TMP, 0	; Check if the user selected Mode 3
	BTFSC   STATUS, 2
	GOTO    CHECK_MATCH_MODE3
	RETURN
	
CHECK_MATCH_MODE1	; Compare CARD1 and CARD2, if they match, then perform the action of the corresponding game
	CALL	CHECK_MATCH
	BTFSC	STATUS, 2
	GOTO	MATCH_MODE1
	GOTO	NO_MATCH_MODE1

CHECK_MATCH_MODE2
	CALL	CHECK_MATCH
	BTFSC	STATUS, 2
	GOTO	MATCH_MODE2
	GOTO	NO_MATCH_MODE2

CHECK_MATCH_MODE3
	CALL	CHECK_MATCH
	BTFSC	STATUS, 2
	GOTO	MATCH_MODE3
	GOTO	NO_MATCH_MODE3


; Checks if CARD1 and CARD2 match, bit 2 on STATUS should be used as the condition
CHECK_MATCH		MOVF	CARD2,	0
	XORWF	CARD1, 0
	MOVWF	TMP
	MOVLW	b'00000111'		; When checking for a match, only the last 3 bits are relevant
							; the other bits are flags which are irrelevant here.
	ANDWF	TMP, 0
	RETURN

; If the cards match in Mode 1, then create a buzz sound and decrement the remaining number of unmatched cards. If there are none left, then the game is over so set the GAME_OVER flag.
MATCH_MODE1		CALL	BUZZ
	DECFSZ	NUM_LEFT, 1
	RETURN
	BSF FLAG_REG, 3
	RETURN

; If the cards don't match in Mode 1, then increment the weakness and hide the cards.
NO_MATCH_MODE1
	BSF	PORTB, 2
	CALL	DELAYS
	
	CALL	HIDE_OPEN_CARDS
	
	BCF	PORTB, 2
	;   Check if weakness is 10, if it is, don't bother incrementing, as maximum weakness has been reached
	MOVLW	d'11'
	XORWF	WEAKNESS, 0
	BTFSC	STATUS, 2
	RETURN
	
	
	; Check if the weakness has reached 8 (about to be 9 after increment), i.e. the player should be shown a message "WEAK", and set the flag accordingly.
	MOVLW	d'8'
	XORWF	WEAKNESS, 0
	BTFSC	STATUS, 2
	BSF FLAG_REG, 5
	
	
	; Check if the weakness has reached 4 (about to be 5 after increment), i.e. the player should be shown a message "AVG", and set the flag accordingly.
	MOVLW	d'4'
	XORWF	WEAKNESS, 0
	BTFSC	STATUS, 2
	BSF FLAG_REG, 4
	
	INCF	WEAKNESS    ; Increment the weakness (or number of misses)
	BTFSS	WEAKNESS, 0 ; We will update the scoreboard once every two times, for convenience, update every time the LSB is 0
	RETURN
	

	MOVLW	b'00001000' 
	MOVWF	CURSOR_POS  ; Set the cursor position initially to 8, we will add some offset to this depending on our weakness level
	MOVLW	1   ; We will calculate the offset based on the following algorithm: Add 1 to the weakness and store the result in a temporary variable. Shift the temporary variable to get the offset.
	ADDWF	WEAKNESS, 0
	MOVWF	TMP
	RRF TMP, 0
	ADDWF	CURSOR_POS, 1
	CALL	PRINT_SHADE
	CALL	RESET_CURSOR
	RETURN
	
	
MATCH_MODE2
	CALL	BUZZ
	INCF	NUM_CORRECT,1
	DECFSZ	NUM_LEFT, 1
	RETURN
	BSF FLAG_REG, 3
	RETURN
NO_MATCH_MODE2
	BSF	PORTB, 2
	CALL	DELAYS
		
	CALL	HIDE_OPEN_CARDS

	BCF	PORTB, 2
	DECF	CURSOR_POS,1
	RETURN
	
MATCH_MODE3	
	INCF	NUM_CORRECT
	INCF	SCORE_REG
	BTFSC	STATUS, 2
	BSF	FLAG_REG, 3
	
	MOVLW	b'00001101'	; Move cursor to the position right after the plus sign
	MOVWF	CURSOR_POS
	MOVF	NUM_CORRECT, 0
	CALL	PRINT_DOUBLE_DIGITS		; Print the number of correct guesses as a potentially double digit number

	MOVLW	b'00011110'			; Print the updated score
	MOVWF	CURSOR_POS
	MOVF	SCORE_REG, 0
	CALL	PRINT_DOUBLE_DIGITS


	MOVLW	d'6'
	XORWF	NUM_CORRECT, 0
	BTFSC	STATUS, 2	; End the game if there are 6 successes.
	GOTO	MODE3_SUCCESS	; End the game, performing necessary actions for a win
	RETURN

NO_MATCH_MODE3
	CALL	DELAYS
	BTFSC	CARD1, 6
	CALL	PENALTY

	BTFSC	CARD2, 6
	CALL	PENALTY

	CALL	HIDE_OPEN_CARDS	

	MOVLW	b'00001001'	; Move cursor to the position right after the negative sign
	MOVWF	CURSOR_POS
	MOVF	NUM_ERRORS, 0
	CALL	PRINT_DOUBLE_DIGITS		; Print the number of errors as a potentially double digit number

	MOVLW	b'00011110'		; Print the updated score
	MOVWF	CURSOR_POS
	MOVF	SCORE_REG, 0
	CALL	PRINT_DOUBLE_DIGITS

	MOVLW	d'13'
	SUBWF	NUM_ERRORS, 0
	BTFSC	STATUS, 0	; Check if NUM_ERRORS > 12
	GOTO	MODE3_FAIL	; End the game, performing necessary actions for a loss

	RETURN

HIDE_OPEN_CARDS		MOVLW	d'42'	; Load Bias
	ADDWF	CURSOR_POS, 0	; Add Bias
	MOVWF	FSR
	BCF	INDF, 7
	
	MOVLW	LTR_BOX
    CALL  WRITE_CHAR

	MOVF	CELL_ADDRESS, 0
	MOVWF	CURSOR_POS
	MOVLW	d'42'	; Load Bias
	ADDWF	CURSOR_POS, 0	; Add Bias
	MOVWF	FSR
	BCF	INDF, 7
	
	MOVLW	LTR_BOX
    CALL  WRITE_CHAR

	RETURN

; Decrement the SCORE_REG if the card has been opened before
PENALTY	INCF	NUM_ERRORS
	MOVLW	0
	XORWF	SCORE_REG
	BTFSS	STATUS, 2
	DECF	SCORE_REG
	RETURN

; If the user has finished Mode 3 successfully, then show the LED pattern and end the game. (Set the GAME_OVER flag)
MODE3_SUCCESS	MOVLW	d'3'
	MOVWF	COUNT1
	CALL	PATTERN_LOOP
	BSF		FLAG_REG, 3	; If this is the case, then game is over.
	RETURN

; If the user has finished Mode 3 unsuccessfully, then make the buzzer sound and end the game. (Set the GAME_OVER flag)
MODE3_FAIL	
	CALL	BUZZ
	CALL	DELAY3S
	BSF		FLAG_REG, 3	; If this is the case, then game is over.
	RETURN

; ***********************************************************************	DELAYS	    **********************************************************************

; DELAYS: Causes a 1s delay, uses registers COUNT4, COUNT5 & COUNT6.
	
DELAYS	MOVLW	H'00'
	MOVWF	COUNT6
	MOVWF	COUNT5
	MOVLW	0x5
	MOVWF	COUNT4
	
LOOP	INCFSZ	COUNT6,F
	GOTO	LOOP
	INCFSZ	COUNT5,F
	GOTO	LOOP
	DECFSZ	COUNT4,F
	GOTO	LOOP
	RETURN
	
; DELAYS: Causes a 3s delay, uses registers COUNT7, COUNT8 & COUNT9.
    
DELAY3S	MOVLW	H'00'
	MOVWF	COUNT9
	MOVWF	COUNT8
	MOVLW	H'10'
	MOVWF	COUNT7
	
LOOP3S	INCFSZ	COUNT9,F
	GOTO	LOOP3S
	INCFSZ	COUNT8,F
	GOTO	LOOP3S
	DECFSZ	COUNT7,F
	GOTO	LOOP3S
	
	RETURN

; DELAYFMS: Causes a 40 ms delay, uses register COUNT1.
	
DELAYFMS	MOVLW	d'37'
	MOVWF	COUNT1
LOOP3	CALL	DELAYMS
	DECFSZ	COUNT1,F
	GOTO	LOOP3
	RETURN

; DELAYMS: Causes a 1 ms delay, uses registers COUNT2 & COUNT 3.
	
DELAYMS	MOVLW d'0'	    
	MOVWF	COUNT2
	MOVLW	d'76'
	MOVWF	COUNT3
LOOP1	INCFSZ	COUNT2,F
	GOTO	LOOP1
LOOP2	NOP	
	DECFSZ	COUNT3,F
	GOTO LOOP2
	RETURN

; ***********************************************************************	MISC	    **********************************************************************
; Make a buzzer sound for 1 second.
BUZZ	BSF	PORTB, 0
	CALL	DELAYS
	BCF	PORTB, 0
	RETURN

; Make the LEDs light in a certain pattern for a certain number of iterations.
; Note: the iterations must be specified in COUNT1 before calling PATTERN_LOOP.
PATTERN_LOOP
	BSF	PORTB, 3
	BCF	PORTB, 2
	CALL	DELAYS
	BSF	PORTB, 2
	BCF	PORTB, 3
	CALL	DELAYS
	DECFSZ	COUNT1
	GOTO	PATTERN_LOOP
	RETURN

; Move the position of the asterisk in the Main Menu.
; Possible cursor positions: 0110 1001 1100	
MOVE_AST_RIGHT CALL DELETE_BEFORE 
	BTFSS CURSOR_POS,3  ; SKIP IF BIT3=1
	GOTO R1 ; BIT3 is 0
	BTFSS CURSOR_POS,2
	GOTO R1 ; BIT2 is 0

	MOVLW b'00000110' ;  ROTATE
	MOVWF CURSOR_POS 
	GOTO EXIT_RIGHT

R1	MOVLW d'2'
	ADDWF CURSOR_POS,1

EXIT_RIGHT	CALL PRINT_ASTERISK 
	RETURN

	END
