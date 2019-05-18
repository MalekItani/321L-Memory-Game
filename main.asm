    title	"LCD"
    list	p=16f84A
    radix	hex
    include "p16f84a.inc"

COUNT1	EQU	d'12'
COUNT2	EQU	d'13'
COUNT3	EQU	d'14'
COUNT4	EQU	d'15'
COUNT5	EQU	d'16'
COUNT6	EQU	d'17'
COUNT7	EQU	d'22'
COUNT8	EQU	d'23'
COUNT9	EQU	d'24'

CURSOR_POS  EQU	d'18' ; Register stores detail about the cursor's current position. Bits[3:0] specify column number, where as bit 4 specifies the row.
		      ; Since there are 16 columns and 2 rows, such an encoding is guaranteed to span all cells on the LCD.
CHAR_CODE   EQU	d'19'

FLAG_REG    EQU d'20' ; Register containing some flags to be used within the program. Structure: [-,-,-,-,-,CONFIRM_STATUS,MODE_BIT2,MODE_BIT1] within the program. Structure: [-,-,-,-,-,-,-,Main Menu Flag]

TMP	EQU d'21'  
LEFT_BTN    EQU	d'4'
RIGHT_BTN   EQU	d'5'
UP_DOWN_BTN EQU	d'6'
CONFIRM_BTN EQU	d'7'

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

CELL_11 EQU d'52'
CELL_12 EQU d'53'
CELL_13 EQU d'54'
CELL_14 EQU d'55'
CELL_15 EQU d'56'
CELL_16 EQU d'57'

CELL_21 EQU d'58'
CELL_22 EQU d'59'
CELL_23 EQU d'60'
CELL_24 EQU d'61'
CELL_25 EQU d'62'
CELL_26 EQU d'63'   

	ORG	0x0
	GOTO	MAIN
	ORG	0x04
	BTFSC	INTCON, d'0'
	GOTO RB4_INT

	
; Register Configurations
MAIN	CLRF	PORTA
	BSF	STATUS,RP0
	MOVLW	b'11110000' ; Set   RB[7:4] as inputs, the rest are outputs
	MOVWF	TRISB
	
	CLRF	TRISA	    ; Set all pins of PORTA as outputs
	BCF	STATUS,RP0
	

INDIRECT	MOVLW	d'11'
	MOVWF	COUNT2
	MOVLW	H'20'
	MOVWF	FSR
LOOPX	DECFSZ	COUNT2,F
	GOTO LOOPY
	GOTO FINISH
LOOPY	MOVF	COUNT2,W
	MOVWF	INDF
	INCF	FSR
	GOTO	LOOPX
FINISH


; Initialize the LCD
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

			
WELCOME_SCREEN MOVLW b'00000010' ; Move the cursor to 0x04
	MOVWF CURSOR_POS
	
	CALL PRINT_M
	CALL PRINT_E
	CALL PRINT_M
	CALL PRINT_O
	CALL PRINT_R
	CALL PRINT_Y
	
	MOVLW b'00001010' ; Move the cursor to 0x0C (Column 10)
	MOVWF CURSOR_POS
	
	CALL PRINT_G
	CALL PRINT_A
	CALL PRINT_M
	CALL PRINT_E
	
	MOVLW b'00000000' ; "move the cursor to 0x00"
	MOVWF CURSOR_POS
	
	CALL DELAYS ; TODO: Implement some longer delay (4 or  seconds maybe?)

MAIN_MENU   CALL    CLEAR_SCREEN
	CLRF	FLAG_REG
	MOVLW	b'10001000' ; Properly setup the interrupts. Only GIE and RBIE should be set.
	MOVWF	INTCON
	CALL PRINT_M
	CALL PRINT_O
	CALL PRINT_D
	CALL PRINT_E
	
	MOVLW d'2'   ; Move the cursor two units to the right
	ADDWF CURSOR_POS, 1
	
	CALL PRINT_ASTERISK
	CALL PRINT_1
	
	MOVLW d'2'   ; Move the cursor two units to the right
	ADDWF CURSOR_POS, 1
	
	CALL PRINT_2
	
	MOVLW d'2'   ; Move the cursor two units to the right
	ADDWF CURSOR_POS, 1
	
	CALL PRINT_3
	MOVLW	b'00000111'
	MOVWF	CURSOR_POS

INF BTFSC   FLAG_REG,1
    GOTO  MODE2_OR_3
    GOTO  MODE1_OR_INF

MODE2_OR_3 BTFSC    FLAG_REG,0
    GOTO  MODE3
    GOTO  MODE2

MODE1_OR_INF BTFSC  FLAG_REG,0
    GOTO  MODE1
    GOTO  INF


MODE1 CALL  SETUP
	CALL	MODE1_SCOREBOARD
LOOP_M1 GOTO	LOOP_M1


MODE2 CALL  SETUP
CALL	MODE1_SCOREBOARD
LOOP_M2	GOTO	LOOP_M2


MODE3 CALL  SETUP
CALL	MODE1_SCOREBOARD
LOOP_M3 GOTO    LOOP_M3

SETUP
	BCF	INTCON, 7
	
	CALL	CLEAR_SCREEN	
	CALL 	POPULATE_TABLE
    CALL	HIDE_TABLE

	MOVLW	b'00000000'
	MOVWF	CURSOR_POS
	CALL	MOVE_CURSOR
	CALL	SHOW_CURSOR
	BSF INTCON, 7
	
	RETURN
	
POPULATE_TABLE MOVLW	LTR_C
    MOVWF  CELL_11
    MOVLW  LTR_A
    MOVWF  CELL_12
    MOVLW  LTR_E
    MOVWF  CELL_13
    MOVLW  LTR_F
    MOVWF  CELL_14
    MOVLW  LTR_B
    MOVWF  CELL_15
    MOVLW  LTR_D
    MOVWF  CELL_16
    MOVLW  LTR_B
    MOVWF  CELL_21
    MOVLW  LTR_D
    MOVWF  CELL_22
    MOVLW  LTR_F
    MOVWF  CELL_23
    MOVLW  LTR_C
    MOVWF  CELL_24
    MOVLW  LTR_A
    MOVWF  CELL_25
    MOVLW  LTR_E
    MOVWF  CELL_26
    RETURN 


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
	
MODE1_SCOREBOARD	MOVLW	b'00001000'
	MOVWF	CURSOR_POS
	CALL	PRINT_S
	CALL	PRINT_SEPARATOR
	CALL	PRINT_SEPARATOR
	CALL	PRINT_SEPARATOR
	CALL	PRINT_SEPARATOR
	CALL	PRINT_SEPARATOR
	CALL	PRINT_W
	RETURN



; ****************************************************************	    LCD INTERFACING	    ***********************************************************************

; CLEAR_SCREEN: Erases all characters on the screen and sets the cursor back to row 0, col 0
CLEAR_SCREEN	MOVLW	b'00000'    ; Send to the LCD "00 0000 0001", which is the instruction to clear the display.
	MOVWF	PORTA
	CALL ET
	
	MOVLW	b'00001'
	MOVWF	PORTA
	CALL ET
	
	RETURN
	
; ET: Writes data to LCD.
ET	BSF	PORTB,1
	NOP
	BCF	PORTB,1
	CALL DELAYFMS
	RETURN 

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

;   WRITE_CHAR:	Automatically writes a character at address specified in CURSOR_POS and increments CURSOR_POS in anticipation of the next character


WRITE_CHAR	MOVWF	CHAR_CODE   ; In order to save lines, the W register is initially assumed to contain the character code. Load it into CHAR_CODE register.
	CALL	MOVE_CURSOR
	
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

; One-liners to print specific characters (feel free to abuse)
	
PRINT_A	MOVLW	LTR_A
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
	
PRINT_M	MOVLW	LTR_M
	CALL WRITE_CHAR
	RETURN

PRINT_O	MOVLW	LTR_O
	CALL WRITE_CHAR
	RETURN

PRINT_R	MOVLW	LTR_R
	CALL WRITE_CHAR
	RETURN

PRINT_S	MOVLW	LTR_S
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
                                         
PRINT_SPACE	MOVLW	LTR_SPACE        ; "print space in order to erase the char"
	CALL WRITE_CHAR
	RETURN
	
PRINT_BOX MOVLW	LTR_BOX
    CALL  WRITE_CHAR
    RETURN

PRINT_SEPARATOR MOVLW	LTR_SEPARATOR
    CALL  WRITE_CHAR
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
	
; ***********************************************************************   INTERRUPTS	    **********************************************************************
	
; NOTE: Should all of these be BTFSS?
	
RB4_INT	CALL	DELAYFMS ;  Debounce
	BTFSS	PORTB,LEFT_BTN
	CALL	HANDLE_LEFT
	
	BTFSS	PORTB,RIGHT_BTN
	CALL	HANDLE_RIGHT
	
	BTFSS	PORTB,UP_DOWN_BTN
	CALL	HANDLE_UP_DOWN
	
	BTFSS	PORTB,CONFIRM_BTN
	CALL	HANDLE_CONFIRM
	
	BCF INTCON, 0
	RETFIE

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
	MOVLW	b'00010000'
	RETURN	; TODO: Implement Card opening functionality


MAIN_MENU_CONFIRMATION	MOVLW	b'00000111'; Check if the Mode selected is Mode 1(i.e. cursor is at pos 6)
	XORWF	CURSOR_POS, 0
	BTFSC	STATUS, 2
	GOTO	SKIP_MODE1_CONFIRMATION
	BSF		FLAG_REG, 0
	RETURN
SKIP_MODE1_CONFIRMATION	MOVLW	b'00001010'; Check if the Mode selected is Mode 2(i.e. cursor is at pos 9)
	XORWF	CURSOR_POS, 0
	BTFSC	STATUS, 2
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

; ****************************************************     MISC      **************************************************;

BUZZ	BSF	PORTB, 0
	CALL	DELAYS
	BCF	PORTB, 0
	RETURN

DELETE_BEFORE	DECF	CURSOR_POS
	CALL	PRINT_SPACE ; "THE CURSOR NOW IS AFTER THE DELETING POSITION"
	RETURN

; "POSSIBLE CURSOR POSITIONS: 0110 1001 1100"	
MOVE_AST_RIGHT CALL DELETE_BEFORE 
	BTFSS CURSOR_POS,3  ;"SKIP IF BIT3=1"
	GOTO R1 ;"BIT3 is 0"
	BTFSS CURSOR_POS,2
	GOTO R1 ;"BIT2 is 0"

	MOVLW b'00000110' ;"ROTATE"
	MOVWF CURSOR_POS 
	GOTO EXIT_RIGHT

R1	MOVLW b'0000010'
	ADDWF CURSOR_POS,1;
	GOTO EXIT_RIGHT

EXIT_RIGHT	CALL PRINT_ASTERISK 
	RETURN

; "POSSIBLE CURSOR POSITIONS: 0110 1001 1100"
MOVE_AST_LEFT CALL DELETE_BEFORE
	BTFSC CURSOR_POS,3  ;"SKIP IF BIT3=1"
	GOTO L1 ;"BIT3 IS 1"

	MOVLW b'00001100' ;"ROTATE SINCE THE CURSOR IS IN THE LEFT MOST POSITION"
	MOVWF CURSOR_POS 
	GOTO EXIT_LEFT

L1	MOVLW b'0000100'
	SUBWF CURSOR_POS,1;
	GOTO EXIT_LEFT

EXIT_LEFT	CALL PRINT_ASTERISK 
	RETURN

	END
