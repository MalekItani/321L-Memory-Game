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

CURSOR_POS  EQU	d'18' ; Register stores detail about the cursor's current position. Bits[3:0] specify column number, where as bit 4 specifies the row.
		      ; Since there are 16 columns and 2 rows, such an encoding is guaranteed to span all cells on the LCD.
CHAR_CODE   EQU	d'19'

FLAG_REG    EQU d'20' ; Register containing some flags to be used within the program. Structure: [-,-,-,-,-,-,-,Main Menu Flag]
   
LEFT_BTN    EQU	d'4'
RIGHT_BTN   EQU	d'5'
UP_DOWN_BTN EQU	d'6'
CONFIRM_BTN EQU	d'7'
 
LTR_A	EQU b'01000001'
LTR_B	EQU b'01000010'	
LTR_C	EQU b'01000011' 
LTR_D	EQU b'01000100'
LTR_E	EQU b'01000101'
LTR_F	EQU b'01000110'
LTR_G	EQU b'01000111'	
LTR_H	EQU b'01001000'
LTR_I	EQU b'01001001'	
LTR_J	EQU b'01001010'	
LTR_K	EQU b'01001011'
LTR_L	EQU b'01001100'
LTR_M	EQU b'01001101'	
LTR_N	EQU b'01001110'
LTR_O	EQU b'01001111'	
LTR_P	EQU b'01010000'	
LTR_Q	EQU b'01010001'	
LTR_R	EQU b'01010010'
LTR_S	EQU b'01010011'		
LTR_T	EQU b'01010100'		
LTR_U	EQU b'01010101'		
LTR_V	EQU b'01010110'		
LTR_W	EQU b'01010111'	
LTR_X	EQU b'01011000'		
LTR_Y	EQU b'01011001'
LTR_Z	EQU b'01011010'
LTR_0	EQU b'00110000'
LTR_1	EQU b'00110000'
LTR_2	EQU b'00110000'
LTR_3	EQU b'00110000'
LTR_4	EQU b'00110000'
LTR_5	EQU b'00110000'
LTR_6	EQU b'00110000'
LTR_7	EQU b'00110000'
LTR_8	EQU b'00110000'
LTR_9	EQU b'00110000'
LTR_ASTERISK	EQU b'00101010'
LTR_SPACE   EQU	b'10100000'
	
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
	
	MOVLW	b'10001000' ; Properly setup the interrupts. Only GIE and RBIF should be set.
	MOVWF	INTCON
	

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

			
WELCOME_SCREEN	MOVLW	b'00000100' ; Move the cursor to 0x04
	MOVWF	CURSOR_POS

	CALL	PRINT_M
	CALL	PRINT_E
	CALL	PRINT_M
	CALL	PRINT_O
	CALL	PRINT_R
	CALL	PRINT_Y
	
	MOVLW	b'00001010' ; Move the cursor to 0x0A (Column 10)
	MOVWF	CURSOR_POS
	
	CALL	PRINT_G
	CALL	PRINT_A
	CALL	PRINT_M
	CALL	PRINT_E
	
	CALL	DELAYS	; TODO: Implement some longer delay (4/5 seconds maybe?)

MAIN_MENU   CALL    CLEAR_SCREEN
	CALL	PRINT_M
	CALL	PRINT_O
	CALL	PRINT_D
	CALL	PRINT_E
	
	MOVLW	d'2'   ; Move the cursor two units to the right
	ADDWF	CURSOR_POS, 1
	
	CALL	PRINT_ASTERISK
	CALL	PRINT_1
	
	MOVLW	d'2'   ; Move the cursor two units to the right
	ADDWF	CURSOR_POS, 1
	
	CALL	PRINT_2
	
	MOVLW	d'2'   ; Move the cursor two units to the right
	ADDWF	CURSOR_POS, 1
	
	CALL	PRINT_3
	

INF	GOTO	INF
	
	
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

	
;   WRITE_CHAR:	Automatically writes a character at address specified in CURSOR_POS and increments CURSOR_POS in anticipation of the next character
	
WRITE_CHAR	MOVWF	CHAR_CODE   ; In order to save lines, the W register is initially assumed to contain the character code. Load it into CHAR_CODE register.
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
	
PRINT_Y	MOVLW	LTR_M
	CALL WRITE_CHAR
	RETURN
	
PRINT_ASTERISK	MOVLW	LTR_ASTERISK
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
	
; ***********************************************************************   INTERRUPTS	    **********************************************************************
	
; NOTE: Should all of these be BTFSS?
	
RB4_INT	CALL	DELAYFMS ;  Debounce
	BTFSC	PORTB,LEFT_BTN
	NOP ; TODO: Implement functionality
	BTFSC	PORTB,RIGHT_BTN
	NOP ; TODO: Implement functionality
	BTFSC	PORTB,UP_DOWN_BTN
	CALL	HANDLE_UP_DOWN
	BTFSC	PORTB,CONFIRM_BTN
	NOP ; TODO: Implement functionality
	BCF INTCON, 0
	RETFIE
	
HANDLE_UP_DOWN	; Unless you're in the Main Menu, toggle the row that the cursor is at
	BTFSC	FLAG_REG, 0 ; Check if the Main Menu flag is set, skip accordingly
	GOTO	SKIP_HANDLE_UP_DOWN
	MOVLW	b'00010000'
	XORWF	CURSOR_POS, 1
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

	END
		