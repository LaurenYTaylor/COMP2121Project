;IMPORTANT NOTICE: 
;The labels on PORTL are reversed, i.e., PLi is actually PL7-i (i=0, 1, ��, 7).  

;Board settings: 
;Connect the four columns C0~C3 of the keypad to PL3~PL0 of PORTL and the four rows R0~R3 to PL7~PL4 of PORTL.
;Connect LED0~LED7 of LEDs to PC0~PC7 of PORTC.
    
; For I/O registers located in extended I/O map, "IN", "OUT", "SBIS", "SBIC", 
; "CBI", and "SBI" instructions must be replaced with instructions that allow access to 
; extended I/O. Typically "LDS" and "STS" combined with "SBRS", "SBRC", "SBR", and "CBR".

.include "m2560def.inc"
.def numStats=r3
.def holder=r4
.def numPressed = r13
.def tempVar = r14
.def numLetters = r15
.def temp =r16
.def row =r17
.def col =r18
.def mask =r19
.def temp2 =r20
.def symbol = r21
.def programCounter=r23
.def workingRegister = r24
.def display_counter=r25
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F
.macro do_lcd_command
	push r16
	 ldi r16, @0
	 rcall lcd_command
	 rcall lcd_wait
	pop r16
.endmacro
.macro do_lcd_data
	push r16
	 mov r16, @0
	 rcall lcd_data
	 rcall lcd_wait
	pop r16
.endmacro
.macro wait_loop
	push r22
	push r23
	push r24
/*
	ldi  r22, 5
    ldi  r23, 15
    ldi  r24, 24
	L1: dec  r20
		brne L1
		dec  r23
		brne L1
		dec  r22
		brne L1
*/
	ldi r22, 25
	ldi r23, 90
	ldi r24, 178
	dec_wait_loop: ; I made the wait loop a macro to make code neater
		dec r24
		brne dec_wait_loop
		dec r23
		brne dec_wait_loop
		dec r22
		brne dec_wait_loop
		nop

	pop r24
	pop r23
	pop r22
.endmacro
.macro load_string
	ldi zl, low(@0<<1) ; point to memory location of string
	ldi zh, high(@0<<1)
	clr display_counter
.endmacro
.cseg
numStatStr: .db "Please type the max number of stations: ",0,0 ;<- these zeros add some kind of padding that stops weird characters been printed at the end for some reason
nameStatStr: .db "Please type name of Station ",0,0
tooManyStats: .db "Number of stations must be less than 10.",0,0


RESET:
	clr tempVar
	ldi temp, low(RAMEND)
	out SPL, temp
	ldi temp, high(RAMEND)
	out SPH, temp
	ldi temp, PORTLDIR ; columns are outputs, rows are inputs
	STS DDRL, temp     ; cannot use out
	ser temp
	out DDRF, temp
	out DDRA, temp
	clr temp
	out PORTF, temp
	out PORTA, temp
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink
	clr numStats
	clr programCounter

init:
	do_lcd_command 0b00000001
	clr numStats
	load_string numStatStr
	rcall PRINT_STR

keypad_scanner:
		ldi mask, INITCOLMASK ; initial column mask
		clr col ; initial column
		colloop:
		STS PORTL, mask ; set column to mask value
		; (sets column 0 off)
		ldi temp, 0xFF ; implement a delay so the
		; hardware can stabilize
	delay:
		dec temp
		brne delay
		LDS temp, PINL ; read PORTL. Cannot use in 
		andi temp, ROWMASK ; read only the row bits
		cpi temp, 0xF ; check if any rows are grounded
		breq nextcol ; if not go to the next column
		ldi mask, INITROWMASK ; initialise row check
		clr row ; initial row
	rowloop:      
		mov temp2, temp
		and temp2, mask ; check masked bit
		brne skipconv ; if the result is non-zero,
		; we need to look again
		rcall convert ; if bit is clear, convert the bitcode
		jmp keypad_scanner ; and start again
	skipconv:
		inc row ; else move to the next row
		lsl mask ; shift the mask to the next bit
		jmp rowloop          
	nextcol:     
		cpi col, 3 ; check if we^�re on the last column
		breq keypad_scanner ; if so, no buttons were pushed,
		; so start again.
		sec ; else shift the column mask:
		; We must set the carry bit
		rol mask ; and then rotate left by a bit,
		; shifting the carry into
		; bit zero. We need this to make
		; sure all the rows have
		; pull-up resistors
		inc col ; increment column value
		jmp colloop ; and check the next column
		; convert function converts the row and column given to a
		; binary number and also outputs the value to PORTC.
		; Inputs come from registers row and col and output is in
		; temp.
	convert:
		cpi col, 3 ; if column is 3 we have a letter
		breq letters
		cpi row, 3 ; if row is 3 we have a symbol or 0
		breq symbols
		mov temp, row ; otherwise we have a number (1-9)
		lsl temp ; temp = row * 2
		add temp, row ; temp = row * 3
		add temp, col ; add the column address
		; to get the offset from 1
		inc temp ; add 1. Value of switch is
		; row*3 + col + 1.
		mov numPressed, temp
		clr r22
		cp numStats,r22 ; check if numStats already has a digit
		breq FIRST_DIGIT
		ldi r22, 10
		mul numStats, r22
		mov numStats, r0
		add numStats, temp
		subi temp, -48
		rjmp screen_write
	FIRST_DIGIT:
		mov numStats, temp
		subi temp, -48
		rjmp screen_write
	letters:
		ldi temp, 'A' ; 
		add temp, row ; increment from 1 (A in ASCII) by the row value
		correct_last_num:
			do_lcd_command 0b00010000 ; move cursor back 1 place
			wait_loop
			mov workingRegister, numPressed
			subi workingRegister, 2
			ldi display_counter, 3 ; display_counter is used as a working register here
			mul workingRegister, display_counter ; as it is only used for printing a string
			mov workingRegister, r0
			add workingRegister, temp
			cpi workingRegister, 'Q'
			brlt no_change ; handles the lack of Q on a keyboard
			subi workingRegister, -1
		no_change:
			mov temp, workingRegister
			rjmp screen_write
	symbols:
		cpi col, 0 ; check if we have a star
		breq star
		cpi col, 1 ; or if we have zero
		breq zero
		do_lcd_command 0b00000001
		mov workingRegister, holder
		cpi workingRegister, 0
		brge not_end
		cpi programCounter, 5
		brlt END_INPUT; if the hash is pressed and the monorail isn't running, end input
	not_end:
		rjmp PRINT_STRINGS
	star:
		ldi temp, '*' ; 42 is star in ASCII
		do_lcd_command 0b00010000 ; turns * into a back button to fix typos
		rjmp set_decrementers
	zero:
		ldi temp, '0'
	screen_write:
		mov symbol, temp
		do_lcd_data symbol
	set_decrementers:
		wait_loop
		rjmp keypad_scanner ; return to caller

END_INPUT:
	inc programCounter
	cpi programCounter, 1
	breq NAME_STATIONS
	
NAME_STATIONS:
	ldi r22, 10
	cp r22, numStats ; if 10 is less than numStats, tell the user they have entered too many stations 
	brge STAT_AMOUNT_OK
	rjmp TOO_MANY_STATIONS
STAT_AMOUNT_OK:
	push temp
	push workingRegister
	mov workingRegister, numStats
	mov holder, workingRegister
PRINT_STRINGS:	
	mov workingRegister, holder
	cpi workingRegister, 0
	brge CONTINUE
	rjmp END_INPUT
CONTINUE:
	do_lcd_command 0b00000001
	load_string nameStatStr
	rcall PRINT_STR
	subi workingRegister, -48
	do_lcd_data workingRegister
	wait_loop
	ldi workingRegister, ':'
	do_lcd_data workingRegister
	wait_loop
	do_lcd_command 0b00000010 ; move cursor home
	clr display_counter
	INC_CURSOR2: ; move cursor to the first place in the second line
		cpi display_counter, 40 
		breq FIN_INC2
		do_lcd_command 0b00010100 ; increment, display shift
		inc display_counter
		rjmp INC_CURSOR2
	FIN_INC2:
		do_lcd_command 0b00000110 ; increment, no display shift
		do_lcd_command 0b00001111 ; Cursor on, bar, blink
		dec holder
		pop temp
		pop workingRegister
	jmp keypad_scanner


TOO_MANY_STATIONS: ; gives error message and makes user re-enter a number less than 10
	do_lcd_command 0b00000001
	load_string tooManyStats
	rcall PRINT_STR
	clr numStats
	clr display_counter
	clr programCounter
	jmp init

PRINT_STR:
		cpi display_counter, 16 ; if the first line has been used up, start scrolling
		breq SCROLL_CURSOR
		lpm r17, z+ ; get value of byte of string then increment pointer
		tst r17 ; test if the value of the byte is null (i.e. it's the end of the string)
		breq END_PRINT_STR
		do_lcd_data r17 ; print the character
		wait_loop
		inc display_counter
		rjmp PRINT_STR
	SCROLL_CURSOR:
		cpi display_counter, 40 ; see if the screen is at the end of its length
		breq CLEAR_SCREEN 
		do_lcd_command 0b00011000 ; shift display (allows scrolling)
		lpm r17, z+
		tst r17
		breq END_PRINT_STR
		do_lcd_data r17
		wait_loop
		inc display_counter
		rjmp SCROLL_CURSOR
	CLEAR_SCREEN: ; I'll need to put something here if we have lines more than 40 characters.. let's try not to
		;do_lcd_command 0b00010100 ; increment, no display shift
		clr display_counter
		rjmp PRINT_STR
	END_PRINT_STR:
		;ldi programCounter, 1
		cpi programCounter, 1
		brlt MOVE_CURSOR
		ret
	MOVE_CURSOR:
		do_lcd_command 0b00000010 ; move cursor home
		clr display_counter
		wait_loop
		INC_CURSOR: ; move cursor to the first place in the second line
			cpi display_counter, 40 
			breq FIN_INC
			do_lcd_command 0b00010100 ; increment, display shift
			inc display_counter
			rjmp INC_CURSOR
		FIN_INC:
			do_lcd_command 0b00000110 ; increment, no display shift
			do_lcd_command 0b00001111 ; Cursor on, bar, blink
	ret



.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4
.macro lcd_set
 sbi PORTA, @0
.endmacro
.macro lcd_clr
 cbi PORTA, @0
.endmacro
;
; Send a command to the LCD (r16)
;
lcd_command:
 out PORTF, r16
 nop
 lcd_set LCD_E
 nop
 nop
 nop
 lcd_clr LCD_E
 nop
 nop
 nop
 ret
lcd_data:
 out PORTF, r16
 lcd_set LCD_RS
 nop
 nop
 nop
 lcd_set LCD_E
 nop
 nop
 nop
 lcd_clr LCD_E
 nop
 nop
 nop
 lcd_clr LCD_RS
 ret
lcd_wait:
 push r16
 clr r16
 out DDRF, r16
 out PORTF, r16
 lcd_set LCD_RW
lcd_wait_loop:
 nop
 lcd_set LCD_E
 nop
 nop
 nop
 in r16, PINF
 lcd_clr LCD_E
 sbrc r16, 7
 rjmp lcd_wait_loop
 lcd_clr LCD_RW
 ser r16
 out DDRF, r16
 pop r16
 ret
.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead
sleep_1ms:
 push r24
 push r25
 ldi r25, high(DELAY_1MS)
 ldi r24, low(DELAY_1MS)
delayloop_1ms:
 sbiw r25:r24, 1
 brne delayloop_1ms
 pop r25
 pop r24
 ret
sleep_5ms:
 rcall sleep_1ms
 rcall sleep_1ms
 rcall sleep_1ms
 rcall sleep_1ms
 rcall sleep_1ms
 ret
