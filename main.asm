;IMPORTANT NOTICE: 
;The labels on PORTL are reversed, i.e., PLi is actually PL7-i (i=0, 1, ¡­, 7).  

;Board settings: 
;Connect the four columns C0~C3 of the keypad to PL3~PL0 of PORTL and the four rows R0~R3 to PL7~PL4 of PORTL.
;Connect LED0~LED7 of LEDs to PC0~PC7 of PORTC.
    
; For I/O registers located in extended I/O map, "IN", "OUT", "SBIS", "SBIC", 
; "CBI", and "SBI" instructions must be replaced with instructions that allow access to 
; extended I/O. Typically "LDS" and "STS" combined with "SBRS", "SBRC", "SBR", and "CBR".

.include "m2560def.inc"
.def temp =r16
.def row =r17
.def col =r18
.def mask =r19
.def temp2 =r20
.def symbol = r21
.def counterL=r22
.def counterM=r23
.def counterH=r24
.def display_counter=r25
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F
.macro do_lcd_command
	 ldi r16, @0
	 rcall lcd_command
	 rcall lcd_wait
.endmacro
.macro do_lcd_data
	 mov r16, @0
	 rcall lcd_data
	 rcall lcd_wait
.endmacro
.macro wait_loop
	ldi counterL, 25
	ldi counterM, 90
	ldi counterH, 178
	print_wait_loop: ; add delay for readability
		dec counterH
		brne print_wait_loop
		dec counterM
		brne print_wait_loop
		dec counterL
		brne print_wait_loop
		nop
.endmacro
.cseg
stationsStrCon: .db "Please type the max number of stations: ",0,0

jmp RESET


.org 0x72
RESET:
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
rjmp init

init:
	ldi zl, low(stationsStrCon)
	ldi zh, high(stationsStrCon)
	clr display_counter
	PRINT_STR:
		cpi display_counter, 16
		breq RESET_CURSOR
		lpm r17, z+
		tst r17
		breq END_PRINT_STR
		do_lcd_data r17
		wait_loop
		inc display_counter
		rjmp PRINT_STR
RESET_CURSOR:
	cpi display_counter, 40
	breq CLEAR_SCREEN 
	do_lcd_command 0b00011000 ; shift display
	lpm r17, z+
	tst r17
	breq END_PRINT_STR
	do_lcd_data r17
	wait_loop
	inc display_counter
	rjmp RESET_CURSOR

CLEAR_SCREEN:
	do_lcd_command 0b00010100 ; increment, no display shift
	clr display_counter
	rjmp PRINT_STR

END_PRINT_STR:
	do_lcd_command 0b00000010 ; move cursor home
	clr display_counter
	INC_CURSOR:
		cpi display_counter, 40
		breq FIN_INC
		do_lcd_command 0b00010100 ; increment, display shift
		inc display_counter
		rjmp INC_CURSOR
	FIN_INC:
		do_lcd_command 0b00000110 ; increment, no display shift
		do_lcd_command 0b00001111 ; Cursor on, bar, blink

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
		cpi col, 3 ; check if we^Òre on the last column
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
		subi temp, -48
		rjmp screen_write
	letters:
		ldi temp, 'A' ; 
		add temp, row ; increment from 1 (A in ASCII) by the row value
		rjmp screen_write
	symbols:
		cpi col, 0 ; check if we have a star
		breq star
		cpi col, 1 ; or if we have zero
		breq zero
		ldi temp, '#' ; 
		rjmp screen_write
	star:
		ldi temp, '*' ; 42 is star in ASCII
		rjmp screen_write
	zero:
		ldi temp, '0'
	screen_write:
		mov symbol, temp
		do_lcd_data symbol
		rjmp set_decrementers
		do_lcd_data symbol
	set_decrementers:
		wait_loop
		rjmp keypad_scanner ; return to caller

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
