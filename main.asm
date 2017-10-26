;IMPORTANT NOTICE: 
;The labels on PORTL are reversed, i.e., PLi is actually PL7-i (i=0, 1, бн, 7).  

;Board settings: 
;Connect the four columns C0~C3 of the keypad to PL3~PL0 of PORTL and the four rows R0~R3 to PL7~PL4 of PORTL.
;Connect LED0~LED7 of LEDs to PC0~PC7 of PORTC.
    
; For I/O registers located in extended I/O map, "IN", "OUT", "SBIS", "SBIC", 
; "CBI", and "SBI" instructions must be replaced with instructions that allow access to 
; extended I/O. Typically "LDS" and "STS" combined with "SBRS", "SBRC", "SBR", and "CBR".

/*
 * Tasks, counted in programCounter
 * 1. Getting the num stations
 * 2. Getting the station names
 * 3. Getting the times between stations 
 * 4. Getting the monorail stopping time
 * 5. Running the monorail
 */

.include "m2560def.inc"
.def numStats=r3
.def holder=r4
.def holder2=r5
.def lastNum=r6
.def stoppingTime=r7
.def stopAtStat=r8
.def numPressed = r13
.def tempVar = r14
.def numLetters = r15
.def temp =r16
.def row =r17
.def col =r18
.def working2 =r19 ; things are only saved here while polling keybad
.def temp2 =r20
.def symbol = r21
.def programCounter=r23
.def workingRegister = r24 ; Nothing is saved here
.def display_counter=r25
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F

.macro xloadaddr ; load address of data mem into x
	ldi xl, low(@0<<1)
	ldi xh, high(@0<<1)
.endmacro
.macro yloadaddr 
	ldi yl, low(@0<<1)
	ldi yh, high(@0<<1)
.endmacro
.macro zloadaddr ;
	ldi zl, low(@0<<1)
	ldi zh, high(@0<<1)
.endmacro

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
.macro do_lcd_data_const
	 push r16
	 ldi r16, @0
	 rcall lcd_data
	 rcall lcd_wait
	pop r16
.endmacro

.macro wait_one_sec
	push r22
	push r23
	push r24

	ldi  r22, 82
    ldi  r23, 43
    ldi  r24, 0
	L2: dec  r24
		brne L2
		dec  r23
		brne L2
		dec  r22
		brne L2
		lpm
		nop

	pop r24
	pop r23
	pop r22
.endmacro

.macro wait_loop
	push r22
	push r23
	push r24

	ldi  r22, 5 ; This loop is quicker, for repeated testing
    ldi  r23, 15
    ldi  r24, 24
	L1: dec  r20
		brne L1
		dec  r23
		brne L1
		dec  r22
		brne L1
/*
	ldi r22, 25
	ldi r23, 90
	ldi r24, 178
	dec_wait_loop: 
		dec r24
		brne dec_wait_loop
		dec r23
		brne dec_wait_loop
		dec r22
		brne dec_wait_loop
		nop
*/
	pop r24
	pop r23
	pop r22
.endmacro
.macro load_string
	ldi zl, low(@0<<1) ; point to memory location of string
	ldi zh, high(@0<<1)
	clr display_counter
.endmacro
.macro newline
	push display_counter
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
	pop display_counter
.endmacro

.dseg
/*
 * Exists as a solid block in data memory. Each time "Enter station name x" is printed the pointer is aligned to relevant block of ten.
 * After this, every time a number is entered the pointer is incremented by 1 (10x 1byte numbers representing times to next station)
 */
stationsMem: .byte 110 ; 11*letters * 10 stations
stationTimes: .byte 10 ; 
stopTime: .byte 1


.cseg
.org 0x0
jmp RESET
.org INT0addr ; INT0addr is the address of EXT_INT0
jmp EXT_INT0
.org INT1addr ; INT1addr is the address of EXT_INT1
jmp EXT_INT1
numStatStr: .db "Please type the max number of stations: ",0,0 ;<- these zeros add some kind of padding that stops weird characters been printed at the end for some reason
nameStatStr: .db "Please type the name of Station ",0,0
tooManyStats: .db "Number of stations must be from 1 to 10.",0,0
timingStr1: .db "Time from Station ",0,0
timingStr2: .db " to Station ",0,0
timingStr3: .db " is:",0,0
stopTimeStr1: .db "The stop time of the monorail at any",0,0
stopTimeStr2: .db "station is: ",0,0
finalStr1: .db "Configuration complete.",0
finalStr2: .db "Please wait 5 seconds.",0,0
incorrectStr: .db "The time must be from 1 to 10.",0,0
nextStatStr: .db "Next Station: ",0,0

RESET:
   ; zloadaddr stopTime ; point z to memory allocated to hold the stop time
    clr lastNum
	clr tempVar
	ldi temp, low(RAMEND)
	out SPL, temp
	ldi temp, high(RAMEND)
	out SPH, temp
	ldi temp, (1<<PE4)
	out DDRE, temp
	ldi temp, (1<<WGM30)|(1<<COM3B1)
	sts TCCR3A, temp
	ldi temp, (1<<CS31)
	STS TCCR3B, temp
	ldi temp, 0x00
	STS OCR3BL, temp
	clr temp
	sts OCR3BH, temp
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
	; Set up push buttons
	ser temp
	out PORTD, temp
	clr temp
	out DDRD, temp
	ldi temp, (2 << ISC10) | (2 << ISC00)
	sts EICRA, temp
	in temp, EIMSK
	ori temp, (1<<INT0) | (1<<INT1)
	out EIMSK, temp
	clr numStats
	clr programCounter
	rjmp init

EXT_INT0:
	push temp
	in temp, SREG
	push temp
	ldi workingRegister, 1
	mov stopAtStat, workingRegister
	pop temp
	out SREG, temp
	pop temp
	sbi EIFR, INT0
	reti

EXT_INT1:
	push temp
	in temp, SREG
	push temp
	ldi workingRegister, 1
	mov stopAtStat, workingRegister
	pop temp
	out SREG, temp
	pop temp
	sbi EIFR, INT1
	reti

init:
	do_lcd_command 0b00000001
	clr numStats
	load_string numStatStr
	rcall PRINT_STR

keypad_scanner:
		ldi working2, INITCOLMASK ; initial column mask
		clr col ; initial column
		colloop:
		STS PORTL, working2 ; set column to mask value
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
		ldi working2, INITROWMASK ; initialise row check
		clr row ; initial row
	rowloop:
		mov temp2, temp
		and temp2, working2 ; check masked bit
		brne skipconv ; if the result is non-zero,
		; we need to look again
		rcall convert ; if bit is clear, convert the bitcode
		jmp keypad_scanner ; and start again
	skipconv:
		inc row ; else move to the next row
		lsl working2 ; shift the mask to the next bit
		jmp rowloop          
	nextcol:     
		cpi col, 3 ; check if we're on the last column
		breq keypad_scanner ; if so, no buttons were pushed,
		; so start again.
		sec ; else shift the column mask:
		; We must set the carry bit
		rol working2 ; and then rotate left by a bit,
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
		brne NUMBERS
		rjmp symbols
	NUMBERS:
		mov temp, row ; otherwise we have a number (1-9)
		lsl temp ; temp = row * 2
		add temp, row ; temp = row * 3
		add temp, col ; add the column address
		; to get the offset from 1
		inc temp ; add 1. Value of switch is
		; row*3 + col + 1.
	START_MATHS:
		mov numPressed, temp
		cpi programCounter, 1
		breq WRITE_NUM
		cpi programCounter, 2
		breq SKIP_MATHS
		cpi programCounter, 3
		breq SKIP_MATHS
		clr r22
		cp numStats,r22 ; check if numStats already has a digit
		breq FIRST_DIGIT
		ldi r22, 10
		mul numStats, r22
		mov numStats, r0
		add numStats, temp
		subi temp, -48
		rjmp screen_write
	SKIP_MATHS:
		; multiply lastNum by ten, add temp, store in lastNum
		ldi workingRegister, 10
		mul workingRegister, lastNum
		mov lastNum, r0
		add lastNum, temp
        st x, lastNum ; save time in data memory
	WRITE_NUM:
		subi temp, -48
		rjmp screen_write
	FIRST_DIGIT:
		mov numStats, temp
		subi temp, -48
		rjmp screen_write
	letters:
		ldi temp, 'A' ; 
		add temp, row ; increment from 1 (A in ASCII) by the row value
		cpi temp, 'D'
		breq PRINT_SPACE
		correct_last_num:
			do_lcd_command 0b00010000 ; move cursor back 1 place
			wait_loop
			; the following is the maths needed to calculate correct ASCII value
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
            st x+, temp ;
			rjmp screen_write
	PRINT_SPACE:
		ldi temp, 32
		st x+, temp 
		rjmp screen_write
	symbols:
		cpi col, 0 ; check if we have a star
		breq goto_star
		cpi col, 1 ; or if we have zero
		breq goto_zero
		do_lcd_command 0b00000001
		cpi programCounter, 1
		breq JUMP_TO_NAMING
		cpi programCounter, 2
		breq JUMP_TO_TIMING
		cpi programCounter, 3
		breq goto_final
		inc programCounter; if the programCounter is at zero, prepare to name stations
		rjmp NAME_STATIONS
	goto_final:
		jmp JUMP_TO_FINAL
	goto_zero:
		jmp zero
	goto_star:
		jmp star
	JUMP_TO_NAMING:
		clr temp
		st x+, temp
		jmp PRINT_NAMING_STRINGS ; if programCounter at one, keep asking for station names
	JUMP_TO_TIMING:
		ld workingRegister, x
		cpi workingRegister, 11
		brge NOT_TIME_OK
		cpi workingRegister, 1
		brge TIME_OK
		NOT_TIME_OK:
		load_string incorrectStr ; load the first part of the timing string
		rcall PRINT_STR
		wait_loop
		wait_loop
		clr lastNum
		jmp PRINT_TIMING_STRINGS
	TIME_OK:
		ldi workingRegister, 1
		add xl, workingRegister
		clr lastNum
		adc xh, lastNum
		rjmp INC_STATION ; if programCounter at two, keep asking for travel times
	JUMP_TO_FINAL:
		ld workingRegister, x ; Confirem it's <= 10
		cpi workingRegister, 11
		brge NOT_GOOD_STOP_TIME
		cpi workingRegister, 1
		brge GOOD_STOP_TIME
		NOT_GOOD_STOP_TIME:
		load_string incorrectStr ; load the first part of the timing string
		rcall PRINT_STR
		wait_loop
		wait_loop
		clr lastNum
		dec programCounter
		jmp MONO_STOP_TIME
		GOOD_STOP_TIME:
			rjmp FINAL_STRING ; if pC at three, jump to configuration complete string
	star:
		ldi temp, '*' ; 42 is star in ASCII
		do_lcd_command 0b00010000 ; turns * into a back button to fix typos
		sbiw xh:xl, 1
		rjmp set_decrementers
	zero:
		ldi temp, 0
		rjmp START_MATHS
	screen_write:
		mov symbol, temp
		do_lcd_data symbol
	set_decrementers:
		wait_loop
		rjmp keypad_scanner ; return to caller

NAME_STATIONS:
	ldi r22, 10
	cp r22, numStats ; if 10 is less than numStats, tell the user they have entered too many stations 
	brlt NOT_STAT_AMOUNT_OK
	ldi r22, 1
	cp numStats, r22
	brge STAT_AMOUNT_OK
	NOT_STAT_AMOUNT_OK:
	rjmp TOO_MANY_STATIONS
STAT_AMOUNT_OK:
	clr holder
	;ldi xl, low(stationsMem<<1)
	;ldi xh, high(stationsMem<<1)
PRINT_NAMING_STRINGS:	
	inc holder
	cp holder, numStats
	brlt CONTINUE
	cp holder, numStats
	breq CONTINUE
	rjmp ENTER_TIMES
CONTINUE:
	do_lcd_command 0b00000001
	load_string nameStatStr
	rcall PRINT_STR
	mov workingRegister, holder
	cpi workingRegister, 10
	brlt LOW_STATION_NUM
	ldi workingRegister, '1'
	do_lcd_data workingRegister
	ldi workingRegister, '0'
	do_lcd_data workingRegister
	rjmp PRINT_END_STAT_NAME_ASK
LOW_STATION_NUM:
	mov workingRegister, holder
	subi workingRegister, -48
	do_lcd_data workingRegister
	subi workingRegister, 48
PRINT_END_STAT_NAME_ASK:
	wait_loop
	do_lcd_command 0b00011000
	do_lcd_data_const ':'
	wait_loop
	wait_loop
	newline
	rcall adjustX ; set pointer to (holder - 1)*10 + stationsMem
	jmp keypad_scanner

TOO_MANY_STATIONS: ; gives error message and makes user re-enter a number less than 10
	do_lcd_command 0b00000001
	load_string tooManyStats
	rcall PRINT_STR
	clr numStats
	clr display_counter
	clr programCounter
	jmp init

; holder holds the number of the 'From' station in the string,
; holder2 holds the number of the 'To' station
; i.e. the complete string is: "The time from Station "+holder+" to Station "+holder2+" is: "
ENTER_TIMES:
	inc programCounter
	clr holder
	xloadaddr stationTimes
INC_STATION:
	clr holder2
	inc holder
	mov holder2, holder
	cp holder,numStats
	brlt CHECK_TO_STATION
	cp holder,numStats
	breq CHECK_TO_STATION
	rjmp MONO_STOP_TIME
CHECK_TO_STATION:
	inc holder2
	cp holder2, numStats
	brlt PRINT_TIMING_STRINGS
	cp holder2, numStats
	breq PRINT_TIMING_STRINGS
	ldi workingRegister, 1 ; 
	mov holder2, workingRegister
PRINT_TIMING_STRINGS:
	do_lcd_command 0b00000001
	load_string timingStr1 ; load the first part of the timing string
	rcall PRINT_STR
	mov workingRegister, holder
	subi workingRegister, -48
	do_lcd_data workingRegister ; print the 'From' station
	subi workingRegister, 48
	wait_loop
	load_string timingStr2 ; print the second part of the timing string
	rcall SCROLL_CURSOR
	mov workingRegister, holder2
	subi workingRegister, -48
	do_lcd_data workingRegister ; print the 'To' station
	subi workingRegister, 48
	wait_loop
	load_string timingStr3 ; print the last part of the timing string
	rcall SCROLL_CURSOR
	wait_loop
	newline
	jmp keypad_scanner

MONO_STOP_TIME:
	inc programCounter 
	do_lcd_command 0b00000001
	load_string stopTimeStr1 ; load first part of string that asks for stop time
	rcall PRINT_STR
	wait_loop
	wait_loop
	newline
	load_string stopTimeStr2 ; load second part of string that asks for stop time
	rcall PRINT_STR
	xloadaddr stopTime
	jmp keypad_scanner
	FINAL_STRING:
		do_lcd_command 0b00000001
		load_string finalStr1 ; load the first part of configuration complete string
		rcall PRINT_STR
		wait_loop
		newline
		load_string finalStr2 ; load the second part of configuration complete string
		rcall PRINT_STR
		wait_one_sec ; wait 5 seconds
		wait_one_sec
		wait_one_sec
		wait_one_sec
		wait_one_sec
		do_lcd_command 0b00000001 ; clear screen


RUN_MONORAIL:
	clr holder
	clr working2
	zloadaddr stopTime
	ld r25, z
	mov stoppingTime, temp
INC_STATION2:
	do_lcd_command 0b00000001
	ldi temp, 0x4A
	STS OCR3BL, temp
	clr temp
	sts OCR3BH, temp
	inc holder
	cp numStats, holder
	brlt RUN_MONORAIL
	clr stopAtStat
GOTO_NEXT_STAT:
	load_string nextStatStr ;
	rcall FAST_PRINT_STR
	newline
	rcall adjustX
PRINT_NEXT_STAT:
	ld r25, x+
	tst r25
	breq TRAVELLING
	do_lcd_data r25
	rjmp PRINT_NEXT_STAT
TRAVELLING:
	rcall timesAdjustY
	ld r25, y
TRAVEL_TIME:
	cpi r25, 0
	breq SHOULD_MONO_STOP
	wait_one_sec
	dec r25
	rjmp TRAVEL_TIME
SHOULD_MONO_STOP:
	mov workingRegister, stopAtStat
	cpi workingRegister,1 
	breq CURRENT_STATION_NAME
	rjmp INC_STATION2
CURRENT_STATION_NAME:
	ldi temp, 0x00
	STS OCR3BL, temp
	clr temp
	sts OCR3BH, temp
	do_lcd_command 0b00000001
	rcall adjustX
PRINTING_CUR_STAT:
	ld r25, x+
	tst r25
	breq WAIT_AT_STAT
	do_lcd_data r25
	rjmp PRINTING_CUR_STAT
WAIT_AT_STAT:
	mov workingRegister, stoppingTime
WAIT_TIME:
	cpi workingRegister, 0
	breq BACK_TO_START
	wait_one_sec
	dec workingRegister
	rjmp WAIT_TIME
BACK_TO_START:
	rjmp INC_STATION2




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
		;cpi display_counter, 40 ; see if the screen is at the end of its length
;		breq CLEAR_SCREEN 
		do_lcd_command 0b00011000 ; shift display (allows scrolling)
		lpm r17, z+
		tst r17
		breq END_PRINT_STR
		do_lcd_data r17
		wait_loop
		inc display_counter
		rjmp SCROLL_CURSOR
	;CLEAR_SCREEN: ; I'll need to put something here if we have lines more than 40 characters.. let's try not to
		;do_lcd_command 0b00010100 ; increment, no display shift
		;clr display_counter
		;rjmp PRINT_STR
	END_PRINT_STR:
		cpi programCounter, 0
		breq MOVE_CURSOR
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

FAST_PRINT_STR:
		lpm r17, z+ ; get value of byte of string then increment pointer
		tst r17 ; test if the value of the byte is null (i.e. it's the end of the string)
		breq END_FAST_PRINT_STR
		do_lcd_data r17 ; print the character
		inc display_counter
		rjmp FAST_PRINT_STR
	END_FAST_PRINT_STR:
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

 adjustX:
    ; prelogue
    push working2
	push workingRegister
    ; body
    mov workingRegister, holder
	subi workingRegister, 1
    ldi working2, 11
    mul workingRegister, working2
    mov workingRegister, r0 ; number already saved * 11 (to get the offset from initial pointer)
    xloadaddr stationsMem ; load initial pointer to x
    add xl, workingRegister ; add offset to initial pointer
	clr working2
	adc xh, working2
    ; prologue
	pop workingRegister
    pop working2
    ret;

timesAdjustY:
	push working2
	push workingRegister
	; body
	mov workingRegister, holder
	subi workingRegister, 1
	yloadaddr stationTimes ; load initial pointer to x
	add yl, workingRegister ; add offset to initial pointer
	clr working2
	adc yh, working2
	; prologue
	pop workingRegister
	pop working2
	ret;

PRINT_TIME:
	;pre
	push workingRegister
	push working2

	cpi r17, 10
	breq TEN
	subi r17, -48
	do_lcd_data r17
	rjmp END_PRINT
	TEN:
		ldi workingRegister, '1'
		do_lcd_data workingRegister
		ldi workingRegister, '0'
		do_lcd_data workingRegister

	; post
	END_PRINT:
	pop working2
	pop workingRegister
	ret

