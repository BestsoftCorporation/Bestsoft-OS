;Bestsoft OS Kernel

BITS 16
[org 0]

mov ax,cs
mov ds,ax
mov es,ax
mov ax,0x7000
mov ss,ax
mov sp,ss
start:
	
	jmp jumphere
	command:
	jmp overregistration
	lines dw 0
	overregistration:
	
	
	
mov ebx, 0

	mov ebx, 0
	mov dh, 9			; First, draw red background box 
 	mov dl, 9 
 
        call os_move_cursor
 .blackbox:				; Loop to draw all lines of box 
 	call os_move_cursor 
 
 
 	pusha 
	mov ah, 09h 
	mov bh, 0 
	mov cx, 60 
 	mov bl, 80h		; White on red 
 	mov al, ' ' 
 	int 10h 
 	popa 
 
 
 	inc dh 
 	cmp dh, 20 
 	je menuapps1 
 	jmp .blackbox 
	
 menuapps1:
	mov dh, 9			
 	mov dl, 9 
 
        call os_move_cursor
   mov si, welcome
   call print_string
 
 mainloop:
mov si, prompt
   call print_string
 
   mov di, buffer
   call get_string
 
   mov si, buffer
   cmp byte [si], 0  ; blank line?
   je mainloop       ; yes, ignore it
 
 


mov si, buffer
mov di, cmd_Shutdown
call strcmp
jc Shutdown
cmp edx, 4
jne nxt11

Shutdown:
cmp edx, 4
jne bb11
cmd_Shutdown db 'Shutdown', 0
bb11:


    mov ax, 0x1000
    mov ax, ss
    mov sp, 0xf000
    mov ax, 0x5307
    mov bx, 0x0001
    mov cx, 0x0003
    int 0x15

jmp mainloop
nxt11:
mov si, buffer
mov di, cmd_Restart
call strcmp
jc Restart
cmp edx, 4
jne nxt18

Restart:
cmp edx, 4
jne bb18
cmd_Restart db 'Restart', 0
bb18:


int 19h
	
jmp mainloop
nxt18:
mov si, buffer
mov di, cmd_Exit
call strcmp
jc Exit
cmp edx, 4
jne nxt25

Exit:
cmp edx, 4
jne bb25
cmd_Exit db 'Exit', 0
bb25:

call start


jmp mainloop
nxt25:
mov si, buffer
mov di, cmd_Time
call strcmp
jc Time
cmp edx, 4
jne nxt34

Time:
cmp edx, 4
jne bb34
cmd_Time db 'Time', 0
bb34:
add dh, 1
mov dl, 10
call os_move_cursor

	mov bx, tmp_string
	call os_get_time_string
	mov si, bx
	call os_print_string
	call os_print_newline
	

jmp mainloop
nxt34:
mov si, buffer
mov di, cmd_Date
call strcmp
jc Date
cmp edx, 4
jne nxt41

Date:
cmp edx, 4
jne bb41
cmd_Date db 'Date', 0
bb41:
add dh, 1
mov dl, 10
call os_move_cursor
	mov bx, tmp_string 
	call os_get_date_string 
 	mov si, bx 
 	call os_print_string 
 	call os_print_newline 
	

jmp mainloop
nxt41:
  mov si, buffer
   mov di, cmd_help  ; "help" command
   call strcmp
   jc .help

mov si,badcommand
   call print_string 
   jmp mainloop  
 

 
 .help:
   mov si, msg_help
   call print_string
 
   jmp mainloop
welcome db 'BestsoftOS',0x0D,0x0A, 0
 badcommand db '         Bad command entered.', 0x0D, 0x0A, 0
 prompt db '         >', 0
 cmd_help db 'help', 0
 msg_help db '         Bestsoft OS Commands: Shutdown, Restart, Exit, Date, Time', 0x0D, 0x0A, 0
 buffer times 64 db 0
 
 ; ================
 ; calls start here
 ; ================
 
 print_string:

   lodsb        ; grab a byte from SI
 
   or al, al  ; logical or AL by itself
   jz .done   ; if the result is zero, get out
 
   mov ah, 0x0E
   int 0x10      ; otherwise, print out the character!
 
   jmp print_string
 
 .done:
   ret
 
 get_string:
   xor cl, cl
 
 .loop:
   mov ah, 0
   int 0x16   ; wait for keypress
 
   cmp al, 0x08    ; backspace pressed?
   je .backspace   ; yes, handle it
 
   cmp al, 0x0D  ; enter pressed?
   je .done      ; yes, were done
 
   cmp cl, 0x3F  ; 63 chars inputted?
   je .loop      ; yes, only let in backspace and enter
 
   mov ah, 0x0E
   int 0x10      ; print out character
 
   stosb  ; put character in buffer
   inc cl
   jmp .loop
 
 .backspace:
   cmp cl, 0	; beginning of string?
   je .loop	; yes, ignore the key
 
   dec di
   mov byte [di], 0	; delete character
   dec cl		; decrement counter as well
 
   mov ah, 0x0E
   mov al, 0x08
   int 10h		; backspace on the screen
 
   mov al, ' '
   int 10h		; blank character out
 
   mov al, 0x08
   int 10h		; backspace again
 
   jmp .loop	; go to the main loop
 
 .done:
   mov al, 0	; null terminator
   stosb
	

add ebx, 1
 cmp ebx, 10
jg command

		
   mov ah, 0x0E
   mov al, 0x0D
   int 0x10
   mov al, 0x0A
   int 0x10		; newline
 
   ret
 
 strcmp:
 .loop:
   mov al, [si]   ; grab a byte from SI
   mov bl, [di]   ; grab a byte from DI
   cmp al, bl     ; are they equal?
   jne .notequal  ; nope, were done.
 
   cmp al, 0  ; are both bytes (they were equal before) null?
   je .done   ; yes, were done.
 
   inc di     ; increment DI
   inc si     ; increment SI
   jmp .loop  ; loop!
 
 .notequal:
   clc  ; not equal, clear the carry flag
   ret
 
 .done: 	
   stc  ; equal, set the carry flag
   ret
	jumphere:
	mov ebx, 4
	mov al,03h 
	mov ah,0 
	int 10h 

 mov ah, 09h
 mov cx, 1000h
 mov al, 20h

mov bl, 10h
int 10h

     mov ah, 09h
     mov cx, 80d
     mov al, 20h
     
mov bl, 80h
int 10h

	mov si, msg9
call write
call nnn
msg9 db 'Bestosft OS                                                    ' , 0
nnn:
print_time:
	
	mov bx, tmp_string
	call os_get_time_string
	mov si, bx
	call os_print_string
	call os_print_newline





call mouselib_install_driver
mov ax, 0
mov bx, 0
mov cx, 79
mov dx, 24
call mouselib_range
mov dh, 3
mov dl, 2
call mouselib_scale

main:
	call mouselib_freemove
	jc keypress

	push dx
	mov dx, 0
	call os_move_cursor
	mov si, filler
	call os_printt_string
	call os_move_cursor
	
	pop dx
	
	
	cmp dx, 4
	jg start
	cmp cx, 11
	jg start	
	
	cmp ebx, 12
	jne asknext3
	cmp dx, 1
	je asknext
	jmp asknext1
	asknext:
	cmp cx, 11
	jl shutdown
	
	asknext1:
	
	cmp dx, 2
	je asknext2
	jmp asknext3
	asknext2:
	cmp cx, 11
	jl restart
	
	asknext3:
	cmp dx, 3
	je asknext4
	jmp asknext5
	asknext4:
	cmp cx, 11
	jl command
	
	asknext5:
	cmp cx, 0
	jg uuu
        uuu:
	cmp dx, 0
	jne main
	cmp cx, 11
	jl rest	

   	jmp main
jmp nxtxt
rest:


        mov dh, 1			; First, draw red background box 
 	mov dl, 0 
 
 
 .redbox:				; Loop to draw all lines of box 
 	call os_move_cursor 
 
 
 	pusha 
	mov ah, 09h 
	mov bh, 0 
	mov cx, 11 
 	mov bl, 01001111b		; White on red 
 	mov al, ' ' 
 	int 10h 
 	popa 
 
 
 	inc dh 
 	cmp dh, 4 
 	je menuapps 
 	jmp .redbox 
	jmp start
	menuapps:
	
	mov dh, 0			; First, draw red background box 
 	mov dl, 0
	call os_move_cursor
		mov si, msg12
call write
call rrr
msg12 db 10,13,'Shutdown',10,13,'Restart',10,13,'Commander', 0
rrr:
	mov ebx, 12
	jmp main
	shutdown:
	
    mov ax, 0x1000
    mov ax, ss
    mov sp, 0xf000
    mov ax, 0x5307
    mov bx, 0x0001
    mov cx, 0x0003
    int 0x15
	restart:
	int 19h
	jmp start		
nxtxt:	
keypress:
	cmp al, 27
	je exit
	
	jmp main

exit:
	call mouselib_remove_driver
	ret
	jmp main
	os_move_cursor:
	pusha

	mov bh, 0
	mov ah, 2
	int 10h				; BIOS interrupt to move cursor

	popa
	ret
	os_printt_string:
	pusha

	mov ah, 0Eh			; int 10h teletype function

.repeat:
	lodsb				; Get char from string
	cmp al, 0
	je .done			; If char is zero, end of string

	int 10h				; Otherwise, print it
	jmp .repeat			; And move on to next char

.done:
	popa
	ret
os_int_to_string:
	pusha

	mov cx, 0
	mov bx, 10			; Set BX 10, for division and mod
	mov di, .t			; Get our pointer ready

.push:
	mov dx, 0
	div bx				; Remainder in DX, quotient in AX
	inc cx				; Increase pop loop counter
	push dx				; Push remainder, so as to reverse order when popping
	test ax, ax			; Is quotient zero?
	jnz .push			; If not, loop again
.pop:
	pop dx				; Pop off values in reverse order, and add 48 to make them digits
	add dl, '0'			; And save them in the string, increasing the pointer each time
	mov [di], dl
	inc di
	dec cx
	jnz .pop

	mov byte [di], 0		; Zero-terminate string

	popa
	mov ax, .t			; Return location of string
	ret


	.t times 7 db 0

jmp overmousemessages
filler db '', 0
message db 'Mouse click at X: ', 0
seperator db ' Y: ', 0

%include 'mouse.lib'
overmousemessages:

cmp eax , 4
jne noshowtime	
os_get_time_string:
	pusha

	mov di, bx			; Location to place time string

	clc				; For buggy BIOSes
	mov ah, 2			; Get time data from BIOS in BCD format
	int 1Ah
	jnc .read

	clc
	mov ah, 2			; BIOS was updating (~1 in 500 chance), so try again
	int 1Ah

.read:
	mov al, ch			; Convert hours to integer for AM/PM test
	call os_bcd_to_int
	mov dx, ax			; Save

	mov al,	ch			; Hour
	shr al, 4			; Tens digit - move higher BCD number into lower bits
	and ch, 0Fh			; Ones digit
	test byte [fmt_12_24], 0FFh
	jz .twelve_hr

	call .add_digit			; BCD already in 24-hour format
	mov al, ch
	call .add_digit
	jmp short .minutes

.twelve_hr:
	cmp dx, 0			; If 00mm, make 12 AM
	je .midnight

	cmp dx, 10			; Before 1000, OK to store 1 digit
	jl .twelve_st1

	cmp dx, 12			; Between 1000 and 1300, OK to store 2 digits
	jle .twelve_st2

	mov ax, dx			; Change from 24 to 12-hour format
	sub ax, 12
	mov bl, 10
	div bl
	mov ch, ah

	cmp al, 0			; 1-9 PM
	je .twelve_st1

	jmp short .twelve_st2		; 10-11 PM

.midnight:
	mov al, 1
	mov ch, 2

.twelve_st2:
	call .add_digit			; Modified BCD, 2-digit hour
.twelve_st1:
	mov al, ch
	call .add_digit

	mov al, ':'			; Time separator (12-hr format)
	stosb

.minutes:
	mov al, cl			; Minute
	shr al, 4			; Tens digit - move higher BCD number into lower bits
	and cl, 0Fh			; Ones digit
	call .add_digit
	mov al, cl
	call .add_digit

	mov al, ' '			; Separate time designation
	stosb

	mov si, .hours_string		; Assume 24-hr format
	test byte [fmt_12_24], 0FFh
	jnz .copy

	mov si, .pm_string		; Assume PM
	cmp dx, 12			; Test for AM/PM
	jg .copy

	mov si, .am_string		; Was actually AM

.copy:
	lodsb				; Copy designation, including terminator
	stosb
	cmp al, 0
	jne .copy

	popa
	ret


.add_digit:
	add al, '0'			; Convert to ASCII
	stosb				; Put into string buffer
	ret
	.hours_string	db 'hours', 0
	.am_string 	db 'AM', 0
	.pm_string 	db 'PM', 0

os_bcd_to_int:
pusha
mov bl, al	; Store entire number for now
and ax, 0Fh	; Zero-out high bits
mov cx, ax	; CH/CL = lower BCD number, zero extended
shr bl, 4	; Move higher BCD number into lower bits, zero fill msb
mov al, 10
mul bl	; AX = 10 * BL
add ax, cx	; Add lower BCD to 10*higher
mov [.tmp], ax
popa
mov ax, [.tmp]	; And return it in AX!
ret
.tmp	dw 0


os_print_newline:
	pusha

	mov ah, 0Eh			; BIOS output char code

	mov al, 13
	int 10h
	mov al, 10
	int 10h

	popa
	ret
os_print_string:
	pusha

	mov ah, 0Eh			; int 10h teletype function

.repeat:
	lodsb				; Get char from string
	cmp al, 0
	je .done			; If char is zero, end of string

	int 10h				; Otherwise, print it
	jmp .repeat			; And move on to next char

.done:
	popa
	ret

fmt_12_24	db 0
fmt_date db 0, '/'

tmp_string	times 15 db 0

os_get_date_string:
	pusha

	mov di, bx			; Store string location for now
	mov bx, [fmt_date]		; BL = format code
	and bx, 7F03h			; BH = separator, 0 = use month names

	clc				; For buggy BIOSes
	mov ah, 4			; Get date data from BIOS in BCD format
	int 1Ah
	jnc .read

	clc
	mov ah, 4			; BIOS was updating (~1 in 500 chance), so try again
	int 1Ah

.read:
	cmp bl, 2			; YYYY/MM/DD format, suitable for sorting
	jne .try_fmt1

	mov ah, ch			; Always provide 4-digit year
	call .add_2digits
	mov ah, cl
	call .add_2digits		; And '/' as separator
	mov al, '/'
	stosb

	mov ah, dh			; Always 2-digit month
	call .add_2digits
	mov al, '/'			; And '/' as separator
	stosb

	mov ah, dl			; Always 2-digit day
	call .add_2digits
	jmp short .done

.try_fmt1:
	cmp bl, 1			; D/M/Y format (military and Europe)
	jne .do_fmt0

	mov ah, dl			; Day
	call .add_1or2digits

	mov al, bh
	cmp bh, 0
	jne .fmt1_day

	mov al, ' '			; If ASCII months, use space as separator

.fmt1_day:
	stosb				; Day-month separator

	mov ah,	dh			; Month
	cmp bh, 0			; ASCII?
	jne .fmt1_month

	call .add_month			; Yes, add to string
	mov ax, ', '
	stosw
	jmp short .fmt1_century

.fmt1_month:
	call .add_1or2digits		; No, use digits and separator
	mov al, bh
	stosb

.fmt1_century:
	mov ah,	ch			; Century present?
	cmp ah, 0
	je .fmt1_year

	call .add_1or2digits		; Yes, add it to string (most likely 2 digits)

.fmt1_year:
	mov ah, cl			; Year
	call .add_2digits		; At least 2 digits for year, always

	jmp short .done

.do_fmt0:				; Default format, M/D/Y (US and others)
	mov ah,	dh			; Month
	cmp bh, 0			; ASCII?
	jne .fmt0_month

	call .add_month			; Yes, add to string and space
	mov al, ' '
	stosb
	jmp short .fmt0_day

.fmt0_month:
	call .add_1or2digits		; No, use digits and separator
	mov al, bh
	stosb

.fmt0_day:
	mov ah, dl			; Day
	call .add_1or2digits

	mov al, bh
	cmp bh, 0			; ASCII?
	jne .fmt0_day2

	mov al, ','			; Yes, separator = comma space
	stosb
	mov al, ' '

.fmt0_day2:
	stosb

.fmt0_century:
	mov ah,	ch			; Century present?
	cmp ah, 0
	je .fmt0_year

	call .add_1or2digits		; Yes, add it to string (most likely 2 digits)

.fmt0_year:
	mov ah, cl			; Year
	call .add_2digits		; At least 2 digits for year, always


.done:
	mov ax, 0			; Terminate date string
	stosw

	popa
	ret


.add_1or2digits:
	test ah, 0F0h
	jz .only_one
	call .add_2digits
	jmp short .two_done
.only_one:
	mov al, ah
	and al, 0Fh
	call .add_digit
.two_done:
	ret

.add_2digits:
	mov al, ah			; Convert AH to 2 ASCII digits
	shr al, 4
	call .add_digit
	mov al, ah
	and al, 0Fh
	call .add_digit
	ret

.add_digit:
	add al, '0'			; Convert AL to ASCII
	stosb				; Put into string buffer
	ret

.add_month:
	push bx
	push cx
	mov al, ah			; Convert month to integer to index print table
	call os_bcd_to_int
	dec al				; January = 0
	mov bl, 4			; Multiply month by 4 characters/month
	mul bl
	mov si, .months
	add si, ax
	mov cx, 4
	rep movsb
	cmp byte [di-1], ' '		; May?
	jne .done_month			; Yes, eliminate extra space
	dec di
.done_month:
	pop cx
	pop bx
	ret


	.months db 'Jan.Feb.Mar.Apr.May JuneJulyAug.SeptOct.Nov.Dec.'
noshowtime:
write: 
mov bp,sp 
cont: 
lodsb 
or al,al 
jz dne 
mov ah,0x0e 
mov bx,0 
int 10h 
jmp cont 

dne: 
mov sp,bp 
ret 

	

ret
