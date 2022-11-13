IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
; Your variables here
dir db 0
randomm dw 0
snakel dw 3
check db 0
snake dw 2000,2002,2004
; --------------------------
CODESEG
;---------------------------
proc random
	push ax

	mov ax,40h
	mov es,ax
	mov ax,[es:6ch]
	and ax,111111111111b
	mov [randomm],ax
	mov ax,0b800h
	mov es,ax

	pop ax
	ret
endp random
;========================================
proc draw
	push ax
	push bx
	push di
	push cx
;----------------------
	mov cx,[snakel]
	xor di,di
	mov bx,offset snake
	mov di,[bx+6]
	mov ah,0
	mov al,' '
	mov [es:di],ax
	mov ah,200
	mov al,'*'
whytho:
	mov di,[bx]
	add bx,2
	mov [es:di],ax
	loop whytho
;----------------------
	pop cx
	pop di
	pop bx
	pop ax
	ret
endp draw
;========================================
proc next
	push bp
	mov bp,sp

	push bx
	push dx
	push di
	push cx
	push ax

	mov cx,[snakel]
	xor dx,dx
	mov ax,cx
	mov bx,2
	div bx
	add ax,dx
	mov cx,ax
	mov ax,[bp+4]
	mov di, offset snake
nlxt:
	mov bx,[di]
	mov [di],ax
	mov ax,[di+2]
	mov [di+2],bx
	add di,4
	loop nlxt

	pop ax
	pop cx
	pop di
	pop dx
	pop bx
	pop bp
	ret 2
endp next
;========================================
proc w
	push di
	push ax
	push bx

	mov di,[snake]
	cmp di,160
	jb ew
	sub di,(1*80)*2
	mov ax,di
	call cheeck
	mov bl,[check]
	cmp bl,1
	jz ew
	push ax
	call next
	call draw
ew:
	mov [check],0

	pop bx
	pop ax
	pop di
	ret
endp w
;========================================
proc s
	push di
	push ax
	push bx

	mov di,[snake]
	cmp di,3840
	ja ees
	add di,(1*80)*2
	mov ax,di
	call cheeck
	mov bl,[check]
	cmp bl,1
	jz ees
	push ax
	call next
	call draw
ees:
	mov [check],0

	pop bx
	pop ax
	pop di
	ret
endp s
;========================================
proc a
	push di
	push dx
	push ax
	push bx

	mov di,[snake]
	xor dx,dx
	mov ax,di
	mov bx,160
	div bx
	cmp dx,0
	jz ea
	sub di,2
	mov ax,di
	call cheeck
	mov bl,[check]
	cmp bl,1
	jz ea
	push ax
	call next
	call draw
ea:
	mov [check],0

	pop bx
	pop dx
	pop ax
	pop bx
	ret
endp a
;========================================
proc d
	push di
	push dx
	push ax
	push bx

	mov di,[snake]
	xor dx,dx
	mov ax,di
	mov bx,160
	div bx
	cmp dx,158
	jz ed	
	add di,2
	mov ax,di
	call cheeck
	mov bl,[check]
	cmp bl,1
	jz ed
	push ax
	call next
	call draw
ed:
	mov [check],0

	pop bx
	pop dx
	pop ax
	pop bx
	ret
endp d
;========================================
proc cheeck
	push cx
	push di
	push ax
	push bx

	mov cx,[snakel]
	dec cx
	mov di,offset snake
chlck:
	mov bx,[di+2]
	cmp ax,bx
	jnz ok
	mov [check],1
ok:
	add di,2
	loop chlck

	pop bx
	pop ax
	pop di
	pop cx
	ret
endp cheeck
;--------------------------
start:
	mov ax, @data
	mov ds, ax
	mov ax,0b800h
	mov es,ax
	
	mov cx,[snakel]

	xor bx,bx
	mov ah,00
	mov al,' '
cller:
	mov [es:bx],ax
	add bx,2
	cmp bx,4000
	jnz cller
	
call draw
lop:

	mov cx,0ffffh

	lop1:
		
		push cx
		mov cx,5

		lop2:
		
			loop lop2
			pop cx
			loop lop1

	mov al,[dir]
	mov ah,1
	int 16h
	jz noinput
	mov ah,0
	int 16h
noinput:
	mov [dir],al

	cmp al,'w'
	jnz ww
	call w
ww:
	cmp al,'a'
	jnz wa
	call a
wa:
	cmp al,'s'
	jnz ws
	call s
ws:
	cmp al,'d'
	jnz wd
	call d
wd:
	cmp al,27
	jz exit
	jmp lop

	
exit:
	mov ax, 4c00h
	int 21h
END start