IDEAL
MODEL small
STACK 100h
DATASEG

food dw (4*80+40)*2	; the cordinate of food
dir db 0  	   	; direction of star moving
star_Count dw 1  	;number of stars 
star dw (13*80+40)*2 	; first star place

CODESEG

;------------------------general perpuse porc-----------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;clean the screen for starting the game
proc clear_Screen
	push di
	push ax
	push cx
	mov al,' '
	mov ah, 0
	mov di, 0 
	mov cx, 4000
loop_clear_screen:
	mov [es:di], ax
	add di,2
	loop loop_clear_screen
	pop cx
	pop ax
	pop di
	ret
endp clear_Screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;small delay useing 2 loops
proc delay
	push cx
	mov cx, 0ffffh
outer_loop:
	push cx
	mov cx, 500
inner_loop:
	loop inner_loop
	pop cx
	loop outer_loop
	pop cx
	ret
endp delay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---------------------------end general perpuse porc------------------------

;----------------------------control screen proc------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this proc show the starts on the screen
;in start push the postion of star array
proc put_Star

	push bp
	mov bp, sp
	push di
	push cx
	push ax
	push bx
	;;;;;;;;;;;;;;;;;
	mov al, '*'  ;star
	mov ah, 4    ;red
	mov cx, [bp + 4]  
	mov bx, cx 	;number of stars to address bx
	shl bx,1 ; bx*2
loop_Star:
	mov di, [star + bx - 2] 
	mov [es:di], ax
	dec bx
	dec bx
	loop loop_Star
	;;;;;;;;;;;;;;;;;
	pop bx
	pop ax
	pop cx
	pop di
	pop bp
	ret 2
endp put_Star
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check if the cordinate of the food are simmiler to the cordinate of the head of the snake
;recive food postiton, star one
proc check_food
	push bp
	mov bp,sp
	push bx
	push cx
	mov bx, [bp+4]
	mov cx, [bp+6]
	cmp cx,bx
	jne out_food
	inc [star_count]
	call random
out_food: 
	pop cx
	pop bx
	pop bp
	ret 4
endp check_food
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this proc should make a random star on screen as food for the snake
;*i haven't made this*
Proc random
	Push ax
	Push di
	Mov al, 'x'
	Mov ah, 7
	Mov di, (4*80+40)*2
	Mov [es:di], ax
	pop di 
	pop ax
	ret
Endp random
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---------------------------end control screen proc---------------------------


;---------------------------move related proc----------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;delete the last start from the snake
proc delete_last_star
	push ax
	mov al, ' '
	mov ah, 0
	mov bx, [bp+4]
	shl bx,1
	mov di, [star + bx - 2]
	mov [es:di],ax
	pop ax
	ret
endp delete_last_star
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;move all star cordnations to the in preperation to the next move
;recive the number of stars
Proc sub_move
	push bp
	mov bp,sp
	push cx
	push bx
	push di
;;;;;;;;;;;;;;;;;;;;;;;
	call delete_last_star
	mov cx, [bp+4]
	cmp cx,01
	je leave_sub_move
	dec cx
	mov bx, cx
	shl bx,1
loop_move_up:
	mov di, [star + bx - 2]
	mov [star + bx ], di
	loop loop_move_up
leave_sub_move:
;;;;;;;;;;;;;;;;;;;;;;;
	pop di
	pop cx
	pop bx
	pop bp
	ret 2
Endp sub_move
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this proc control the move up
proc move_Up
	push [star_Count]
	Call sub_move
	sub [star],160
	push [star]
	push [food]
	call check_food
	ret 
endp move_Up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this proc control the move left
proc move_left
	push [star_Count]
	Call sub_move
	sub [star],2
	push [star]
	push [food]
	call check_food
	ret 
endp move_left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------end move related proc----------------------

start:

	mov ax, @data
	mov ds, ax
	mov ax, 0b800h
	mov es, ax
	
	call clear_Screen
	push [star_Count]
	Call put_star
	Call random
	
waitfordata:
	cmp [dir],48h
	je up
	cmp [dir],4bh
	je left
	cmp [dir],1
	je exit
continue:
	call delay
	mov ah,1
	int 16h
	je waitfordata
	mov ah,0
	int 16h
	mov [dir],ah
	jmp waitfordata
	
up:
	call move_Up
	push [star_Count]
	Call put_star
	jmp continue
	
left:
	call move_left
	push [star_Count]
	Call put_star
	jmp continue
	
exit:
	mov ax, 4c00h
	int 21h
END start



