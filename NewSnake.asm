IDEAL

MODEL small

STACK 100h

DATASEG
    ;;;;;	CONSTANTS	;;;;;
		;; colours ;;
		blue equ 1000h					            ; the colour blue, the colour of the snake's dots
		red equ 4000h					            ; the colour red, the colour of the apples
		black equ 0000h					            ; the colour black, to clear the tail and the background

		;; screen values ;;
		screenW equ 80			    	            ; the width of the screen
		screenH equ 25			    	            ; the length of the screen

		;; border values ;;
		upBorder equ 0000d				            ; the value of the first cell in the screen, if the snake's head is lower than that it had reached the up border
		downBorder equ 4000d			            ; the value of the last cell in the screen, if the snake's head is higher than that it had reached the down border
		rightBorder equ 0000d			            ; if the snake's head position is divided by the screen's width and the result is 0000 in ax it had reached the right border
		leftBorder equ 0000d			            ; if 2 is added to the position value of the head of the snake and divide by the screen's width it had reached the left border

		;; addresses ;;
		vidMem equ 0b800h				            ; the video mode's address in the exstra segmant 
		terminate equ 4c00h				            ; terminates the program and move the control back to the OS

		;; keys ;;
		escape equ 27d				            	; ASCII value of the escape key
	;;;;;	VARIABLES	;;;;;
        lost db 0                                   ; if player hit the border or its self
        dir db ?                                    ; player's key input
        snakeLeangth dw 2                           ; the leangth of the snake
        applePos dw ?                               ; position of the apple on the video mode screen
		snakeArray dw 2002, 2000, 1998, 2000 dup(0) ; position of the snake's dots on the video mode screen
CODESEG
    proc drawSnake
        ;;====================================================;;
        ; a procedure to draw the snake on the screen,         ;
        ; gets as a parameter the offset of the head [bp + 4]. ;
        ;;====================================================;;

        ;; pushing the base pointer and pointing it to it's self
        push bp
        mov bp, sp
        ;; saving used regs values
        push ax
        push bx
        push cx
        push di

        ;; bx holds the offset of the head
        mov bx, [bp + 4]

        ;; cx hold the total amount of members in the snakeArray
        mov cx, [snakeLeangth]
        inc cx
        
        ;; a loop to draw every member on the screen
        draw:
            mov di, [bx]
            mov ax, blue
            mov [es : di], ax
            add bx, 2
            loop draw

        pop di
        pop cx
        pop bx
        pop ax
        pop bp

        ret 2
    endp drawSnake

    proc apple
		;;============================================================================;;
        ; a procedure to place an apple in a random position in the video mode screen, ;
        ; gets as a parameter the offset of the head [bp + 4].                         ;
        ;;============================================================================;;

        ;; pushing the base pointer and pointing it to it's self 
        push bp
        mov bp, sp
		;; saving used reg values
		push ax
		push di

        ;; gets a random apple position
        mov ax, [bp + 4]
        push ax
		call rand

        ;; ax holds the colour of the apple and di it's pos, than place the apple on the screen
        mov ax, red
        mov di, [applePos]
		mov [es : di], ax

		pop di
		pop ax
        pop bp

		ret 2
	endp apple

    proc rand
		;;========================================================;;
        ; a procedure to get a random number in range of 0 - 2000, ;
        ; gets as a parameter the offset of the head [bp + 4].     ;
        ;;========================================================;;

        ;; pushing the base pointer and pointing it to it's self
        push bp
        mov bp, sp
		;; saving used regs values
		push ax
        push bx
		push cx
		push dx

        ;; get a random number via timer
		randloop:
            ;; get system time, cx : dx now hold number of clock ticks since midnight
			xor ax, ax
			int 1ah

            ;; ax holds the number of clock ticks, cx the range and dx the random number (the remainder of the tickes divided by the range)
			mov ax, dx
			xor dx, dx
			mov cx, screenH * screenW + 1
            inc cx
			div cx
			sal dx, 1

        ;; bx holds the offset of the head, cx the total amount of members in the snakeArray
        mov bx, [bp + 4]
        mov cx, [snakeLeangth]
        inc cx

        ;; compare the random number position to each member of the snakeArray, if the collide generate a new random number
        snakeCollision:
            cmp [bx], dx
            je randloop
            add bx, 2
            loop snakeCollision

        ;; saves the random number
		mov [applePos], dx

		pop dx
		pop cx
        pop bx
		pop ax
        pop bp

		ret 2
	endp rand

    proc appleCollision
		;;=================================================================;;
		; a procedure to check if the head is at the same pos as the apple, ;
		; gets as a parameter the offset of the head [bp + 4].              ;
		;;=================================================================;;

        ;; pushing the base pointer and pointing it to it's self
		push bp
		mov bp, sp
        ;; saving used regs values
		push ax
        push bx
        push di

        ;; check if the head collides with the apple, if not return
		mov bx, [bp + 4]
        mov ax, [bx]
		cmp [applePos], ax
		jne appleCollisionRet

        ;; bx holds the offset the tail
        mov bx, [snakeLeangth]
        sal bx, 1
        add bx, [bp + 4]

        ;; new tail pos is the old tail pos
        mov di, [bx]
        add bx, 2
        mov [bx], di

        ;; bx holds the offset of the member before the tail
        sub bx, 4
        
        ;; if the tail is above the member before it the new tail will be placed above the old tail
        add di, screenW * 2
        cmp [bx], di
        jne checkIfDown
        sub [word ptr bx + 4], 160
        jmp callDraw
        
        ;; if the tail is below the member before it the new tail will be placed below the old tail
        checkIfDown:
            mov di, [bx + 4]
            sub di, screenW * 2
            cmp [bx], di
            jne checkIfRight
            add [word ptr bx + 4], 160
            jmp callDraw

        ;; if the tail is right to the member before it the new tail will be placed right to the old tail
        checkIfRight:
            mov di, [bx + 4]
            sub di, 2
            cmp [bx], di
            jne newTailLeft
            add [word ptr bx + 4], 2
            jmp callDraw

        ;; if the tail is left to the member before it the new tail will be placed left to the old tail
        newTailLeft:
            sub [word ptr bx + 4], 2

        ;; drawing the new snake
        callDraw:
            inc [snakeLeangth]
            mov bx, [bp + 4]
            push bx
            call drawSnake

        ;; drawing a new apple
        push bx
		call apple

		appleCollisionRet:
			pop ax
            pop bx
            pop di
			pop bp

			ret 2
	endp appleCollision

    proc hitBody
        ;;==================================================================================;;
		; a procedure to check if the head hit the body, if it did the player lost the game, ;
		; gets as a parameter the offset of the head [bp + 4].                               ;
		;;==================================================================================;;
        
        ;; pushing the base pointer and pointing it to it's self
        push bp
        mov bp, sp
        ;; saving used regs values
        push ax
        push bx
        push cx

        mov bx, [bp + 4]
        mov ax, [bx]
        mov cx, [snakeLeangth]
        inc cx

        hitBodyLoop:
            add bx, 2
            cmp [bx], ax
            je headHitBody
            loop hitBodyLoop
            jmp retHitBody

        headHitBody:
            mov [lost], 1

        retHitBody:
            pop cx
            pop bx
            pop ax
            pop bp

            ret 2
    endp hitBody

    proc delay
        ;;====================================;;
        ; does a long loop to delay the dosbox ;
        ;;====================================;;

        ;; saving used reg value
        push cx
        
        ;; does 2 long loops
        mov cx, 02feh
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

    proc updateSnake
        ;;=================================================================;;
		; a procedure to update the snake past the head from back to front, ;
		; gets as a parameter the offset of the head [bp + 4].              ;
		;;=================================================================;;

        ;; pushing the base pointer and pointing it to it's self
        push bp
        mov bp, sp
        ;; saving used regs values
        push ax
        push bx
        push cx

        ;; ax holds the colour of the background, bx the offset of the tail and di the pos of the tail and than clear the tail
        mov ax, black
        mov bx, [snakeLeangth]
        sal bx, 1
        add bx, [bp + 4]
        mov di, [bx]
        mov [es : di], ax

        ;; cx hold the head offset
        mov cx, [bp + 4]
        
        ;; switching every member with the member before them
        updateSnakeLoop:
            cmp bx, cx
            je retUpdateSnake
            mov ax, [bx - 2]
            mov [bx], ax
            sub bx, 2
            jmp updateSnakeLoop

        retUpdateSnake:
            pop cx
            pop bx
            pop ax
            pop bp

            ret 2
    endp updateSnake

    proc up
        ;;==================================================;;
		; a procedure to move up                             ;
		; gets as parameter the offset of the head [bp + 4]. ;
		;;==================================================;;

        ;; pushing the base pointer and pointing it to it's self
        push bp
        mov bp, sp
        ;; saving used regs values
        push ax
        push bx
        push dx

        ;; check that the head doesn't colide with the body
		mov bx, [bp + 4]
		mov dx, [bx + 2]
		add dx, screenW * 2
		cmp [bx], dx
		je retUp

        ;; updates snake
        push bx
        call updateSnake

        sub [word ptr bx], screenW * 2

        ;; check if head collides with border
        mov ax, [bx]
        cmp ax, upBorder
        jg retUp
        mov [lost], 1

        retUp:
            pop dx
            pop bx
            pop ax
            pop bp

            ret 2
    endp up

    proc down
        ;;
		; a procedure to move down
		; gets as parameter the offset of the head [bp + 4].
		;;
        push bp
        mov bp, sp
        ;; saving used regs values
        push ax
        push bx
        push dx

        ;; check that the head doesn't colide with the body
		mov bx, [bp + 4]
		mov dx, [bx + 2]
		sub dx, screenW * 2
		cmp [bx], dx
		je retDown
        
        push bx
        call updateSnake

        add [word ptr bx], screenW * 2

        ;; checks if head collides with border
        mov ax, [bx]
        cmp ax, downBorder
        jl retDown
        mov [lost], 1

        retDown:
            pop dx
            pop bx
            pop ax
            pop bp

            ret 2    
    endp down

    proc right
        ;;
		; a procedure to move right
		; gets as parameter the offset of the head [bp + 4].
		;;
        push bp
        mov bp, sp
        ;; saving used regs values
        push ax
        push bx
        push cx
        push dx

        ;; check that the head doesn't colide with the body
		mov bx, [bp + 4]
		mov dx, [bx + 2]
		sub dx, 2
		cmp [bx], dx
		je retRight

        push bx
        call updateSnake

        add [word ptr bx], 2

        ;; checks if head collides with border
        xor dx, dx
        mov cx, 160
        mov ax, [bx]
        div cx
        cmp dx, rightBorder
        jne retRight
        mov [lost], 1
        
        retRight:
            pop dx
            pop cx
            pop bx
            pop ax
            pop bp

            ret 2
    endp right

    proc left
        ;;
		; a procedure to move left
		; gets as parameter the offset of the head [bp + 4].
		;;
        push bp
        mov bp, sp
        ;; saving used regs values
        push ax
        push bx
        push cx
        push dx

        ;; check that the head doesn't colide with the body
		mov bx, [bp + 4]
		mov dx, [bx + 2]
		add dx, 2
		cmp [bx], dx
		je retLeft

        push bx
        call updateSnake

        sub [word ptr bx], 2

        ;; checks if head collides with border
        xor dx, dx
        mov cx, 160
        mov ax, [bx]
        add ax, 2
        div cx
        cmp dx, leftBorder
        jne retLeft
        mov [lost], 1

        retLeft:
            pop dx
            pop cx
            pop bx
            pop ax
            pop bp

            ret 2
    endp left
    main:
		;; dummy proof ds
		mov ax, @data
		mov ds, ax
		
		;; dummy proof es
		mov ax, VIDMEM
		mov es, ax

		;; cx holds the total amount of cells in the screen, ax the colour of the background, di the starting pos
		mov cx, screenH * screenW
		mov ax, black
		xor di, di

		;; a loop that repeats 4000 times and clears each cell in the video mode screen
		clearLoop:
			mov [es : di], ax
			add di, 2
			loop clearLoop

        ;; pass to drawSnake and run it
        mov bx, offset snakeArray
        push bx
        call drawSnake

        ;; draw an apple on the screen
        push bx
        call apple
        jmp input

        waitfordata:
            cmp [dir], 'w'
            je upLabel
            cmp [dir], 's'
            je downLabel
            cmp [dir], 'd'
            je rightLabel
            cmp [dir], 'a'
            je leftLabel
            cmp [dir], escape
            je exit

        continue:
            push bx
            call hitBody
            cmp [lost], 1
            je exit

            push bx
            call drawSnake
            push bx
            call appleCollision

        ;; reads an input from the player's keyboard
        input:
            call delay
            mov ah,1
            int 16h
            jz waitfordata
            mov ah,0
            int 16h
            mov [dir],al
            jmp waitfordata
            
        upLabel:
            push bx
            call up
            jmp continue
            
        downLabel:
            push bx
            call down
            jmp continue
            
        rightLabel:
            push bx
            call right
            jmp continue
                   
        leftLabel:
            push bx
            call left
            jmp continue
            
        exit:
            mov ax, terminate
            int 21h

    END main