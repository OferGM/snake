IDEAL
MODEL small
STACK 100h
DATASEG
    ;;;;;	CONSTANTS	;;;;;
        ;; colors ;;
        blue equ 1000h					            ; the color blue, the color of the snake's dots
        red equ 4000h					            ; the color red, the color of the apples
        black equ 0000h					            ; the color black, to clear the tail and the background

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
        speed dw 01aeh
        lost db 0                                   ; if player hit the border or its self
        preDir db ?                                 ; player's previous key input
        dir db ?                                    ; player's key input
        snakeLength dw 1                            ; the leangth of the snake
        applePos dw ?                               ; position of the apple on the video mode screen
        snakeArray dw 2000, 2000 dup(0)             ; position of the snake's dots on the video mode screen
CODESEG
    ;;;;;   <============================================================ Screen Control Procedures ============================================================>   ;;;;;
    ;$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$;
        
        ;===============================================================================================================================================================;;
        ;;  ----- a proc to clear the screen ----   ;;
        proc clearBackground
            push ax
            push cx
            push di

            mov cx, screenH * screenW
            mov ax, black
            xor di, di
            clearBackgroundLoop:
                stosw
                loop clearBackgroundLoop

            pop di
            pop cx
            pop ax
            ret
        endp clearBackground
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to draw the snake on the screen -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4] and of snakeLength [bp + 6] >>   ;
        proc drawSnake
            push bp
            mov bp, sp
            push ax
            push bx
            push cx
            push di
            
            ;; cx hold the total amount of members in the snakeArray, bx holds the offset of the head, di the pos of the head and ax the color blue.
            mov bx, [bp + 6]
            mov cx, [bx]
            mov bx, [bp + 4]
            mov ax, blue
            
            ;; a loop to draw every member on the screen
            draw:
                mov di, [bx]
                mov [es : di], ax
                add bx, 2
                loop draw

            pop di
            pop cx
            pop bx
            pop ax
            pop bp
            ret 4
        endp drawSnake
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to draw the apple on the screen -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6] and of applePos [bp + 8] >>   ;
        proc apple 
            push bp
            mov bp, sp
            push ax
            push bx
            push cx
            push dx
            push di

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
            mov bx, [bp + 6]
            mov cx, [bx]
            mov bx, [bp + 4]

            ;; compare the random number position to each member of the snakeArray, if the collide generate a new random number
            snakeCollision:
                cmp [bx], dx
                je randloop
                add bx, 2
                loop snakeCollision

            ;; ax holds the color of the apple and di it's pos, than place the apple on the screen
            mov ax, red
            mov di, dx
            mov [es : di], ax

            ;; saves the random number
            mov bx, [bp + 8]
            mov [bx], dx

            pop di
            pop dx
            pop cx
            pop bx
            pop ax
            pop bp
            ret 6
        endp apple
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to check if the apple and the head are in the same cords -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6], of applePos [bp + 8], and of speed [bp + 10] >>    ;
        proc appleCollision
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
            mov bx, [bp + 8]
            cmp [bx], ax
            jne appleCollisionRet

            ;; bx holds the offset the tail
            mov di, [bp + 6]
            mov bx, [di]
            sal bx, 1
            add bx, [bp + 4]
            sub bx, 2

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
                mov bx, [bp + 6]
                inc [byte ptr bx]
                push [bp + 6]
                push [bp + 4]
                call drawSnake

            ;; drawing a new apple
            push [bp + 8]
            push [bp + 6]
            push [bp + 4]
            call apple

            ;mov bx, [bp + 10]
            ;sub [word ptr bx], 1fh

            appleCollisionRet:
                pop di
                pop bx
                pop ax
                pop bp
                ret 8
        endp appleCollision
        ;===============================================================================================================================================================;

    ;$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$;
    ;;;;;   ============================================================> Screen Control Procedures <============================================================   ;;;;;
    
    ;;;;;   <============================================================= Move Related Procedures =============================================================>   ;;;;;
    ;$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure that calls other procedures in the case that the player didn't lose -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6], of applePos [bp + 8], and of speed [bp + 10] >>    ;
        proc ifDidNotLose
            push bp
            mov bp, sp

            push [bp + 6]
            push [bp + 4]
            call drawSnake

            push [bp + 10]
            push [bp + 8]
            push [bp + 6]
            push [bp + 4]
            call appleCollision

            push [bp + 10]
            call delay

            pop bp
            ret 8
        endp ifDidNotLose
        
        ;===============================================================================================================================================================;
        
        ;===============================================================================================================================================================;
        ;;   ----- a procedure that does a long loop to delay the DosBox -----   ;;
        ;   << gets as a parameter the offsets of speed [bp + 4] >>  ;
        proc delay
            push bp
            mov bp, sp
            push bx
            push cx
            
            ;; does 2 long loops
            mov bx, [bp + 4]
            mov cx, [bx]
            outer_loop:
                push cx
                mov cx, 500

                inner_loop:
                    loop inner_loop
                    pop cx
                    loop outer_loop

            pop cx
            pop bx
            pop bp
            ret 2
        endp delay
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to check if the head hit the body, if it did the player lost the game -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6] and of lost [bp + 8] >>  ;
        proc hitBody
            push bp
            mov bp, sp
            push ax
            push bx
            push cx

            ;; ax holds the pos of the head and cx the total amount of members in the snakeArray
            mov bx, [bp + 6]
            mov cx, [bx]
            mov bx, [bp + 4]
            mov ax, [bx]

            ;; a loop to check if any member of snakeArray collided with the head
            hitBodyLoop:
                add bx, 2
                cmp [bx], ax
                je headHitBody
                loop hitBodyLoop
                jmp retHitBody

            headHitBody:
                mov bx, [bp + 8]
                mov [byte ptr bx], 1

            retHitBody:
                pop cx
                pop bx
                pop ax
                pop bp
                ret 6
        endp hitBody
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to update the snake past the head from back to front and clear the tail -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4] and of snakeLength [bp + 6] >>   ;
        proc updateSnake
            push bp
            mov bp, sp
            push ax
            push bx
            push cx
            push di

            ;; ax holds the color of the background, bx the offset of the tail and di the pos of the tail. this block clears the tail
            mov ax, black
            mov di, [bp + 6]
            mov bx, [di]
            sal bx, 1
            add bx, [bp + 4]
            sub bx, 2
            mov di, [bx]
            mov [es : di], ax

            ;; switching every member with the member before them
            mov cx, [bp + 4]
            updateSnakeLoop:
                cmp bx, cx
                je retUpdateSnake
                mov ax, [bx - 2]
                mov [bx], ax
                sub bx, 2
                jmp updateSnakeLoop

            retUpdateSnake:
                pop di
                pop cx
                pop bx
                pop ax
                pop bp
                ret 4
        endp updateSnake
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to move up -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6] and of lost [bp + 8] >>   ;
        proc up
            push bp
            mov bp, sp
            push ax
            push bx
            push dx

            ;; updates snake
            push [bp + 6]
            push [bp + 4]
            call updateSnake
            mov bx, [bp + 4]
            sub [word ptr bx], screenW * 2

            ;; check if head collides with border
            mov ax, [bx]
            cmp ax, upBorder
            jg retUp
            mov bx, [bp + 8]
            mov [byte ptr bx], 1

            retUp:
                pop dx
                pop bx
                pop ax
                pop bp

                ret 6
        endp up
        ;===============================================================================================================================================================;
        
        ;===============================================================================================================================================================;
        ;;   ----- a procedure to move down -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6] and of lost [bp + 8] >>   ;
        proc down
            push bp
            mov bp, sp
            push ax
            push bx
            push dx
            
            ;; updates snake
            push [bp + 6]
            push [bp + 4]
            call updateSnake
            mov bx, [bp + 4]
            add [word ptr bx], screenW * 2

            ;; checks if head collides with border
            mov ax, [bx]
            cmp ax, downBorder
            jl retDown
            mov bx, [bp + 8]
            mov [byte ptr bx], 1

            retDown:
                pop dx
                pop bx
                pop ax
                pop bp
                ret 6    
        endp down
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to move right -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6] and of lost [bp + 8] >>   ;
        proc right
            push bp
            mov bp, sp
            push ax
            push bx
            push cx
            push dx

            ;; updates snake
            push [bp + 6]
            push [bp + 4]
            call updateSnake
            mov bx, [bp + 4]
            add [word ptr bx], 2

            ;; checks if head collides with border
            xor dx, dx
            mov cx, 160
            mov ax, [bx]
            div cx
            cmp dx, rightBorder
            jne retRight
            mov bx, [bp + 8]
            mov [byte ptr bx], 1
            
            retRight:
                pop dx
                pop cx
                pop bx
                pop ax
                pop bp
                ret 6
        endp right
        ;===============================================================================================================================================================;

        ;===============================================================================================================================================================;
        ;;   ----- a procedure to move left -----   ;;
        ;   << gets as a parameters the offsets of the head [bp + 4], of snakeLength [bp + 6] and of lost [bp + 8] >>   ;
        proc left
            push bp
            mov bp, sp
            push ax
            push bx
            push cx
            push dx

            ;; updates snake
            push [bp + 6]
            push [bp + 4]
            call updateSnake
            mov bx, [bp + 4]
            sub [word ptr bx], 2

            ;; checks if head collides with border
            xor dx, dx
            mov cx, 160
            mov ax, [bx]
            add ax, 2
            div cx
            cmp dx, leftBorder
            jne retLeft
            mov bx, [bp + 8]
            mov [byte ptr bx], 1

            retLeft:
                pop dx
                pop cx
                pop bx
                pop ax
                pop bp
                ret 6
        endp left
        ;===============================================================================================================================================================;

    ;$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$;
    ;;;;;   =============================================================> Move Related Procedures <=============================================================   ;;;;;
    main:
        ;; dummy proofs to ds and es
		mov ax, @data
		mov ds, ax
		mov ax, VIDMEM
		mov es, ax

        call clearBackground

        mov bx, offset snakeArray
        mov cx, offset snakeLength
        mov dx, offset applePos
        mov di, offset speed
        mov si, offset lost

        push cx
        push bx
        call drawSnake
        push dx
        push cx
        push bx
        call apple

        input:
            ;; reads an input from the player's keyboard, if no key was press comtinue in current direction
            mov ah,1
            int 16h
            jz waitfordata
            mov ah,0
            int 16h
            mov [dir],al
            
        waitfordata:
            mov bx, offset snakeArray
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
        
        opDirLabel:
            mov bl, [preDir]
            mov [dir], bl
            jmp input
        continue:
            ;; dir is now preDir
            mov bl, [dir]
            mov [preDir], bl
            
            ;; check lose conditions
            mov bx, offset snakeArray
            push si
            push cx
            push bx
            call hitBody
            cmp [lost], 1
            je exit

            ;; draw new snake, check apple and delay
            push di
            push dx
            push cx
            push bx
            call ifDidNotLose
            jmp input

        upLabel:
            cmp [preDir], 's'
            je opDirLabel
            mov [preDir], 'w'
            push si
            push cx
            push bx
            call up
            jmp continue
            
        downLabel:
            cmp [preDir], 'w'
            je opDirLabel
            mov [preDir], 's'
            push si
            push cx
            push bx
            call down
            jmp continue
            
        rightLabel:
            cmp [preDir], 'a'
            je opDirLabel
            mov [preDir], 'd'
            push si
            push cx
            push bx
            call right
            jmp continue
                   
        leftLabel:
            cmp [preDir], 'd'
            je opDirLabel
            mov [preDir], 'a'
            push si
            push cx
            push bx
            call left
            jmp continue

        exit:
            mov ax, terminate
            int 21h

    END main