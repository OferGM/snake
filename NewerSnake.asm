IDEAL

MODEL small

STACK 100h

DATASEG

    snakeDots dw 2002, 2000, 1998

CODESEG
    
    proc clear      ;A proc to clear the screen

        push bp
        mov bp, sp

        push ax     ;ax = 0000h
        push di     ;di = 0000h

        lopClear:

            mov [es:di], ax
            inc di
            
            cmp di, 4000
            jnz lopClear

        pop di
        pop ax
        pop bp

        ret

    endp clear
    
    start:
        mov ax, @data
        mov ds, ax

        mov ax, 0b800h
        mov es, ax

        xor ax, ax
        xor di, di

        call clear

    exit:

        mov ax, 4c00h
        int 21h
    
    END start