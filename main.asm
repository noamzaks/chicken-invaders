; PROJECT: Chicken Invaders in Turbo Assembly x8086
; AUTHOR: Noam Zaks
; DATE: January - May, 2020
; VERSION: 1.2


.model small
.stack 100h

.data

include data

.code
include support
include game
include menu

;Simply initializes the ds register
proc begin
    mov ax,@data
    mov ds,ax

    xor ax,ax
    ret
endp begin

proc main
    push ax

    mainstart:
        call menu
        call game
        mov al,1
        jmp mainstart
        
    pop ax
    ret
endp main

proc finish
    ;Clear the screen
    call videomode
    call clear
    call textmode
    call clearinput

    mov dx,0beefh ;Out of the screen
    call movecursor

    ;Display exit text
        mov dx,0
        mov cx,3
        call delay
        mov dx,0000h
        call movecursor
        mov dx,offset finishl1
        call printstring
        mov dx,0beefh ;Out of the screen
        call movecursor

        mov dx,0
        mov cx,7
        call delay
        mov dx,0100h
        call movecursor
        mov dx,offset finishl2
        call printstring
        mov dx,0beefh ;Out of the screen
        call movecursor

        mov dx,0
        mov cx,7
        call delay
        mov dx,0200h
        call movecursor
        mov dx,offset finishl3
        call printstring
        mov dx,0beefh ;Out of the screen
        call movecursor

    mov dx,0
    mov cx,7
    call delay

    mov dx,0200h
    call movecursor

    call clearinput

    mov ax,4c00h
    int 21h
    ret
endp finish

start:
    call begin
    call main
    call finish
    end start
