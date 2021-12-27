; PROJECT: Chicken Invaders in Turbo Assembly x8086
; AUTHOR: Noam Zaks
; DATE: January - May, 2020
; VERSION: 1.2


.model small
.stack 100h

.data

;Graphics
textcode equ 03h ;VGA code for text mode
textlocation equ 0b800h ;Location of screen in text mode

videocode equ 13h ;VGA code for video mode
videolocation equ 0a000h ;Location of screen in video mode

rowlength equ 320 ;Number of pixels in row (video)
columnlength equ 200 ;Number of pixels in column (video)

midrow equ rowlength/2 ;Number of pixels in half-row (video)
middle equ midrow+midrow*columnlength ;Location of middle of screen
;
;Colors
;Constants representing color by the 256 color table
black equ 0h
blue equ 1h
red equ 4h
bordo equ 0b8h
orange equ 2ah
brown equ 6h
white equ 0fh
grey equ 1ah
;
;Movements
up equ -rowlength ;Moving one pixel up
down equ rowlength ;Moving one pixel down
right equ 1 ;Moving one pixel right
left equ -1 ;Moving one pixel left
;
;Menu
highscore dw ?
playername db 5 dup(?),'$' ;if beat highscore

;Constant strings for presenting the user
;0ah - ASCII newline code
menuloading db 'Loading Chicken Invaders$'
menuwelcome db 'Welcome to Chicken Invaders$'
menuversion db 'Version 1.2 of May 15th, 2020$'
menuauthor db 'Created by Noam Zaks$'
menuplay db 'Start Playing$'
menureplay db 'Re-Play$'
menuquit db 'Exit Game$'
menuscore db ' Score: $'
menuspace db ' $'
menusettings db 'Options$'

menuupkey db 'Enter Preffered Up Key',0ah,'                         Default is W',0ah,'                         Current is $'
menuleftkey db 'Enter Preffered Left Key',0ah,'                         Default is A',0ah,'                         Current is $'
menudownkey db 'Enter Preffered Down Key',0ah,'                         Default is S',0ah,'                         Current is $'
menurightkey db 'Enter Preffered Right Key',0ah,'                         Default is D',0ah,'                         Current is $'
menuexitkey db 'Enter Preffered Exit Key',0ah,'                         Default is ESC$'
menushootkey db 'Enter Preffered Shoot Key',0ah,'                         Default is SPACE$'

;weird bug first E is not showing up
menushipcolor db 'EEnter Preffered Ship Color',0ah,'                         Default is RED',0ah,'                         1 - Green',0ah,'                         2 - Cyan',0ah,'                         3 - Red',0ah,'                         4 - Pink',0ah,'                         5 - Beige',0ah,'                         6 - Light Grey',0ah,'                         7 - Dark Grey',0ah,'                         8 - Light Blue',0ah,'                         9 - Light Green',0ah,'                         0 - Blue',0ah,'                         Current is $'
menudifficulty db 'Game Difficulty: ',0ah,'                         Default is MEDIUM',0ah,'                         0 - EASY',0ah,'                         1 - MEDIUM',0ah,'                         2 - HARD',0ah,'                         3 - EXTREME',0ah,'                         9 - SANDBOX',0ah,'                         Current is $'

;
;Input Keys
;The user's preffered keys to controlling the game
upkey db 'w'
leftkey db 'a'
downkey db 's'
rightkey db 'd'
shootkey db 20h
exitkey db 1bh
;
;Sounds
;frequency constants of game sounds
dartsound equ 5658h
eggsound equ 0500h
deathsound equ 1658h
soundtimer db 0
;
;Game
difficulty db 1 ;0 - Easy, 1 - Medium, 2 - Hard, 3 - Extreme, 9 - Sandbox
score dw 0
scorestring db 6 dup(0),'$' ;Score as a string to print
life db 5
;
;Ship
shiplocation dw middle
previousship dw 0
shipcolor db red
shipcornertotop equ -6*rowlength+12 ;Difference in locations between its corner (shiplocation) to its top point
;
;Darts
dartlocations dw 50 dup(0)
dartcount equ 50
;
;Chickens
chickenlocations dw 20 dup(0)
chickendeltas dw 20 dup(0) ;Difference in location between the default starting position of the chicken path and the start position of the selected chicken, this is so that the chickens aren't all in the same position
previousdelta dw 3 ;for better random
previousrandom db 0 ;for better random
chickencount equ 20
chickenpath dw middle/2+rowlength/4+10*left,middle/2+rowlength/4+9*left+up,middle/2+rowlength/4+8*left+2*up,middle/2+rowlength/4+7*left+3*up,middle/2+rowlength/4+6*left+4*up,middle/2+rowlength/4+5*up+5*left,middle/2+rowlength/4+4*up+4*left,middle/2+rowlength/4+3*up+3*left,middle/2+rowlength/4+3*up+3*left,middle/2+rowlength/4+2*up+2*left,middle/2+rowlength/4+up+left,middle/2+rowlength/4,middle/2+rowlength/4,middle/2+rowlength/4,middle/2+rowlength/4,middle/2+rowlength/4,middle/2+rowlength/4,middle/2+rowlength/4
chickenpathcount equ 18
chickenpathcounters dw 20 dup(0)
chickenspeed db 0 ;0 through 7
chickenspawned db 0 ;Has a chicken already spawned this second?
chickentoptobottom equ 18*rowlength ;Difference in location between the top (chickenlocation) to the bottom
;
;Eggs
egglocations dw 50 dup(0)
eggcount equ 50
eggspeed db 0 ;0 through 45
eggspawned db 0 ;Has an egg already spawned this second?
;
;Finish
finishl1 db 'Chicken Invaders Finished Successfully.$'
finishl2 db 'Thank you for playing!$'
finishl3 db 'Created by Noam Zaks in 2020.$'
;

.code
;Graphics
    ;Changes the VGA setting to video
    proc videomode
        push ax

        mov ax,videocode
        int 10h
        mov ax,videolocation
        mov es,ax

        pop ax
        ret
    endp videomode

    ;Changes the VGA setting to text
    proc textmode
        push ax

        mov ax,textcode
        int 10h
        mov ax,textlocation
        mov es,ax

        pop ax
        ret
    endp textmode

    ;Clears the screen in text mode
    proc clear
        push ax
        push cx
        push si

        mov cx,rowlength*columnlength
        mov si,0
        mov al,black
        videoclearloop:
            mov es:[si],ax
            inc si
            loop videoclearloop

        pop si
        pop cx
        pop ax
        ret
    endp clear

    ;Draws a line in video mode
    proc line
        push cx

        mov ah,al
        lineloop:
            mov es:[si],ax
            add si,bx
            loop lineloop
        sub si,bx ;One too much

        pop cx
        ret
    endp line

;Files
    ;Gets the highscore from the file
    proc gethighscore
        ret
    endp gethighscore

    ;Writes the highscore to the file
    proc sethighscore
        ret
    endp sethighscore
;
;Other
    ;Inputs and interprets input from the user
    proc input

        mov bx,0
        ;Any key pressed
            mov ah,1h
            int 16h
            jz inputret
        ;
        ;Which key pressed
            mov ah,0
            int 16h

            cmp al,upkey
            je inputup
            cmp al,leftkey
            je inputleft
            cmp al,downkey
            je inputdown
            cmp al,rightkey
            je inputright
            cmp al,exitkey
            je inputquit
            cmp al,shootkey
            je inputshoot
        ;
        ;Ineffective key pressed
        jmp inputret

        inputup:
            mov bx,up
            mov ax,0
            jmp inputret
        ;
        inputleft:
            mov bx,left
            mov ax,0
            jmp inputret
        ;
        inputdown:
            mov bx,down
            mov ax,0
            jmp inputret
        ;
        inputright:
            mov bx,right
            mov ax,0
            jmp inputret
        ;
        inputret:
            ret
        ;
        inputshoot:
            mov ax,1
            mov bx,0
            jmp inputret
        ;
        inputquit:
            call finish
            jmp inputret
    endp input

    ;Initializes the mouse
    proc initializemouse
        push ax
        push dx

        ;Initialize Mouse
        mov ax,0h
        int 33h

        ;Show Mouse
        mov ax,1h
        int 33h

        pop dx
        pop ax
        ret
    endp initializemouse

    ;Moves the cursor
    proc movecursor
        push ax
        push bx
        push dx

        mov ah,2
        mov al,0
        mov bx,0
        int 10h

        pop dx
        pop bx
        pop ax
        ret
    endp movecursor

    ;Turns the score 0153 to a string "0153$"
    proc makestring
        push ax
        push bx
        push cx
        push di

        mov ax,score
        mov di,3
        mov cx,4
        mov bl,10
        scorestringloop:
            div bl ;remainder at ah
            add ah,30h
            mov scorestring[di],ah
            mov ah,0
            dec di
            loop scorestringloop
        mov di,4
        mov bl,'$'
        mov scorestring[di],bl
        scorestringret:
            pop di
            pop cx
            pop bx
            pop ax
            ret
    endp makestring

    ;Prints a string
    proc printstring
        push ax

        mov ah,9
        int 21h

        pop ax
        ret
    endp printstring

    ;Waits before continuing to the next instruction
    proc delay
        push ax

        mov ah,86h
        int 15h

        pop ax
        ret
    endp delay

    ;Makes a sound
    proc sound
        push ax

        in al,61h
        or al,11b
        out 61h,al

        mov al,0b6h
        out 43h,al

        mov al,bl
        out 42h,al
        mov al,bh
        out 42h,al

        pop ax
        ret
    endp sound

    ;Stops a sound playing
    proc stopsound
        push ax

        in al,61h
        and al,11111100b
        out 61h,al

        pop ax
        ret
    endp stopsound

    ;Clears the input buffer
    proc clearinput
        push ax

        clearinputloop:
            ;Check if key available
                mov ah,1
                int 16h
                jz clearinputret
            ;
            ;Empty key
                mov ah,0
                int 16h
            ;
            jmp clearinputloop

        clearinputret:
            pop ax
            ret
    endp clearinput

;General
    ;Initializes the game variables
    proc initializegame
        push ax
        push di
        push si

        call clearinput
        call videomode
        call clear

        ;Reset variables
            mov ax,0
            mov score,ax
        ;
        ;Reset locations
            mov cx,50
            mov di,0
            initializedarts:
                mov dartlocations[di],ax
                add di,2
                loop initializedarts

            mov cx,20
            mov di,0
            initializechickens:
                mov chickenlocations[di],ax
                add di,2
                loop initializechickens

            mov cx,50
            mov di,0
            initializeeggs:
                mov egglocations[di],ax
                add di,2
                loop initializeeggs
        ;
        ;Set starting locations
            mov si,middle
            mov shiplocation,si
        ;
        mov di,0
        mov chickenlocations[di],si

        ;Check difficulty
        cmp difficulty,0
        je initializegameeasy
        cmp difficulty,2
        je initializegamehard
        cmp difficulty,3
        je initializegameextreme1
        cmp difficulty,9
        je initializegamesandbox1

        jmp initializegamemedium ;Default

        initializegameret:
            pop si
            pop di
            pop ax
            ret

        initializegameeasy:
            mov al,5
            mov life,al

            mov chickenspeed,0
            mov eggspeed,0

            jmp initializegameret
        ;
        initializegamemedium:
            mov al,4
            mov life,al

            add di,2
            mov si,50*right
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,50*left
            mov chickendeltas[di],si
            mov chickenlocations[di],1

            mov chickenspeed,2
            mov eggspeed,10

            jmp initializegameret
        ;
        initializegameextreme1: jmp initializegameextreme
        initializegamesandbox1: jmp initializegamesandbox

        initializegamehard:
            mov al,3
            mov life,al

            add di,2
            mov si,35*right
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,35*left
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,70*right
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,70*left
            mov chickendeltas[di],si
            mov chickenlocations[di],1

            mov chickenspeed,4
            mov eggspeed,20

            jmp initializegameret
        ;
        initializegameextreme:
            mov al,1
            mov life,al

            add di,2
            mov si,35*right
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,35*left
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,70*right
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,70*left
            mov chickendeltas[di],si
            mov chickenlocations[di],1

            add di,2
            mov si,100*down
            mov chickendeltas[di],si
            mov chickenlocations[di],1


            add di,2
            mov si,35*right+100*down
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,35*left+100*down
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,70*right+100*down
            mov chickendeltas[di],si
            mov chickenlocations[di],1
            add di,2
            mov si,70*left+100*down
            mov chickendeltas[di],si
            mov chickenlocations[di],1


            mov chickenspeed,6
            mov eggspeed,30

            jmp initializegameret
        ;
        initializegamesandbox:
            mov al,1
            mov life,al

            mov chickenspeed,0
            mov eggspeed,0

            jmp initializegameret
    endp initializegame

    ;Runs the game
    proc game
        push ax
        push bx
        push cx
        push dx

        call initializegame
        mov ax,1 ;Game Loop Counter

        gameloop:
            call beforectr
            call control

            call clear
            
            call show
            call afterctr

            call drawscore

            inc ax ;Game Counter

            cmp difficulty,9 ;Sandbox
            je gameloop

            mov bl,life
            cmp bl,0
            jle gameret

            call collusions
            cmp dl,1 ;Ship hits chicken
            je gameret

            jmp gameloop
        ;
        gameret:
            call videomode
            call clear
            mov bx,deathsound
            call sound

            mov dx,0
            mov cx,7
            call delay
            call stopsound
            pop dx
            pop cx
            pop bx
            pop ax
            ret
    endp game

    ;Shows all the game sprites
    proc show
        push ax
        push cx
        push di
        push si

        mov di,0
        mov al,shipcolor
        mov cx,50
        showdartloop:
            mov si,dartlocations[di]
            cmp si,0
            je dontshowdart
            call drawdart

            dontshowdart:
            add di,2
            loop showdartloop


        mov di,0
        mov al,grey
        mov cx,50
        showeggloop:
            mov si,egglocations[di]
            cmp si,0
            je skipegg
            call drawegg
            skipegg: add di,2
            loop showeggloop

        mov si,shiplocation
        call drawship

        mov al,blue
        mov di,0
        mov cx,20
        showchickenloop:
            mov si,chickenlocations[di]
            cmp si,0
            je dontshowchicken
            call drawchicken
            dontshowchicken:
            add di,2
            loop showchickenloop
        ;
        mov cx,0
        add cl,life
        cmp cx,0
        jle dontshowlife
        mov si,5*rowlength+8
        showlifeloop:
            call drawlife
            add si,15
            loop showlifeloop
        dontshowlife:

        pop si
        pop di
        pop cx
        pop ax
    endp show

    ;Changes the sprite locations
    proc control
        push ax
        push bx
        push cx
        push dx

        push ax
        push cx
        push dx
        mov ax,2c00h
        int 21h
        mov previousrandom,dl
        pop dx
        pop cx
        pop ax

        mov dx,0
        mov cx,0100h
        div cx
        cmp dx,0
        jne nomorespeed

        mov al,chickenspeed
        cmp al,7
        jge nomorechickenspeed
        inc al
        mov chickenspeed,al

        nomorechickenspeed:

        mov ah,0
        mov al,eggspeed
        cmp al,45
        jge nomorespeed
        inc al
        mov eggspeed,al

        nomorespeed:

        call input
        call controlship
        call controldarts
        call controleggs
        call controlchickens
        call controlsound

        pop dx
        pop cx
        pop bx
        pop ax
        ret
    endp control

    ;Checks for all sprite collusions
    proc collusions
        call chickendart
        call shipegg

        cmp difficulty,9 ;Sandbox
        je collusionsret

        call shipchicken

        collusionsret: ret
    endp collusions
;
;Ship
    ;Shows a ship
    proc drawship
        push si
        push cx
        push bx
        push ax

        mov cx,3
        mov al,shipcolor

        drawshiparcs:
            push cx
            push si

            mov cx,7
            mov bx,up+2*right
            call line
            mov bx,down+2*right
            call line

            pop si
            pop cx
            add si,up
            loop drawshiparcs

        pop ax
        pop bx
        pop cx
        pop si
        ret
    endp drawship

    ;Moves the ship location
    proc controlship
        push ax
        push dx
        push si

        push bx

        mov si,shiplocation

        ;I have a need
        ;A need for SPEED
        add si,bx
        add si,bx ;i am SPEED
        add si,bx

        cmp si,rowlength*20 ;Upper bound
        jb controlshipreset
        cmp si,rowlength*200 ;Lower bound
        ja controlshipreset

        mov bx,rowlength
        mov dx,0
        mov ax,si
        div bx

        pop bx
        push bx

        ;Location in row is in dx
        cmp dx,10
        jb controlshipreset ;Left bound
        cmp dx,rowlength-40
        ja controlshipreset ;Right bound

        controlshipret:
            mov shiplocation,si

            pop bx
            pop si
            pop dx
            pop ax
            ret

        controlshipreset:
            sub si,bx
            sub si,bx ;i am not speed
            sub si,bx
            jmp controlshipret
    endp controlship
;
;Darts
    ;Shows a dart
    proc drawdart
        push bx
        push cx

        mov cx,5
        mov bx,up
        call line

        pop cx
        pop bx
        ret
    endp drawdart

    ;Changes all the dart locations
    proc controldarts
        push cx
        push di
        push si

        mov cx,50
        mov di,0
        controldartsloop:
            mov si,dartlocations[di]
            call limitdart
            mov dartlocations[di],si
            add di,2
            loop controldartsloop

        cmp ax,1 ;Shoot request
        je shootdarts
        controldartsret:
            pop si
            pop di
            pop cx
            ret
        shootdarts:
            call newdart
            jmp controldartsret
    endp controldarts

    ;Changes a single dart location
    proc limitdart

        cmp si,0
        je limitdartret
        cmp si,rowlength*5
        jb limitdartreset
        add si,up

        limitdartret: ret
        limitdartreset:
            mov si,0
            jmp limitdartret
    endp limitdart

    ;Creates a new dart to the dart array
    proc newdart
        push cx
        push di
        push si

        mov di,0
        mov cx,50
        newdartloop:
            mov si,dartlocations[di]
            cmp si,0
            je newdarthere
            add di,2
            loop newdartloop

        newdartret:
            pop si
            pop di
            pop cx
            ret

        newdarthere:
            mov si,shiplocation
            add si,shipcornertotop
            mov dartlocations[di],si
            jmp newdartret
    endp newdart
;
;Chickens
    ;Shows a chicken pixel by pixel (no good way to do this)
    proc drawchicken
        push si
        push ax

        mov ah,al
        mov es:[si],ah
        inc si
        mov es:[si],ah
        mov ah,white
        add si,down
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        sub si,3
        mov es:[si],ah
        mov ah,orange
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov ah,grey
        mov es:[si],ah
        sub si,3
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+left
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down+left
        mov ah,al
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down+left
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down
        mov ah,white
        mov es:[si],ah
        mov ah,al
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov ah,white
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down+right
        mov ah,orange
        mov es:[si],ah
        inc si
        mov ah,white
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov ah,orange
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si, down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        sub si,7
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,up+left
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,up
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        ;starting wings
        add si,6*up+3*left
        mov ah,white
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,up+left
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,up
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,up+2*right
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,up+2*right
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,up+right
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,up+left
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,up
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,up+2*left
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,up+3*left
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        ;starting right wing
        add si,18
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si, down
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down+2*left
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+3*right
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down+right
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+2*left
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down+4*left
        mov es:[si],ah
        dec si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        inc si
        mov es:[si],ah
        add si,down
        mov es:[si],ah
        dec si
        mov es:[si],ah
        pop ax
        pop si
        ret
    endp drawchicken

    ;Changes all chicken locations
    proc controlchickens
        push ax
        push cx
        push di


        mov cx,20
        mov di,0
        controlchickensloop:
            mov ax,chickenlocations[di]
            cmp ax,0
            je chickendontchange

            call limitchicken
            mov chickenlocations[di],ax

            chickendontchange:
            add di,2
            loop controlchickensloop
        ;
        ;Check if it is time to spawn chicken
            mov ax,2c00h
            int 21h ;dh - seconds

            mov ax,0
            add al,dh ;ax - seconds

            mov cl,10
            sub cl,chickenspeed ;Faster if more divisible
            div cl ;ah - seconds/5 remainder

            cmp ah,0
            je controlchickensnew
        ;
        ;Not time to spawn chicken
        mov chickenspawned,ah
        ;
        controlchickensret:
            pop di
            pop cx
            pop ax
            ret
        ;
        controlchickensnew:
            mov al,chickenspawned
            cmp al,1
            je controlchickensret ;Chicken already spawned this second

            mov al,1
            mov chickenspawned,al
            call newchicken
            jmp controlchickensret
    endp controlchickens

    ;Changes a single chicken's location
    proc limitchicken
        push bx

        mov bx,chickenpathcounters[di]
        inc bx
        mov chickenpathcounters[di],bx
        cmp bx,chickenpathcount
        ja limitchickenreset
        add bx,bx ;array of words
        mov ax,chickenpath[bx]

        limitchickensret:
            add ax,chickendeltas[di]
            pop bx
            ret
        limitchickenreset:
            mov ax,middle/2+rowlength/4
            jmp limitchickensret

    endp limitchicken

    ;Adds a new chicken to the chicken array
    proc newchicken
        push ax
        push bx
        push cx
        push dx
        push di

        mov di,0
        mov cx,20
        newchickenloop:
            mov si,chickenlocations[di]
            cmp si,0
            je newchickhere
            add di,2
            loop newchickenloop

        newchickret:
            pop di
            pop dx
            pop cx
            pop bx
            pop ax
            ret

        newchickhere:
            mov chickenpathcounters[di],0
            ;
            ;Calculate chicken delta - using linear congruntial generator
            ;This works by taking the previous random - x
            ;Multiplying it by some arbitrary a
            ;Adding to it an arbitrary b
            ;Then taking the remainder by some arbitrary c
            ;For more information, visit https://en.wikipedia.org/wiki/Linear_congruential_generator
            ;For examples and explanation, head to the README.md file
                mov ax,previousdelta

                mov cx,1574
                mul cx

                add cx,2630

                mov cx,3339
                div cx

                mov ax,dx

                sub ax,8175

                mov previousdelta,ax

                ;Check if place on line is good - not on edge
                mov bx,ax ;backup
                mov dx,0
                mov cx,rowlength
                div cx
                mov ax,bx ;restore
                cmp dx,rowlength-30
                ja chickendeltadontfix
                add ax,160
                chickendeltadontfix:

            mov cx,chickenpath[0]
            add cx,ax
            mov chickenlocations[di],cx
            mov chickendeltas[di],ax
            jmp newchickret
    endp newchicken
;
;Eggs
    ;Shows an egg
    proc drawegg
        push ax
        push bx
        push cx
        push si

        mov cx,5
        mov bx,down
        call line

        pop si
        pop cx
        pop bx
        pop ax
        ret
    endp drawegg

    ;Changes all egg locations
    proc controleggs
        push ax
        push cx
        push dx
        push di
        push si

        mov cx,50
        mov di,0
        controleggsloop:
            mov si,egglocations[di]
            call limitegg
            mov egglocations[di],si
            add di,2
            loop controleggsloop
        ;
        ;Check if it is time to spawn egg
            mov ax,2c00h
            int 21h ;dl - seconds/100
            mov ax,0
            add al,dl ;ax - seconds/100

            mov cl,50
            sub cl,eggspeed
            div cl ;ah - random number from 0 to 49

            cmp ah,0
            je controleggsnew
        ;
        mov ah,0
        mov eggspawned,ah
        controleggsret:
            pop si
            pop di
            pop dx
            pop cx
            pop ax
            ret
        ;
        controleggsnew:
            mov ah,eggspawned
            cmp ah,1
            je controleggsret
            mov ah,1
            mov eggspawned,ah
            push ax
            mov di,0
            mov cx,20
            controleggsnewloop:
                mov ax,chickenlocations[di]
                call newegg
                add di,2
                loop controleggsnewloop

            pop ax
            jmp controleggsret

    endp controleggs

    ;Changes a single egg's location
    proc limitegg

        cmp si,0
        je limiteggret
        cmp si,rowlength*195
        ja limiteggreset
        add si,down

        limiteggret:
            ret
        limiteggreset:
            mov si,0
            jmp limiteggret
    endp limitegg

    ;Adds a new egg to the egg array
    proc newegg
        push si
        push di
        push cx

        mov si,ax
        cmp si,0
        je neweggret
        mov di,0
        mov cx,50
        neweggloop:
            mov si,egglocations[di]
            cmp si,0
            je newegghere
            add di,2
            loop neweggloop

        neweggret:
            pop cx
            pop di
            pop si
            ret

        newegghere:
            mov si,ax
            add si,chickentoptobottom
            mov egglocations[di],si
            jmp neweggret
    endp newegg
;
;Collusions
    ;Checks for chicken-dart collusions
    proc chickendart
        push ax
        push bx
        push cx
        push dx
        push si
        push di

        mov cx,20
        mov di,0
        chickenhitsloop:
            mov ax,chickenlocations[di]
            cmp ax,0
            je skipdarthits
            push cx

            mov cx,50
            mov si,0
            darthitsloop:
                push cx
                mov bx,dartlocations[si]
                cmp bx,0
                je chickendonthit

                ;Difference (nonnegative)
                    sub ax,bx
                    cmp ax,0
                    jg chickendonthit
                    neg ax
                    chickendartnoneg:

                ;Row difference - ax, Location in row difference - dx
                    mov cx,rowlength
                    mov dx,0
                    div cx

                cmp ax,25
                ja chickendonthit

                cmp dx,rowlength-15
                ja chickenhit

                cmp dx,15
                jb chickenhit

                jmp chickendonthit

                chickenhit:
                    mov ax,0
                    mov dartlocations[si],ax
                    mov chickenlocations[di],ax

                    mov bx,dartsound
                    call sound
                    mov al,5 ;fast sound
                    mov soundtimer,al

                    mov ax,score
                    inc ax
                    mov score,ax
                ;
                chickendonthit:
                pop cx
                add si,2
                loop darthitsloop

            pop cx
            skipdarthits: add di,2
            loop chickenhitsloop

        chickenhitsdartret:
            pop di
            pop si
            pop dx
            pop cx
            pop bx
            pop ax
            ret
    endp chickendart

    ;Checks for ship-egg collusions
    proc shipegg
        push ax
        push bx
        push cx
        push di
        push si

        mov di,0
        mov cx,50
        shiphitseggloop:
            mov si,egglocations[di]
            cmp si,0
            je shiphitseggloopskip

            mov ah,es:[si]
            cmp ah,shipcolor
            je egghits

            shiphitseggloopskip: add di,2
            loop shiphitseggloop
        shiphitseggret:
            pop si
            pop di
            pop cx
            pop bx
            pop ax
            ret

        egghits:

            mov bx,eggsound
            call sound
            mov bl,1
            mov soundtimer,bl

            mov ax,0
            mov egglocations[di],ax

            cmp difficulty,9 ;Sandbox
            je shiphitseggret

            mov al,life
            dec al
            mov life,al

            jmp shiphitseggret
    endp shipegg

    ;Checks for ship-chicken collusions
    proc shipchicken
        push ax
        push bx
        push cx
        push si

        mov cx,3
        mov si,shiplocation
        shipchickenloop:
            push cx
            push si

            mov cx,7
            shipchickencheck1:
                mov al,es:[si]
                cmp al,shipcolor
                je shipchickencheck1continue
                cmp al,black
                je shipchickencheck1continue
                cmp al,grey ;egg
                je shipchickencheck1continue

                jmp shiphitschicken

                shipchickencheck1continue:
                add si,up+2*right
                loop shipchickencheck1
            ;
            mov cx,7
            sub si,up+2*right
            ;
            shipchickencheck2:
                mov al,es:[si]
                cmp al,shipcolor
                je shipchickencheck2continue
                cmp al,black
                je shipchickencheck2continue
                cmp al,grey
                je shipchickencheck2continue

                jmp shiphitschicken

                shipchickencheck2continue:
                add si,down+2*right
                loop shipchickencheck2
            ;
            pop si
            pop cx
            add si,up
            loop shipchickenloop
        ;
        shipchickenret:
            pop si
            pop cx
            pop bx
            pop ax
            ret
        ;
        shiphitschicken:
            mov dl,1 ;Quit Game
            jmp shipchickenret
    endp shipchicken

;
;Visualizations
    ;Shows one heart symbol pixel by pixel(no good way to do this)
    proc drawlife
        push ax
        push si

        ;mov al,white
        ;add si,2*right
        ;mov es:[si],al
        ;add si,right
        ;mov es:[si],al
        ;add si,right
        ;mov es:[si],al
        ;add si,down+right
        ;mov es:[si],al
        ;add si,down+right
        ;mov es:[si],al
        ;add si,up+right
        ;mov es:[si],al
        ;add si,up+right
        ;mov es:[si],al
        ;add si,right
        ;mov es:[si],al
        ;add si,right
        ;mov es:[si],al
        ;add si,down+right
        ;mov es:[si],al
        ;add si,down+right
        ;mov es:[si],al
        ;add si,down
        ;mov es:[si],al
        ;add si,down
        ;mov es:[si],al
        ;add si,down
        ;mov es:[si],al
        ;add si,down+left
        ;mov es:[si],al
        ;add si,down+left
        ;mov es:[si],al
        ;add si,down+left
        ;mov es:[si],al
        ;add si,down+left
        ;mov es:[si],al
        ;add si,down+left
        ;mov es:[si],al
        ;add si,down+left
        ;mov es:[si],al
        ;add si,up+left
        ;mov es:[si],al
        ;add si,up+left
        ;mov es:[si],al
        ;add si,up+left
        ;mov es:[si],al
        ;add si,up+left
        ;mov es:[si],al
        ;add si,up+left
        ;mov es:[si],al
        ;add si,up+left
        ;mov es:[si],al
        ;add si,up
        ;mov es:[si],al
        ;add si,up
        ;mov es:[si],al
        ;add si,up
        ;mov es:[si],al
        ;add si,up+right
        ;mov es:[si],al

        mov al,red
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,4*right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov al,bordo
        mov es:[si],al
        add si,down+right
        mov es:[si],al
        add si,down
        mov es:[si],al
        add si,down
        mov es:[si],al
        add si,down
        mov es:[si],al
        add si,down+left
        mov es:[si],al
        add si,down+left
        mov es:[si],al
        add si,down+left
        mov es:[si],al
        add si,down+left
        mov es:[si],al
        add si,down+left
        mov es:[si],al
        mov al,red
        add si,up
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,up+left
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,up+right
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,up+left
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,up+right
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov al,white
        mov es:[si],al
        add si,left
        mov al,red
        mov es:[si],al
        add si,up
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,up
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov es:[si],al
        add si,left
        mov al,white
        mov es:[si],al
        mov al,red
        add si,left
        mov es:[si],al
        add si,up
        mov es:[si],al
        add si,right
        mov al,white
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov al,red
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,2*right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al
        add si,right
        mov es:[si],al




        pop si
        pop ax
        ret
    endp drawlife

    ;Shows the score
    proc drawscore
        push dx


        call makestring
        mov dx,0201h
        call movecursor
        mov dx,offset scorestring
        call printstring

        pop dx
        ret
    endp drawscore

;
;Sound
    ;Stops the sound after a bit of time
    proc controlsound
        push ax

        mov al,soundtimer
        cmp al,0
        je controlsoundret
        inc al
        mov soundtimer,al
        cmp al,10
        jl controlsoundret
        mov al,0
        mov soundtimer,al
        call stopsound
        controlsoundret:
            pop ax
            ret
    endp controlsound
;
;Double Buffering
    ;I am clearing the screen then showing all sprites where they should be
    ;By using double buffering, I don't show this as a process of clearing then showing, 
    ;But rather I am clearing then showing in another location
    ;Then copying that location to the screen.

    ;Runs before the control and makes it so that the es doesn't change the screen, so that there is no jitter
    proc beforectr
        push ax

        mov ax,1574h
        mov es,ax

        pop ax
        ret
    endp beforectr

    ;Runs after the control and copies to the screen
    proc afterctr
        push ax
        push cx
        push si
        push ds

        ;This code is taken from https://stackoverflow.com/questions/30326895/asm-8086-free-segment
        ;I did not create it nor do I take any responsibility or credit for it
        ;I thank the creator very much
        mov ax,1574h
        mov ds,ax
        mov di,0
        mov ax,videolocation
        mov es,ax
        mov si,0
        mov cx,8000h
        rep movsw

        pop ds
        pop si
        pop cx
        pop ax
        ret
    endp afterctr
;Main
proc menu
    push ax
    push bx
    push cx
    push dx

    call clearinput

    cmp al,1 ;After screen
    je menudontload
    ;Loading Screen
        call menuload

        menudontload:
        call clear
        call textmode
        call menutext

        call initializemouse
    ;
    menuloop: ;Wait for user to choose option
        mov ax,120
        mov bx,105
        call checkforclick
        cmp cl,1
        je menuret

        mov ax,140
        mov bx,125
        call checkforclick
        cmp cl,1
        je mmenuquit

        mov ax,160
        mov bx,145
        call checkforclick
        cmp cl,1
        jne dontcallsettings
        call mmenusettings
        dontcallsettings:
        jmp menuloop
    ;
    menuret:
        pop dx
        pop cx
        pop bx
        pop ax
        ret
    ;
    mmenuquit: ;Exit game
        call finish
        jmp menuret
endp menu

;A loading screen for the menu
proc menuload
    push ax
    push cx
    push dx
    push si

    call videomode
    call clear

    mov dx,0
    mov cx,3
    call delay

    mov dx,0a08h
    call movecursor
    mov dx,offset menuloading
    call printstring

    mov dx,0
    mov cx,3
    call delay

    mov al,blue
    mov si,middle
    call drawchicken

    mov dx,0
    mov cx,3
    call delay

    add si,80*down
    sub si,shipcornertotop
    mov al,orange
    call drawship
    add si,shipcornertotop
    add si,10*up

    mov dx,0
    mov cx,3
    call delay

    mov cx,4
    loadingdartloop:
        push cx

        add si,4*down
        mov al,black
        call drawdart
        mov al,orange
        add si,7*up
        call drawdart
        mov dx,0
        mov cx,2
        call delay

        pop cx
        loop loadingdartloop

    pop si
    pop dx
    pop cx
    pop ax
    ret
endp menuload

;Checks for a mouse click in a specific row and column
proc checkforclick
    push dx
    push ax
    push bx


    ;Get mouse
        mov ax,3
        int 33h

    ;Check mouse clicks
        and bx,11h
        jz checkforclicknotclicked

    ;Check location of click
        pop bx
        pop ax
        push ax
        push bx

        cmp dx,ax
        ja checkforclicknotclicked
        cmp dx,bx
        jb checkforclicknotclicked

    ;Mouse is clicked
    mov cl,1

    checkforclickret:
        pop bx
        pop ax
        pop dx
        ret

    checkforclicknotclicked:
        mov cl,0
        jmp checkforclickret
endp checkforclick

;Displays the menu's text
proc menutext
    push bx
    push cx
    push dx
    push si

    push ax
    ;Draw frame
        mov cx,17
        mov si,1984
        mov al,4
        mov bx,2
        call line
        add si,2
        mov cx,9
        mov bx,160
        call line

        mov cx,19
        mov bx,-2
        call line

        mov cx,9
        mov bx,-160
        call line
    pop ax

    cmp al,1
    je menutextaftergame

    ;Before Game
        mov dx,061bh ;Row 6 Column 25
        call movecursor
        mov dx,offset menuwelcome
        call printstring

        mov dx,0819h ;Row 8 Column 25
        call movecursor
        mov dx,offset menuversion
        call printstring

        mov dx,0a1fh ;Row 10 Column 25
        call movecursor
        mov dx,offset menuauthor
        call printstring

        mov dx,0e22h ;Row 14 Column 25
        call movecursor
        mov dx,offset menuplay
        call printstring

        mov dx,1024h ;Row 16 Column 25
        call movecursor
        mov dx,offset menuquit
        call printstring

        mov dx,1225h ;Row 18 Column 25
        call movecursor
        mov dx,offset menusettings
        call printstring

        jmp menutextret
    ;
    menutextaftergame:
        mov dx,0c22h ;Row 12 Column 25
        call movecursor
        mov dx,offset menuscore
        call printstring
        mov dx,offset scorestring
        call printstring
        mov dx,offset menuspace
        call printstring

        mov dx,0e25h ;Row 14 Column 25
        call movecursor
        mov dx,offset menureplay
        call printstring

        mov dx,1024h ;Row 16 Column 25
        call movecursor
        mov dx,offset menuquit
        call printstring

        mov dx,1225h ;Row 18 Column 25
        call movecursor
        mov dx,offset menusettings
        call printstring
    ;
    menutextret:
        ;Hide cursor
            mov dx,0beefh ;Out of the screen ;D
            call movecursor
        ;
        pop si
        pop dx
        pop cx
        pop bx
        ret
endp menutext

;Shows available options to the user
proc mmenusettings
    push ax
    push bx
    push cx
    push dx

    call videomode
    call clear
    call textmode

    ;Ask for input keys
        mov dx,offset menuupkey
        mov bl,upkey
        sub bl,20h ;Capitalize
        call menuquestions
        mov upkey,al

        call videomode
        call clear
        call textmode
        mov dx,offset menuleftkey
        mov bl,leftkey
        sub bl,20h ;Capitalize
        call menuquestions
        mov leftkey,al

        mov dx,1
        mov cx,0
        call delay
        call videomode
        call clear
        call textmode
        mov bl,downkey
        sub bl,20h ;Capitalize
        mov dx,offset menudownkey
        call menuquestions
        mov downkey,al

        mov dx,1
        mov cx,0
        call delay
        call videomode
        call clear
        call textmode
        mov dx,offset menurightkey
        mov bl,rightkey
        sub bl,20h ;Capitalize
        call menuquestions
        mov rightkey,al

        mov dx,1
        mov cx,0
        call delay
        call videomode
        call clear
        call textmode
        mov dx,offset menushootkey
        mov bl,shootkey
        call menuquestions
        mov shootkey,al

        mov dx,1
        mov cx,0
        call delay
        call videomode
        call clear
        call textmode
        mov dx,offset menuexitkey
        mov bl,exitkey
        call menuquestions
        mov exitkey,al

        mov dx,1
        mov cx,0
        call delay
        call videomode
        call clear
        call textmode
        mov dx,offset menushipcolor
        mov bl,shipcolor
        add bl,30h
        dec bl ;Started from 0
        call menuquestions
        sub al,30h ;'0'=30h
        inc al ;Started from 0
        mov shipcolor,al

        mov dx,1
        mov cx,0
        call delay
        call videomode
        call clear
        call textmode
        mov dx,offset menudifficulty
        mov bl,difficulty
        add bl,30h
        call menuquestions
        sub al,30h ;'0'=30h
        mov difficulty,al

    mov dx,1
    mov cx,0
    call delay
    call videomode
    call clear
    call textmode
    mov al,1
    mov score,0
    call makestring
    call menutext
    call initializemouse

    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp mmenusettings

;Shows a question in the menusettings, and asks for 1 key input
proc menuquestions

    call clearinput

    push dx
    mov dx,0819h ;Row 8 Column 25
    call movecursor
    pop dx

    call printstring
    mov dl,bl ;Display bl character
    mov ah,2
    int 21h

    mov dx,0beefh ;Out of the screen
    call movecursor

    mov ah,0
    int 16h ;Get key

    ret
endp menuquestions


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
