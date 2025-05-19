main proc far

mov al, 12h
mov ah, 0       ;set graphics video mode\
int 10h

mov ax, 1       ;shows mouse cursor
int 33h                              

Next:
mov ax, 3       ;get cursor position in cx,dx
int 33h

call putpix     
jmp Next

mov ah, 4ch
int 21h
main endp

;procedure to print
putpix proc
mov al, 4       ;color of pixel
mov ah, 0ch
shr cx, 1       ;cx will get double so we divide
int 10h         ;set pixel
ret
putpix endp

data segment
    grid db 9 dup(0)
    player db 0
    win db 0
    temp db 0
    timer db ""
    endgame db "would you like a rematch (y = yes )$"
    startmsg db "x and o game $"
    table db "---+---+---$"
    turnplace db "enter your move by the number$"
    turnX db "player X turns$"
    turnO db "player O turns$"
    tie db "TIE GAME!$"  
    winmessage db "winner winner chicken dinner !$"
    DigitError db "ilegal move$"
    inError db "this input is not a digit$"
    newline db 0Dh,0Ah,'$'              
ends
stack segment
    dw 128 dup(0)
ends
;create sound
in al, 61h
or al, 00000011b
out 61h, al
mov al, 0b6h
out 43h, al
mov ax, 2394h
out 42h, al
mov al, ah
out 42h, al
;stop sound:
in al, 61h
and al, 11111100b
out 61h, al

code segment
start:
    mov ax, data
    mov ds, ax
    mov es, ax
    
    newGame:
    call initiateGrid
    mov player, 10b         ;2dec
    mov win, 0
    mov cx, 9
    gameAgain:
        call clearScreen
        lea dx, startmsg
        call printString
        lea dx, newline
        call printString
        lea dx, turnplace
        call printString
        lea dx, newline
        call printString
        call printString
        call printGrid
        mov al, player
        cmp al, 1
        je p2turn
            ; previous player was 2
            shr player, 1       ;0010b  -- > 0001b
            lea dx, turnX
            call printString
            lea dx, newline
            call printString
            jmp endPlayerSwitch  
       p2turn:  ;previous player was 1
            shl player, 1       ;0001b --> 0010b
            lea dx, turnO
            call printString
            lea dx, newline
            call printString
            
      endPlayerSwitch:
      call getMove  ;bx will point to the right...
      mov dl, player
      cmp dl, 1
      jne p2move
      mov dl, 'X'
      jmp contMoves
      p2move:
      mov dl, 'O'
      contMoves:
      mov [bx], dl
      cmp cx, 5 ;before the 5th turn u don..
      jg noWinCheck
      call checkWin
      cmp win, 1
      je won
      noWinCheck:
      loop gameAgain
      
    ;tie cx = 0
    call clearScreen
    lea dx, startmsg
    call printString
    lea dx, newline
    call printString
    call printString
    call printString  
    call printGrid
    lea dx, tie
    call printString
    lea dx, newline
    call printString
    jmp askForNewGame  
    
    won:    
    call clearScreen
    lea dx, startmsg
    call printString
    lea dx, newline
    call printString
    call printString
    call printString  
    call printGrid
    lea dx, winmessage
    call printString
    mov dl, player
    mov dl, 'O'
    call putChar
    lea dx, newline
    call printString
    
    askForNewGame:
    lea dx, endgame ; ask for another game
    call printString
    lea dx, newline
    call printString
    call getChar
    cmp al, 'a'     ;play again if 'y' is pressed
    jne sof
    jmp newGame
                 
    sof:
    mov ax, 4c00h
    int 21h
    
;input char into sl sets ah = 1
getChar:
    mov ah, 01
    int 21h
    ret

;set ah=02
putChar:
    mov ah, 02
    int 21h
    ret
    
;set ah = 09
printString:
    mov ah, 09
    int 21h
    ret

;clear screen
;ah = 0 at the end
clearScreen:
    mov ah, 0fh
    int 10h
    mov ah, 0
    int 10h
    ret              

;get location that can be used
;al = hold the place number( 0 - 8)
;bx - will hold ethe position( bx[al])
getMove:
    call getChar    ;al = getchar()
    call isValidDigit
    cmp ah, 1
    je contCheckTaken
    mov dl, 0dh
    call putChar
    lea dx, inError
    call printString
    lea dx, newline
    call printString
    jmp getMove
    
    contCheckTaken:     ; check this: if(grid[al..
    lea bx, grid
    sub al, '1'      ; hoac la 'l'
    mov ah, 0
    add bx, ax
    mov al, [bx]
    cmp al, '9'
    jnp finishGetMove
    mov dl, 0dh
    call putChar
    lea dx, DigitError
    call printString
    lea dx, newline
    call printString
    jmp getMove
    finishGetMove:
    lea dx, newline
    call printString
    ret            
    
; initiates the grid from '1' to'9'
initiateGrid:
    lea bx, grid
    mov al, '1'
    mov cx, 9
    initNextTa:
    mov [bx], al
    inc al
    inc bx
    loop initNextTa
    ret
    
;checks if a char in al is a digit if is digit ...
isValidDigit:
    mov ah, 0
    cmp al, '1'
    jl sofIsDigit
    cmp al, '9'
    jg sofIsDigit
    mov ah, 1
    sofIsDigit:
    ret
    
;output the 3x3 gird
printGrid:
    lea bx, grid
    call printRow
    lea dx, table
    call printString
    lea dx, newline
    call printString
    call printRow
    lea dx, table
    call printString
    lea dx, newline
    call printString
    call printRow
    ret
    
;output a single row of the grid
;use bx as the first number in the row
;at the end:
;dl = third cell on row
printRow:
    ;first cell
    mov dl,' ' 
    call putChar
    mov dl, [bx]
    call putChar
    mov dl, ' ' 
    call putChar
    mov dl, '|'
    call putChar
    inc bx
    
    ;second cell
    mov dl,' ' 
    call putChar
    mov dl, [bx]
    call putChar
    mov dl, ' ' 
    call putChar
    mov dl, '|'
    call putChar
    inc bx
    
    ;third cell
    mov dl,' ' 
    call putChar
    mov dl, [bx]
    call putChar
    inc bx   
    
    lea dx, newline
    call printString
    ret
    
;return 1 in al if a player won
; 1 for win 0 for no win
;change bx
checkWin:
    lea si, grid
    call checkDiagonal
    cmp win, 1
    je endCheckWin
    call checkRows
    cmp win, 1
    je endCheckWin
    call CheckColumns
    endCheckWin:
    ret   
    
checkDiagonal:
    ;DiagonalLtR
    mov bx, si
    mov al, [bx]
    add bx, 4       ;grid[0] - grid[4]
    cmp al, [bx]
    jne diagonalRtL
    add bx, 4       ;grid[4] - grid[8]
    cmp al, [bx]
    jne diagonalRtL
    mov win, 1
    ret
    
    diagonalRtL:
    mov bx, si
    add bx, 2       ;grid[0] - grid[2]
    mov al, [bx]
    add bx, 2       ;grid[2] - grid[4]
    cmp al, [bx]
    jne endCheckDiagonal 
    add bx, 2       ;grid[4] - grid[6]
    cmp al, [bx]
    jne endCheckDiagonal
    mov win, 1
    endCheckDiagonal:
    ret

checkRows:
    ;first row
    mov bx, si  ;--> grid0
    mov al, [bx]
    inc bx      ;grid 0 --> grid 1
    cmp al, [bx]
    jne secondRow
    inc bx      ;grid 1 - grid 2
    cmp al, [bx]
    jne secondRow
    mov win, 1
    ret
    
    secondRow:
    mov bx, si  ;--> grid 0
    add bx, 3   ;grid 0 - grid 3
    mov al, [bx]
    inc bx      ;grid 3 - grid 4
    cmp al, [bx]
    jne thirdRow
    inc bx      ;grid 4 - grid 5
    cmp al, [bx]
    jne thirdRow
    mov win, 1
    ret
    
    thirdRow:
    mov bx, si  ;-->grid 0
    add bx, 6   ;grid 0 - grid 6
    mov al, [bx]  
    inc bx      ;grid 6 - grid 7
    cmp al, [bx]
    jne endCheckRows
    inc bx      ;grid 7 - grid 8
    cmp al, [bx]
    jne endCheckRows
    mov win, 1
    endCheckRows:
    ret
    
 checkColumns:
    ;first column
    mov bx, si  ;-->grid 0
    mov al, [bx]
    add bx, 3   ;grid 0 - grid 3
    cmp al, [bx] 
    jne secondColumn
    add bx, 3   ;grid 3 - grid 6
    cmp al, [bx]
    jne secondColumn
    mov win, 1
    ret
     
    secondColumn:
    mov bx, si  ;-->grid 0
    inc bx      ;grid 0 - grid 1
    mov al, [bx]
    add bx, 3   ;grid 1 - grid 4
    cmp al, [bx]
    jne thirdColumn
    add bx, 3   ;grid 4 - grid 7
    cmp al, [bx]
    jne thirdColumn
    mov win, 1
    ret
    
    thirdColumn:
    mov bx, si  ; -->grid 0
    add bx, 2   ;grid 0 - grid 2
    mov al, [bx]
    add bx, 3   ;grid 2 - grid 5
    cmp al, [bx]
    jne endCheckColumns
    add bx, 3   ; grid 5 - grid 8
    cmp al, [bx]
    jne endCheckColumns
    mov win, 1
    endCheckColumns:
    ret
    
    
ends
end start                    
                        
    