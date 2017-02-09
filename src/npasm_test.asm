
; This is an example assembly file to test the assembler's parser.  Any odd 
; formatting may not be a mistake, but rather an intentional change to make
; sure it is handled properly.

; These should all be postive (top prevent double negatives, such as --5).
#define CONST_DEC    011   ; Looks like binary, but is decimal (no b suffix)
 #define CONST_HEX   1Fh
#define CONST_BIN   1011B

    ; Start with some code without a label.

    NOP
    NOP
    NOP

LABEL_START:

    WAK             ; Not needed since rst will clear this.
; test test
LABEL_DIRECT_ADDR:  ; Add stuff to mess up parser...

    ; Attempt all of the basic direct addressing mode syntax.

    ADD     A, AFh              ; Immediate value, hex
    ADC     B, 235              ; Immediate value, decimal
    SUB     A, +33              ; Immediate value, signed decimal
    SBB     B, -2               ; Immediate value, signed decimal
    AND     A, 101101b          ; Immediate value, binary

    OR      A, X[7,7]          ; Pixel register [row, col]
    XOR     B, Y[0,0]
    CMP     A, Z[3,1]
    LD      B, X[2,00100b]      ; Binary, < 8 bits, leading 0s
    ST      A, Y[0H,03h]

    ADD     A, X[63]            ; Pixel register [row*8 + col]
    ADC     B, Y[0]
    SUB     A, Z[25H] 
    SBB     B, Z[10011B]      ; Binary, fully specified.
    AND     A, Z[CONST_DEC]  

    OR      A, SR               ; Add status register to acc A
    XOR     B, A                ; Add acc A to acc A
    CMP     A, B                ; Add acc B to acc A
    LD      B, RCR             ; Add row column register to acc A
    ST      A, NW               ; Add top left/NW neighborhood register to acc A
    ADD     B, NE               ; Add top right/NE neighborhood register to acc A
    ADC     A, SW               ; Add bottom left/SW neighborhood register to acc A
    SUB     B, SE               ; Add bottom right/SE neighborhood register to acc A
          
LABEL_WITH_NUM_45:        


    ; Check the conversion from (r,c) to register index.

    ADC     A, X[0, 7]
    SUB     B, X[1,6]
    SBB     A, X[2,5]
    AND     B, X[3,4]
    OR      A, X[4,3]
    XOR     B, X[5,2]
    LD      A, X[6,1]
    ST      B, X[7,0]
                                RANDOM_LABEL:                    
    ADC     B, X[CONST_HEX]
    SUB     A, X[-CONST_HEX]; Checking if a comment will cause a parse error
    SBB     B, X[2,5]
    AND     A, X[3,CONST_BIN]
    OR      B, X[-CONST_BIN]
    XOR     A, SE
    LD      B, NW; Checking if a comment will cause a parse error

    NOP

LABEL_SHIFT:

    SRA     A
    SR      A
    SRC     A
    SL      A; Checking if a comment will cause a parse error
    SLC     A

    SRA     B
    SR      B
    SRC     B
    SL      B
    SLC     B                                                ; Checking if a comment will cause a parse error

    NOP

LABEL_JMP:

    JZ      LABEL_START
    JNZ     LABEL_DIRECT_ADDR
    JC      LABEL_SHIFT; Checking if a comment will cause a parse error
    JNC     RANDOM_LABEL
    JN      LABEL_SHIFT         ;;;;; Checking if a comment will cause a parse error
    JNN     LABEL_JMP
    JO      LABEL_SHORT_JUMP
    JMP     LABEL_WITH_NUM_45

LABEL_SHORT_JUMP:

    JMP     LABEL_SHORT_JUMP    ; jump to the same address.
           
LABEL_SLP:

    SZ 
    SNZ
    SC  ; Check if a comment will cause a parse error
    SNC
    SN ;;;; Checking if a comment will cause a parse error
    SNN
    SO 
    SLP
          
LABEL_INDIRECT_ADDR:

    ADD     A, [B]
    ADD     A, [B]
    ADC     A, [B]
    ADC     A, [B]
    SUB     A, [B]
    SUB     A, [B]
    SBB     A, [B]
    AND     A, [B]; comment
    OR      A, [B];comment
    OR      A, [B];comment
OR      A   , [B];comment
    OR      A, [B];comment
    XOR     A , [B];1_
    CMP     A,  [B]
    ST      A,[B]
    LD      A,    [B]

LABEL_OUT: 

    OUT     A, 0
    OUT     B, 1
    OUT     NW,2
    OUT     NE, 3
    OUT     SW  ,  4
    OUT     SE, 5
    OUT     [B], 6
    OUT     X[ 4 , 5 ] , 7
    OUT     Y[7,7],8
    OUT     Z[0,0], 9
    OUT     RCR, 10
    OUT     SR, 15
              
LABEL_OTHERS:
;comment
    RST
    NOP
    WAK;comment

lABEL_eNd:

    SLP
    RST



;comment
