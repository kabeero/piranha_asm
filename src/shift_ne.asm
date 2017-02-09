; shift_ne.asm
;
; Test program which moves data in and out of the neighborhood registers.

#define NUM_PXL 64

SHIFT_LOOP_INIT:

    LD      A, 0
    ST      A, NW           ; Initialize the local NR since it lacks a reset.
    LD      B, 0

SHIFT_LOOP:

    LD      A, [B]          
    ST      A, NE
    LD      A, SW
    ST      A, [B]

    ADD     B, 1
    CMP     B, NUM_PXL
    JNZ     SHIFT_LOOP

OUTPUT_LOOP_INIT:

    LD      B, 0

OUTPUT_REG_X_LOOP:

    OUT     [B], 0          ; Row 0
    OUT     [B], 1          ; Row 1
    OUT     [B], 2          ; Row 2
    OUT     [B], 3          ; Row 3
    OUT     [B], 4          ; Row 4
    OUT     [B], 5          ; Row 5
    OUT     [B], 6          ; Row 6
    OUT     [B], 7          ; Row 7
    
    ADD     B, 1
    CMP     B, NUM_PXL
    JNZ     OUTPUT_REG_X_LOOP
    SLP
