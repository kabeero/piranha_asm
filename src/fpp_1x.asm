; Custom naming for some instructions to make code more readable.

#define     JAE         JNN         ; Jump Above or Equal - unsigned
#define     JB          JNN         ; Jump Below - unsiged
#define     JE          JZ          ; Jump Equal

; Program constants.

#define     NUM_PXL     64
#define     NUM_ROWS    8
#define     NUM_COLS    8

#define     ROW_LEN     NUM_COLS
#define     COL_LEN     NUM_ROWS

#define     COL_MASK    00000111b
#define     ROW_MASK    00111000b

#define     REG_X_MASK  00111111b   ; Used to prenent (r,c) indices from leaving reg x.

; The starting values/locations when doing the diagnal operations.
#define     DIAG_INIT_INDEX     9
#define     DIAG_INIT_ROW       1
#define     DIAG_INIT_COL       1

; Given a current center pixel (C), index offset constants to reach adjacent
; pixels in each of the cardinal directions.

#define     OFF_N       -8
#define     OFF_S       8
#define     OFF_W       -1
#define     OFF_E       1

#define     OFF_NW      -9
#define     OFF_NE      -7
#define     OFF_SW      7 
#define     OFF_SE      9

; Program variables and their register assignment.

#define     index       B

#define     row         z[0]
#define     col         z[1]

#define     initial_col           z[4]
#define     temp0                 z[5]
#define     temp1                 z[6]

Start_Of_Program:

;-------------------------------------------------------------------------------
; 
; Focal Plane Prediction Algorithm.  One level.
;
;-------------------------------------------------------------------------------

Begin_FPP:

    LD      A, 0                ; Initialize the this NP's local NR.  This 
    ST      A, NW               ;  prevents uninitialized values from being used
                                ;  on the perimeter of the NP array.

;-------------------------------------------------------------------------------
;
; Unsigned to signed conversion.  Converts unsigned 8 bit grayscale to 8 bit
; signed values.  This reduces the output resolution by 1 bit.
;
;-------------------------------------------------------------------------------

Convert_To_Signed:

    LD      B, 0

Conversion_Loop:

    LD      A, [B]              ; This is rA[B] in the memory map.
    SR      A                   ; Make all pxls positive, MSbit=0.
    ST      A, [B]
    ADD     B, 1                ; Move to the next pxl.
    CMP     B, NUM_PXL 
    JNZ     Conversion_Loop
             
;-------------------------------------------------------------------------------
;
; Horizontal Residuals
;
;-------------------------------------------------------------------------------

Horizontal_Intra_PE:

    LD      A, 0
    ST      A, row
    LD      A, 1
    ST      A, col
    LD      B, 1

    Horizontal_i_While:
    
        LD      A, row
        CMP     A, 8
        JNN     Horizontal_i_While_End      ; JMP if (i - 8 >= 0) <=> (i >= 8)

        Horizontal_j_While:
        
            LD      A, col
            CMP     A, 8
            JNN     Horizontal_j_While_End

            ; Check if this a boundary case, with no pxl to the E.
            
            Horizontal_Check_Boundary_If:
        
                LD      A, col
                CMP     A, 7
                JNZ     Horizontal_Interior
                ;JMP     Horizontal_Boundary

            Horizontal_Boundary:

                SUB     B, 7                    ; Move to the 1st pxl in the row.
                LD      A, [B]                  ; Share it with the adj NP.
                ST      A, NW               
                LD      A, NE                   ; A = E.

                ADD     B, 6                    ; Move to the pxl to the W of target.
                ADD     A, [B]                  ; A = W + E.

                ADD     B, 1                    ; Return to C.
                JMP     Horizontal_Find_Residue

            Horizontal_Interior:
                
                SUB     B, 1                    ; Move to the pxl to the W.
                LD      A, [B]                  ; A = W.

                ADD     B, 2                    ; Move to the pxl to the E.
                ADD     A, [B]                  ; A = W + E.

                SUB     B, 1                    ; Return to C.
                JMP     Horizontal_Find_Residue

            Horizontal_Find_Residue:
                
                SR      A                       ; A = (1/2)(W + E).
                SUB     A, [B]                  ; A = (1/2)(W + E) - C.
                ST      A, [B]                  ; rA[B] = residual.

            Horizontal_Next_Pxl:

                LD      A, col
                ADD     A, 2
                ST      A, col
                ADD     B, 2
                JMP     Horizontal_j_While      ; Restart loop.

        Horizontal_j_While_End:
    
            LD      A, row
            ADD     A, 2
            ST      A, row
            LD      A, 1
            ST      A, col
            ADD     B, 8                        ; Skip 1 row.
            JMP     Horizontal_i_While          ; Restart loop.


    Horizontal_i_While_End:

Horizontal_Finished:
                 
;-------------------------------------------------------------------------------
;
; Vertical Residuals
;
;-------------------------------------------------------------------------------

Vertical_Intra_PE:

    LD      A, 1
    ST      A, row
    LD      A, 0
    ST      A, col
    LD      B, 8

    Vertical_i_While:
    
        LD      A, row
        CMP     A, 8
        JNN     Vertical_i_While_End      ; JMP if (i - 8 >= 0) <=> (i >= 8)

        Vertical_j_While:
        
            LD      A, col
            CMP     A, 8
            JNN     Vertical_j_While_End

            ; Check if this a boundary case, with no pxl to the E.
            
            Vertical_Check_Boundary_If:
        
                LD      A, row
                CMP     A, 7
                JNZ     Vertical_Interior
                ;JMP     Vertical_Boundary

            Vertical_Boundary:

                SUB     B, 56                   ; Move to the first pxl in the col.
                LD      A, [B]                  ; Share it with the adj NP.
                ST      A, NW               
                LD      A, SW                   ; A = S

                ADD     B, 48                   ; Move to the pxl to the N of target.
                ADD     A, [B]                  ; A = N + S.

                ADD     B, 8                    ; Return to C.
                JMP     Vertical_Find_Residue

            Vertical_Interior:
                
                ADD     B, 8                    ; Move to the pxl to the S.
                LD      A, [B]                  ; A = S.

                SUB     B, 16                   ; Move to the pxl to the N.
                ADD     A, [B]                  ; A = N + S.

                ADD     B, 8                    ; Return to C.
                JMP     Vertical_Find_Residue

            Vertical_Find_Residue:
                
                SR      A                       ; A = (1/2)(W + E).
                SUB     A, [B]                  ; A = (1/2)(W + E) - C.
                ST      A, [B]                  ; rA[B] = residual.

            Vertical_Next_Pxl:

                LD      A, col
                ADD     A, 2
                ST      A, col
                ADD     B, 2
                JMP     Vertical_j_While      ; Restart loop.

        Vertical_j_While_End:
    
            LD      A, row
            ADD     A, 2
            ST      A, row
            LD      A, 0
            ST      A, col
            ADD     B, 8                        ; Skip 1 row.
            JMP     Vertical_i_While          ; Restart loop.


    Vertical_i_While_End:

Vertical_Finished:
                      
;-------------------------------------------------------------------------------
;
; Diagnal Residuals
;
;-------------------------------------------------------------------------------

Diag_Intra_PE:

    LD      A, DIAG_INIT_INDEX
    LD      index, A
    LD      A, DIAG_INIT_ROW
    ST      A, row
    LD      A, DIAG_INIT_COL
    ST      A, col
    ST      A, initial_col

Diag_Row_While:

    LD      A, row
    CMP     A, NUM_ROWS
    JAE     Diag_Row_While_End

Diag_Col_While:

    LD      A, col
    CMP     A, NUM_COLS
    JAE     Diag_Col_While_End

Diag_Check_If_Last_Row_Or_Col:

    LD      A, row
    CMP     A, {NUM_ROWS - 1}
    JE      Diag_Last_Row_True_Check_If_Last_Col     ; See if it is also the last col
    JMP     Diag_Last_Row_False_Check_If_Last_Col    ; Not in the last row, so check to see if it is the last column in general.

Diag_Last_Row_True_Check_If_Last_Col:
             
    LD      A, col
    CMP     A, {NUM_COLS - 1}
    JE      Diag_SE_Corner          ; Pxl is at the SE corner.
    JMP     Diag_S_Edge             ; Working with pxl in last row.
                                               
Diag_Last_Row_False_Check_If_Last_Col:
             
    LD      A, col
    CMP     A, {NUM_COLS - 1}
    JE      Diag_E_Edge             ; Pxl is in the last column.
    JMP     Diag_Interior           ; Pxl is in the interior, so no special handling req'd.

Diag_Interior:     
     
    ADD     index, OFF_NW           ; Move to the NW pixel from the center.
    LD      A, [index]              ; A = NW
    ADD     index, {2 * OFF_SE}     ; Move to the SE pixel from the center.
    ADD     A, [index]              ; A = NW + NE
    SR      A                       ; A = (1/2)(NW + SE)
    ST      A, temp0                ; temp0 = (1/2)(NW + SE)

    ADD     index, {2 * OFF_W}      ; Move to the SW pixel.
    LD      A, [index]              ; A = SW
    ADD     index, {2 * OFF_NE}     ; Move to the NE pixel from the center.
    ADD     A, [index]              ; A = SW + NE
    SR      A                       ; A = (1/2)(SW + NE)
    ADD     A, temp0                ; A = (1/2)(NW + SE + SW + NE)
    SR      A                       ; A = (1/4)(NW + SE + SW + NE)

    ADD     index, OFF_SW           ; Return from the SE to the center pixel.
    SUB     A, [index]              ; A = (1/4)(NW + SE + SW + NE) - C
    ST      A, [index]              ; Store the new value.
    JMP     Diag_Next_Pxl
   
Diag_SE_Corner:

    ; This code is only reached while doing the diagnol portion of the 
    ; algorithm.  First, the value of the next pixel to the SE is obtained 
    ; through the neighborhood registers.  Since this is a unique case, the 
    ; center pixel is know to the be the far SE pixel, index 63.

    ; The next pxl is in index 0 of the SE NP.  This means we need to share our 
    ; pxl 0 (SE) with the NP to the NW, in turn receiving this value from the SE 
    ; NP.  The same goes for pixels 6 (SW) and 48 (NE).  The pixel at 54 (NW) is
    ; inside the local NP.

    LD      index, 0                ; Share the SE pixel with the adjacent NP.
    LD      A, [index]
    ST      A, NW               
    LD      A, SE                   ; A = SE
    ST      A, temp0                ; temp0 = SE

    LD      index, 54               ; The NW pixel
    LD      A, [index]              ; A = NW
    ADD     A, temp0                ; A = NW + SE
    SR      A                       ; A = (1/2)(NW + SE)
    ST      A, temp0                ; temp0 = (1/2)(NW + SE)
     
    LD      index, 6                ; Share the SW pixel with the adjacent NP.
    LD      A, [index]              
    ST      A, NW               
    LD      A, SW                   ; A = SW
    ST      A, temp1                ; temp1 = SW

    LD      index, 48               ; Share the NE pixel with the adjacent NP.
    LD      A, [index]              
    ST      A, NW
    LD      A, NE                   ; A = NE
    ADD     A, temp1                ; A = NE + SW
    SR      A                       ; A = (1/2)(NE + SW)
    ADD     A, temp0                ; A = (1/2)(NW + SE + NE + SW)
    SR      A                       ; A = (1/4)(NW + SE + NE + SW)
                                              
    LD      index, 63               ; Return to the center pixel.
    SUB     A, [index]              ; A = (1/4)(NW + NE + SW + SE) - C
    ST      A, [index]              ; Store the new value.
    JMP     Diag_Next_Pxl

Diag_E_Edge:

    ; This code can be reach while doing the horizontal or diagnal portions of the 
    ; algorithm.  It must be generic enough to serve poth cases, thus variable 
    ; offsets are used.
           
    ADD     index, OFF_NW               ; Move to the NW pixel
    LD      A, [index]                  ; A = NW
    ST      A, temp0                    ; temp0 = NW
    ADD     index, {2*OFF_S + 6*OFF_W}  ; Move to the SE pixel
    LD      A [index]                   ; Get SE pxl read for W NP
    ST      A, NW
    LD      A, NE                       ; A = SE
    ADD     A, temp0                    ; A = NW + SE
    SR      A                           ; A = (1/2)(NW + SE)
    ST      A, temp0                    ; temp0 = (1/2)(NW + SE)

    ADD     index, {2*OFF_N}            ; Go up two rows to NE element.
    LD      A, [index]                  ; Get NE pxl ready for W NP.
    ST      A, NW
    LD      A, NE                       ; A = NE
    ST      A, temp1                    ; temp1 = NE

    ADD     index, {2*OFF_S + 6*OFF_E}  ; Move to the SW pxl.
    LD      A, [index]                  ; A = SW
    ADD     A, temp1                    ; A = NE + SW
    SR      A                           ; A = (1/2)(NE + SW)
    ADD     A, temp0                    ; A = (1/2)(NW + SE + NE + SW)
    SR      A                           ; A = (1/4)(NW + SE + NE + SW)

    ADD     index, OFF_NE               ; Move back to center pxl.
    SUB     A, [index]                  ; A = (1/4)(NW + SE + NE + SW) - C
    ST      A, [index]                  ; Store the new value.
    JMP     Diag_Next_Pxl   


Diag_S_Edge:

    ; This code can be reach while doing the vertical or diagnal portions of the 
    ; algorithm.  It must be generic enough to serve poth cases, thus variable 
    ; offsets are used.
                
    ADD     index, OFF_NW               ; Move to the NW pixel
    LD      A, [index]                  ; A = NW
    ST      A, temp0                    ; temp0 = NW
    ADD     index, {2*OFF_E + 6*OFF_N}  ; Get the SE pxl ready for the N NP.
    LD      A, [index]
    ST      A, NW
    LD      A, SW                       ; A = SE
    ADD     A, temp0                    ; A = NW + SE
    SR      A                           ; A = (1/2)(NW + SE)
    ST      A, temp0                    ; temp0 = (1/2)(NW + SE)

    ADD     index, {2*OFF_W}            ; Get the SW pxl ready for the N NP.
    LD      A, [index]                  ; A = SW
    ST      A, NW
    LD      A, SW                       ; A = SW
    ST      A, temp1                    ; temp1 = SW
    ADD     index, {2*OFF_E + 6*OFF_S}  ; Move to the NE pxl.
    LD      A, [index]                  ; A = NE
    ADD     A, temp1                    ; A = NE + SW
    SR      A                           ; A = (1/2)(NE + SW)
    ADD     A, temp0                    ; A = (1/2)(NW + SE + NE + SW)
    SR      A                           ; A = (1/4)(NW + SE + NE + SW)

    ADD     index, OFF_SW               ; Return to the center pxl.
    SUB     A, [index]                  ; A = (1/4)(NW + SE + NE + SW) - C
    ST      A, [index]                  ; Store the new value.
    JMP     Diag_Next_Pxl   

Diag_Next_Pxl:

    ADD     index, 2                ; Move index two pxls right.

    LD      A, col                  ; Update col position.   
    ADD     A, 2                    
    ST      A, col

    JMP     Diag_Col_While          ; Restart loop.

Diag_Col_While_End:

    ; At this stage of the loop, the index has been moved to the "next" pixel, 
    ; which wrapped back the the start of the next row.  Every other row is 
    ; skipped in the 1st order FPP, so we need to move an additional row down.  
    ; The col counter needs reset as well.

    ADD     index, ROW_LEN              ; Move 1 pxl down, skipping row.

    LD      A, row                      ; Update row row position.
    ADD     A, 2
    ST      A, row

    AND     A, initial_col              ; Return to start column. 
    ST      A, col

    JMP     Diag_Row_While              ; Restart loop.

Diag_Row_While_End:

Diag_Finished:

;-------------------------------------------------------------------------------
;
; Output loop
;
;-------------------------------------------------------------------------------

; Output the x data on the column buses.

OUTPUT_REG_X:

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
    CMP     B, 64
    JNZ     OUTPUT_REG_X_LOOP
    SLP
                                       
