--------------------------------------------------------------------------------
-- Generated on 09/22/2014 at 23:43:00
-- Command: ./npasm -alr -o out src/no-img-histogram.npasm
--------------------------------------------------------------------------------
library ieee;
library vision;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use vision.pkg_basic.all;
use vision.pkg_np.all;
use vision.pkg_gcu.all;

entity rom is
    generic (
        DATA_WIDTH : natural := 16;
        ADDR_WIDTH : natural := 10);
    port ( 
        a    : in  gcu_addr_t;
        q    : out gcu_instr_t);
end rom;

architecture behavioral of rom is

    type rom_t is array (0 to 2**ADDR_WIDTH-1) of gcu_instr_t;
    constant rom : rom_t := (
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | ; no-img-histogram.npasm
                                    -- | ;
                                    -- | ; Used to measure power and cycle counts for paper writing.  It does not take 
                                    -- | ; images and only performs the algorithm.
                                    -- | ;
                                    -- | ; Computes the histogram for a full image.  This process is distributed across 
                                    -- | ; individual NPs and then their results are summed along the rows and columns
                                    -- | ; to form the final histogram.  In the end, NPs in row 0 contain the column
                                    -- | ; histogram bins in z[0..7] and NPs in column 0 contain the row histogram bins 
                                    -- | ; in z[8..15].
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
                                    -- | ; Define new instruction labels to make the code intuitive to read.
                                    -- | 
                                    -- | #define PXL_ROW_MASK        00111000b   ; The row bits for pixels
                                    -- | #define PXL_COL_MASK        00000111b   ; The column bits for pixels
                                    -- | 
                                    -- | #define NP_ROW_MASK         11110000b   ; The row bits for NPs in RCR
                                    -- | #define NP_COL_MASK         00001111b   ; The column bits for NPs in RCR
                                    -- | 
                                    -- | #define PXL_EDGE_N          00000000b   ; Matches pixels on N edge of NP
                                    -- | #define PXL_EDGE_S          00111000b   ; Matches pixels on S edge of NP
                                    -- | #define PXL_EDGE_W          00000000b   ; Matches pixels on W edge of NP
                                    -- | #define PXL_EDGE_E          00000111b   ; Matches pixels on E edge of NP
                                    -- |  
                                    -- | #define START_OF_X          00000000b   ; Starting address of X RAM.
                                    -- | #define START_OF_Y          01000000b   ; Starting address of Y RAM.
                                    -- | #define START_OF_Z          10000000b   ; Starting address of X RAM.
                                    -- | #define START_OF_V          11000000b   ; Starting address of V RAM.
                                    -- |  
                                    -- | #define TRUE                 1  ; True pattern
                                    -- | #define FALSE                0  ; False pattern
                                    -- | #define NULL                -1  ; Null pattern
                                    -- | 
                                    -- | #define NUM_PIXELS  64          ; The number of pixels in the NP.
                                    -- | #define MASK_AF     01000000b   ; Mask for ADC active flag
                                    -- | 
                                    -- | ; Algorithm constants.
                                    -- | 
                                    -- | #define HIST_BIN_MASK       00111111b   ; Isolates bin portion of hist address
                                    -- | #define HIST_NP_MASK        11000000b   ; Isolates NP portion of hist address
                                    -- | #define HIST_NP_00          00000000b   ; Matches NW NP, bins 0..63
                                    -- | #define HIST_NP_01          01000000b   ; Matches NE NP, bins 64..127
                                    -- | #define HIST_NP_10          10000000b   ; Matches SW NP, bins 128..191
                                    -- | #define HIST_NP_11          11000000b   ; Matches SE NP, bins 192..255
                                    -- | 
                                    -- | ; Algorithm variables.
                                    -- | 
                                    -- | #define intensity           v0          ; The current pixel intensity being binned
                                    -- | #define hist_assignment     v1          ; Allows each NP to know its bin range
                                    -- | #define index               v2          ; The current pixel index being binned
                                    -- | #define index_y             v3          ; index, but with y offset
                                    -- | #define index_z             v4          ; index, but with z offset
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
                                    -- |  
                                    -- | START:
                                    -- |  
                                    -- | ; Zero all of Y and Z memory where the histograms are stored.
                                    -- |          
                                    -- | CLEAR_HIST:
                                    -- | 
                                    -- | CLEAR_HIST_INIT:
                                    -- | 
           0 => "0100000101000000", -- |     LDR     r0, START_OF_Y
           1 => "0100001100000000", -- |     LDR     r1, 0
                                    -- | 
                                    -- | CLEAR_HIST_LOOP:
                                    -- | 
           2 => "0100101011110000", -- |     STR     r1, [r0]
           3 => "0000000100000001", -- |     ADD     r0, 1
           4 => "0011100111000000", -- |     CMP     r0, {START_OF_Y + 2*NUM_PIXELS}
           5 => "1100000000100011", -- |     BLO     CLEAR_HIST_LOOP
                                    -- | 
                                    -- | ;ACQUIRE_IMAGE:
                                    -- | ; 
                                    -- | ;AI_ERASE_X_MEMORY:
                                    -- | ; 
                                    -- | ;    LDR     r0, START_OF_X  ; Overwrite x ram with constant (note this is in gray code)
                                    -- | ;    LDR     r2, 00h      
                                    -- | ;    LDR     r1, r0          ; Calculate the final address
                                    -- | ;    ADD     r1, NUM_PIXELS
                                    -- | ;
                                    -- | ;AI_EXM_LOOP:
                                    -- | ;
                                    -- | ;    STR     r2, [r0]
                                    -- | ;    ADD     r0, 1
                                    -- | ;    CMP     r0, r1
                                    -- | ;    BLO     AI_EXM_LOOP
                                    -- | ; 
                                    -- | ;AI_IMG:
                                    -- | ;
                                    -- | ;    IMG                     ; Acquire a new image from the ADCs
                                    -- | ;
                                    -- | ;AI_WAIT_FOR_ADC:
                                    -- | ;
                                    -- | ;    ; At this point, the FSM for the ADC is running.  Read the status register
                                    -- | ;    ; to check if the conversion is finished. (Note: very inefficient)
                                    -- | ;
                                    -- | ;    LDR     r0, SR          ; Load the status register
                                    -- | ;    AND     r0, MASK_AF     ; Check if the ADC is active
                                    -- | ;    BNZ     AI_WAIT_FOR_ADC ; Loop while the ADC is converting
                                    -- | ;     
                                    -- | ;AI_CONVERT_TO_BINARY:
                                    -- | ;    
                                    -- | ;    LDR     r0, START_OF_X  ; Start of image
                                    -- | ;    LDR     r1, r0
                                    -- | ;    ADD     r1, NUM_PIXELS  ; End of image
                                    -- | ;
                                    -- | ;AI_CTB_LOOP:
                                    -- | ;
                                    -- | ;    LDR     r2, [r0]    ; Convert gray to binary code
                                    -- | ;    GTB     r2
                                    -- | ;    STR     r2, [r0]
                                    -- | ;    ADD     r0, 1
                                    -- | ;    CMP     r0, r1
                                    -- | ;    BNE     AI_CTB_LOOP
                                    -- | ; 
                                    -- | ;OUTPUT_ORIGINAL_IMAGE:
                                    -- | ;
                                    -- | ;    LDR     r0, START_OF_X
                                    -- | ;    BL      Output_Data_Block   ; Output the image
                                    -- |         
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
                                    -- | ; NPs are grouped in 2x2 clusters.  The histogram will have 256 bins 
                                    -- | ; corresponding to intensities from 0 to 255.  Due to the number of pixels, 16
                                    -- | ; bit numbers are used to store bin counts.  The LSByte is stored in Y memory 
                                    -- | ; and the MSByte is stored in Z memory.  Each NP in the 2x2 cluster is 
                                    -- | ; responsible for storing 64 16-bit bins.  The ranges 0..63, 64..127, 128..191,
                                    -- | ; and 192..255 are stored in the NW, NE, SW, and SE NPs, respectively.  Each
                                    -- | ; iteration a new pixel is read and then cycled around the circle of 4 NPs.  
                                    -- | ; Each NP checks if the value is within its histogram range and, if so, updates
                                    -- | ; its bin counts.  Once all of the pixels are scanned, NP clusters shift and
                                    -- | ; combine their data into the cluster neareset the origin, in NP(0,0), NP (0,1),
                                    -- | ; NP(1,0), and NP(1,1).
                                    -- | 
                                    -- | CLUSTER_HISTOGRAM:
                                    -- | 
                                    -- | CH_INIT:
                                    -- | 
                                    -- |     ; Assign each NP a range of histogram bins.  This is done using masks. 
                                    -- |     ; Assignment is based on even/odd rows/columns.
                                    -- | 
                                    -- |     ; Even columns
                                    -- | 
           6 => "0100000011111000", -- |     LDR     r0, RCR         
           7 => "0010000100000001", -- |     AND     r0, 01h                 ; Check if LSb of column is even or odd
           8 => "1001000000000001", -- |     ZNE                             ; Only even columns remain
           9 => "0100000011111000", -- |         LDR     r0, RCR
          10 => "0010000100010000", -- |         AND     r0, 10h             ; Check if LSb of row is even or odd
          11 => "1001000000000001", -- |         ZNE                         ; Only even rows and columns remain
          12 => "0100000100000000", -- |             LDR     r0, HIST_NP_00
          13 => "0100100011000001", -- |             STR     r0, hist_assignment
          14 => "1000001001000000", -- |         WAK
          15 => "1001000000000000", -- |         ZEQ                         ; odd rows, even columns
          16 => "0100000110000000", -- |             LDR     r0, HIST_NP_10
          17 => "0100100011000001", -- |             STR     r0, hist_assignment
          18 => "1000001001000000", -- |         WAK
          19 => "1000001001000000", -- |     WAK
                                    -- | 
                                    -- |     ; Odd columns
                                    -- |        
          20 => "0100000011111000", -- |     LDR     r0, RCR         
          21 => "0010000100000001", -- |     AND     r0, 01h                 ; Check if LSb of column is even or odd
          22 => "1001000000000000", -- |     ZEQ                             ; Only odd columns remain
          23 => "0100000011111000", -- |         LDR     r0, RCR
          24 => "0010000100010000", -- |         AND     r0, 10h             ; Check if LSb of row is even or odd
          25 => "1001000000000001", -- |         ZNE                         ; Only even rows and odd columns remain
          26 => "0100000101000000", -- |             LDR     r0, HIST_NP_01
          27 => "0100100011000001", -- |             STR     r0, hist_assignment
          28 => "1000001001000000", -- |         WAK
          29 => "1001000000000000", -- |         ZEQ                         ; odd rows, odd columns
          30 => "0100000111000000", -- |             LDR     r0, HIST_NP_11
          31 => "0100100011000001", -- |             STR     r0, hist_assignment
          32 => "1000001001000000", -- |         WAK
          33 => "1000001001000000", -- |     WAK
                                    -- | 
                                    -- |     ; Initialize loop variables
                                    -- |        
          34 => "0100000100000000", -- |     LDR     r0, 0               ; Start with the NW pixel
          35 => "0100100011000010", -- |     STR     r0, index
                                    -- | 
                                    -- | CH_LOOP:
                                    -- | 
          36 => "0100001011110000", -- |     LDR     r1, [r0]            ; Read the pixel
          37 => "0100101011000000", -- |     STR     r1, intensity       ; Save pixel intensity
          38 => "1110111000110000", -- |     BL      Bin_Matching_Pixels
          39 => "1110110101100000", -- |     BL      Swap_Pixels_Horizontally
          40 => "1110111000110000", -- |     BL      Bin_Matching_Pixels
          41 => "1110110010010000", -- |     BL      Swap_Pixels_Vertically
          42 => "1110111000110000", -- |     BL      Bin_Matching_Pixels
          43 => "1110110101100000", -- |     BL      Swap_Pixels_Horizontally
          44 => "1110111000110000", -- |     BL      Bin_Matching_Pixels
          45 => "0100000011000010", -- |     LDR     r0, index
          46 => "0000000100000001", -- |     ADD     r0, 1
          47 => "0100100011000010", -- |     STR     r0, index
          48 => "0011100101000000", -- |     CMP     r0, NUM_PIXELS
          49 => "1100001001000011", -- |     BLO     CH_LOOP
                                    -- | 
                                    -- | ;-------------------------------------------------------------------------------
                                    -- |     
                                    -- | ; At this point each 2x2 NP cluster has formed its own histogram.  All 256 bins,
                                    -- | ; each 16 bit, need to be shifted across the array of NPs into one central 
                                    -- | ; histogram, which is located in the 2x2 NP group nearest the origin.  In order
                                    -- | ; to speed up this process, bins are first combined column-wise in two steps and 
                                    -- | ; then row-wise in three steps.  The final column of NP clusters will be reading
                                    -- | ; garbage data during this time since they have no addition NPs to their E, but
                                    -- | ; this does not introduce errors since they will only read and add zeros to 
                                    -- | ; their bin counts.
                                    -- | 
                                    -- | GLOBAL_HISTOGRAM:
                                    -- | 
                                    -- | ; First combine adjacent rows of clusters.
                                    -- | 
                                    -- | GH_ADD_ROWS_X1:
                                    -- | 
          50 => "0100000101000000", -- |     LDR     r0, START_OF_Y
          51 => "0100100011000011", -- |     STR     r0, index_y
          52 => "0100000110000000", -- |     LDR     r0, START_OF_Z
          53 => "0100100011000100", -- |     STR     r0, index_z
                                    -- | 
                                    -- | GH_ADD_ROWS_X1_LOOP:
                                    -- |             
          54 => "0100000011000011", -- |     LDR     r0, index_y
          55 => "0100001011000100", -- |     LDR     r1, index_z
          56 => "0100010011110000", -- |     LDR     r2, [r0]
          57 => "0100011011110001", -- |     LDR     r3, [r1]
                                    -- | 
          58 => "0100010011100010", -- |     LDR     r2, S               ; Read the external NP's bin values
          59 => "0100010011100010", -- |     LDR     r2, S
          60 => "0100011011100010", -- |     LDR     r3, S
          61 => "0100011011100010", -- |     LDR     r3, S
                                    -- | 
          62 => "0100000011110000", -- |     LDR     r0, [r0]            ; Load this NP's bin values
          63 => "0100001011110001", -- |     LDR     r1, [r1]
                                    -- | 
          64 => "0000000011010010", -- |     ADD     r0, r2              ; Perform 16 bit addition
          65 => "0000101011010011", -- |     ADC     r1, r3      
                                    -- | 
          66 => "0100010011000011", -- |     LDR     r2, index_y         ; Save new bin value
          67 => "0100011011000100", -- |     LDR     r3, index_z
          68 => "0100100011110010", -- |     STR     r0, [r2]
          69 => "0100101011110011", -- |     STR     r1, [r3]
                                    -- | 
          70 => "0000010100000001", -- |     ADD     r2, 1               ; Update bin indices
          71 => "0000011100000001", -- |     ADD     r3, 1
          72 => "0100110011000011", -- |     STR     r2, index_y
          73 => "0100111011000100", -- |     STR     r3, index_z
          74 => "0011110110000000", -- |     CMP     r2, {START_OF_Y + NUM_PIXELS}   ; Are all pixels covered?
          75 => "1100001101100011", -- |     BLO     GH_ADD_ROWS_X1_LOOP
                                    -- | 
                                    -- | ; Now combine bins across a span of two rows.
                                    -- | 
                                    -- | GH_ADD_ROWS_X2:
                                    -- |  
          76 => "0100000101000000", -- |     LDR     r0, START_OF_Y
          77 => "0100100011000011", -- |     STR     r0, index_y
          78 => "0100000110000000", -- |     LDR     r0, START_OF_Z
          79 => "0100100011000100", -- |     STR     r0, index_z
                                    -- | 
                                    -- | GH_ADD_ROWS_X2_LOOP:
                                    -- | 
          80 => "0100000011000011", -- |     LDR     r0, index_y
          81 => "0100001011000100", -- |     LDR     r1, index_z
          82 => "0100010011110000", -- |     LDR     r2, [r0]
          83 => "0100011011110001", -- |     LDR     r3, [r1]
                                    -- | 
          84 => "0100010011100010", -- |     LDR     r2, S               ; Read the external NP's bin values
          85 => "0100010011100010", -- |     LDR     r2, S               ; x4 now, to jump two 2x2 clusters
          86 => "0100010011100010", -- |     LDR     r2, S
          87 => "0100010011100010", -- |     LDR     r2, S
          88 => "0100011011100010", -- |     LDR     r3, S
          89 => "0100011011100010", -- |     LDR     r3, S
          90 => "0100011011100010", -- |     LDR     r3, S
          91 => "0100011011100010", -- |     LDR     r3, S
                                    -- | 
          92 => "0100000011110000", -- |     LDR     r0, [r0]            ; Load this NP's bin values
          93 => "0100001011110001", -- |     LDR     r1, [r1]
                                    -- | 
          94 => "0000000011010010", -- |     ADD     r0, r2              ; Perform 16 bit addition
          95 => "0000101011010011", -- |     ADC     r1, r3      
                                    -- | 
          96 => "0100010011000011", -- |     LDR     r2, index_y         ; Save new bin value
          97 => "0100011011000100", -- |     LDR     r3, index_z
          98 => "0100100011110010", -- |     STR     r0, [r2]
          99 => "0100101011110011", -- |     STR     r1, [r3]
                                    -- | 
         100 => "0000010100000001", -- |     ADD     r2, 1                           ; Update bin indices
         101 => "0000011100000001", -- |     ADD     r3, 1
         102 => "0100110011000011", -- |     STR     r2, index_y
         103 => "0100111011000100", -- |     STR     r3, index_z
         104 => "0011110110000000", -- |     CMP     r2, {START_OF_Y + NUM_PIXELS}   ; Are all pixels covered?
         105 => "1100010100000011", -- |     BLO     GH_ADD_ROWS_X2_LOOP
                                    -- |   
                                    -- | ; Combine bins across a span of one column.
                                    -- | 
                                    -- | GH_ADD_COLS_X1:
                                    -- |  
         106 => "0100000101000000", -- |     LDR     r0, START_OF_Y
         107 => "0100100011000011", -- |     STR     r0, index_y
         108 => "0100000110000000", -- |     LDR     r0, START_OF_Z
         109 => "0100100011000100", -- |     STR     r0, index_z
                                    -- | 
                                    -- | GH_ADD_COLS_X1_LOOP:
                                    -- | 
         110 => "0100000011000011", -- |     LDR     r0, index_y
         111 => "0100001011000100", -- |     LDR     r1, index_z
         112 => "0100010011110000", -- |     LDR     r2, [r0]
         113 => "0100011011110001", -- |     LDR     r3, [r1]
                                    -- | 
         114 => "0100010011101000", -- |     LDR     r2, E               ; Read the external NP's bin values
         115 => "0100010011101000", -- |     LDR     r2, E
         116 => "0100011011101000", -- |     LDR     r3, E
         117 => "0100011011101000", -- |     LDR     r3, E
                                    -- | 
         118 => "0100000011110000", -- |     LDR     r0, [r0]            ; Load this NP's bin values
         119 => "0100001011110001", -- |     LDR     r1, [r1]
                                    -- | 
         120 => "0000000011010010", -- |     ADD     r0, r2              ; Perform 16 bit addition
         121 => "0000101011010011", -- |     ADC     r1, r3      
                                    -- | 
         122 => "0100010011000011", -- |     LDR     r2, index_y         ; Save new bin value
         123 => "0100011011000100", -- |     LDR     r3, index_z
         124 => "0100100011110010", -- |     STR     r0, [r2]
         125 => "0100101011110011", -- |     STR     r1, [r3]
                                    -- | 
         126 => "0000010100000001", -- |     ADD     r2, 1                           ; Update bin indices
         127 => "0000011100000001", -- |     ADD     r3, 1
         128 => "0100110011000011", -- |     STR     r2, index_y
         129 => "0100111011000100", -- |     STR     r3, index_z
         130 => "0011110110000000", -- |     CMP     r2, {START_OF_Y + NUM_PIXELS}   ; Are all pixels covered?
         131 => "1100011011100011", -- |     BLO     GH_ADD_COLS_X1_LOOP
                                    -- |                                    
                                    -- | ; Combine bins across a span of two columns.
                                    -- | 
                                    -- | GH_ADD_COLS_X2:
                                    -- |  
         132 => "0100000101000000", -- |     LDR     r0, START_OF_Y
         133 => "0100100011000011", -- |     STR     r0, index_y
         134 => "0100000110000000", -- |     LDR     r0, START_OF_Z
         135 => "0100100011000100", -- |     STR     r0, index_z
                                    -- | 
                                    -- | GH_ADD_COLS_X2_LOOP:
                                    -- | 
         136 => "0100000011000011", -- |     LDR     r0, index_y
         137 => "0100001011000100", -- |     LDR     r1, index_z
         138 => "0100010011110000", -- |     LDR     r2, [r0]
         139 => "0100011011110001", -- |     LDR     r3, [r1]
                                    -- | 
         140 => "0100010011101000", -- |     LDR     r2, E               ; Read the external NP's bin values
         141 => "0100010011101000", -- |     LDR     r2, E               ; x4 for two columns of 2x2 clusters
         142 => "0100010011101000", -- |     LDR     r2, E
         143 => "0100010011101000", -- |     LDR     r2, E
         144 => "0100011011101000", -- |     LDR     r3, E
         145 => "0100011011101000", -- |     LDR     r3, E
         146 => "0100011011101000", -- |     LDR     r3, E
         147 => "0100011011101000", -- |     LDR     r3, E
                                    -- | 
         148 => "0100000011110000", -- |     LDR     r0, [r0]            ; Load this NP's bin values
         149 => "0100001011110001", -- |     LDR     r1, [r1]
                                    -- | 
         150 => "0000000011010010", -- |     ADD     r0, r2              ; Perform 16 bit addition
         151 => "0000101011010011", -- |     ADC     r1, r3      
                                    -- | 
         152 => "0100010011000011", -- |     LDR     r2, index_y         ; Save new bin value
         153 => "0100011011000100", -- |     LDR     r3, index_z
         154 => "0100100011110010", -- |     STR     r0, [r2]
         155 => "0100101011110011", -- |     STR     r1, [r3]
                                    -- | 
         156 => "0000010100000001", -- |     ADD     r2, 1                           ; Update bin indices
         157 => "0000011100000001", -- |     ADD     r3, 1
         158 => "0100110011000011", -- |     STR     r2, index_y
         159 => "0100111011000100", -- |     STR     r3, index_z
         160 => "0011110110000000", -- |     CMP     r2, {START_OF_Y + NUM_PIXELS}   ; Are all pixels covered?
         161 => "1100100010000011", -- |     BLO     GH_ADD_COLS_X2_LOOP
                                    -- |  
                                    -- |  
                                    -- | ; Combine bins across a span of four columns.
                                    -- | 
                                    -- | GH_ADD_COLS_X4:
                                    -- |  
         162 => "0100000101000000", -- |     LDR     r0, START_OF_Y
         163 => "0100100011000011", -- |     STR     r0, index_y
         164 => "0100000110000000", -- |     LDR     r0, START_OF_Z
         165 => "0100100011000100", -- |     STR     r0, index_z
                                    -- | 
                                    -- | GH_ADD_COLS_X4_LOOP:
                                    -- | 
         166 => "0100000011000011", -- |     LDR     r0, index_y
         167 => "0100001011000100", -- |     LDR     r1, index_z
         168 => "0100010011110000", -- |     LDR     r2, [r0]
         169 => "0100011011110001", -- |     LDR     r3, [r1]
                                    -- | 
         170 => "0100010011101000", -- |     LDR     r2, E               ; Read the external NP's bin values
         171 => "0100010011101000", -- |     LDR     r2, E               ; x8 for 4 2x2 cluster columns
         172 => "0100010011101000", -- |     LDR     r2, E
         173 => "0100010011101000", -- |     LDR     r2, E
         174 => "0100010011101000", -- |     LDR     r2, E
         175 => "0100010011101000", -- |     LDR     r2, E
         176 => "0100010011101000", -- |     LDR     r2, E
         177 => "0100010011101000", -- |     LDR     r2, E
         178 => "0100011011101000", -- |     LDR     r3, E
         179 => "0100011011101000", -- |     LDR     r3, E
         180 => "0100011011101000", -- |     LDR     r3, E
         181 => "0100011011101000", -- |     LDR     r3, E
         182 => "0100011011101000", -- |     LDR     r3, E
         183 => "0100011011101000", -- |     LDR     r3, E
         184 => "0100011011101000", -- |     LDR     r3, E
         185 => "0100011011101000", -- |     LDR     r3, E
                                    -- | 
         186 => "0100000011110000", -- |     LDR     r0, [r0]            ; Load this NP's bin values
         187 => "0100001011110001", -- |     LDR     r1, [r1]
                                    -- | 
         188 => "0000000011010010", -- |     ADD     r0, r2              ; Perform 16 bit addition
         189 => "0000101011010011", -- |     ADC     r1, r3      
                                    -- | 
         190 => "0100010011000011", -- |     LDR     r2, index_y         ; Save new bin value
         191 => "0100011011000100", -- |     LDR     r3, index_z
         192 => "0100100011110010", -- |     STR     r0, [r2]
         193 => "0100101011110011", -- |     STR     r1, [r3]
                                    -- | 
         194 => "0000010100000001", -- |     ADD     r2, 1                           ; Update bin indices
         195 => "0000011100000001", -- |     ADD     r3, 1
         196 => "0100110011000011", -- |     STR     r2, index_y
         197 => "0100111011000100", -- |     STR     r3, index_z
         198 => "0011110110000000", -- |     CMP     r2, {START_OF_Y + NUM_PIXELS}   ; Are all pixels covered?
         199 => "1100101001100011", -- |     BLO     GH_ADD_COLS_X4_LOOP
                                    -- |  
                                    -- | ;-------------------------------------------------------------------------------
                                    -- |        
                                    -- | ;DEBUG:
                                    -- | ;
                                    -- | ;    LDR     r0, START_OF_Y
                                    -- | ;    BL      Output_Data_Block
                                    -- | ;    LDR     r0, START_OF_Z
                                    -- | ;    BL      Output_Data_Block
                                    -- | 
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
         200 => "1100000000001111", -- |     B       START
                                    -- |  
                                    -- | 
                                    -- | 
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | ; Swaps pixel values vertically within a 2x2 NP cluster.
                                    -- | ;
                                    -- | ;   NP00   NP01
                                    -- | ;    |      |
                                    -- | ;    |      |
                                    -- | ;   NP10   NP11
                                    -- | ;
                                    -- | ; Inputs:
                                    -- | ;
                                    -- | ;   intensity   Pixel value to be swapped, probably stored in v memory
                                    -- | ;
                                    -- | ; Outputs:
                                    -- | ;
                                    -- | ;   intensity   Pixel value from NP to the N/S depending on this NP's position 
                                    -- | ;               in the 2x2 cluster.
                                    -- | ;
                                    -- | ; Registers Modified:
                                    -- | ;
                                    -- | ;   r0, r1
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
                                    -- | Swap_Pixels_Vertically:
                                    -- | 
         201 => "0100000011000000", -- |     LDR     r0, intensity       ; Load this NPs current intensity
         202 => "0100001011111000", -- |     LDR     r1, RCR             ; Turn off the odd rows
         203 => "0010001100010000", -- |     AND     r1, 10h 
         204 => "1001000000000001", -- |     ZNE
         205 => "0100001011000000", -- |         LDR     r1, intensity   ; Save copy of intensity for S NP
         206 => "0100000011100010", -- |         LDR     r0, S           ; Load pixel from S NP
         207 => "0100100011000000", -- |         STR     r0, intensity
         208 => "1000001001000000", -- |     WAK
         209 => "1001000000000000", -- |     ZEQ                         ; Turn off even rows
         210 => "0100001011100001", -- |         LDR     r1, N           ; Load pixel from N NP
         211 => "0100101011000000", -- |         STR     r1, intensity   ; N NP has our intensity, no tricks req'd
         212 => "1000001001000000", -- |     WAK
         213 => "1000001000010000", -- |     BX
                                    -- |     
                                    -- | 
                                    -- |  
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | ; Swaps pixel values horizontally within a 2x2 NP cluster.
                                    -- | ;
                                    -- | ;   NP00---NP01
                                    -- | ;
                                    -- | ;
                                    -- | ;   NP10---NP11
                                    -- | ;
                                    -- | ; Inputs:
                                    -- | ;
                                    -- | ;   intensity   Pixel value to be swapped, probably stored in v memory
                                    -- | ;
                                    -- | ; Outputs:
                                    -- | ;
                                    -- | ;   intensity   Pixel value from NP to the N/S depending on this NP's position 
                                    -- | ;               in the 2x2 cluster.
                                    -- | ;
                                    -- | ; Registers Modified:
                                    -- | ;
                                    -- | ;   r0, r1
                                    -- | ;-------------------------------------------------------------------------------
                                    -- |  
                                    -- | Swap_Pixels_Horizontally:
                                    -- |       
         214 => "0100000011000000", -- |     LDR     r0, intensity       ; Load this NPs current intensity
         215 => "0100001011111000", -- |     LDR     r1, RCR             ; Turn off the odd columns
         216 => "0010001100000001", -- |     AND     r1, 01h 
         217 => "1001000000000001", -- |     ZNE
         218 => "0100001011000000", -- |         LDR     r1, intensity   ; Save copy of intensity for E NP
         219 => "0100000011101000", -- |         LDR     r0, E           ; Load pixel from E NP
         220 => "0100100011000000", -- |         STR     r0, intensity
         221 => "1000001001000000", -- |     WAK
         222 => "1001000000000000", -- |     ZEQ                         ; Turn off even rows
         223 => "0100001011100100", -- |         LDR     r1, W           ; Load pixel from W NP
         224 => "0100101011000000", -- |         STR     r1, intensity   ; W NP has our intensity, no tricks req'd
         225 => "1000001001000000", -- |     WAK
         226 => "1000001000010000", -- |     BX
                                    -- | 
                                    -- | 
                                    -- |       
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | ; Determines if a passed pixel value belongs in this NP's histogram bins and, if 
                                    -- | ; so, updates the bin count.
                                    -- | ;
                                    -- | ; Inputs:
                                    -- | ;
                                    -- | ;   intensity   Pixel value to be binned, 0..255
                                    -- | ;
                                    -- | ; Outputs:
                                    -- | ;
                                    -- | ;   Local NP bins in Y and Z memory are updated
                                    -- | ;
                                    -- | ; Registers Modified:
                                    -- | ;
                                    -- | ;   r0, r1, r2, r3
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
                                    -- | Bin_Matching_Pixels:
                                    -- | 
                                    -- |     ; Check if pixel belongs in this NP's bins.
                                    -- | 
         227 => "0100000011000000", -- |     LDR     r0, intensity           ; Load this NP's current intensity
         228 => "0010000111000000", -- |     AND     r0, HIST_NP_MASK        ; Check if this intensity belongs to this NP
         229 => "0011100011000001", -- |     CMP     r0, hist_assignment 
         230 => "1001000000000001", -- |     ZNE     
         231 => "0100000011000000", -- |         LDR     r0, intensity       ; Isolate the bin address
         232 => "0010000100111111", -- |         AND     r0, HIST_BIN_MASK
         233 => "0100001011010000", -- |         LDR     r1, r0              ; Calculate the LSB and MSB addresses
         234 => "0000000101000000", -- |         ADD     r0, START_OF_Y
         235 => "0000001110000000", -- |         ADD     r1, START_OF_Z
         236 => "0100010011110000", -- |         LDR     r2, [r0]            ; Peform 16 bit addition
         237 => "0100011011110001", -- |         LDR     r3, [r1]            ; Peform 16 bit addition
         238 => "0000010100000001", -- |         ADD     r2, 1
         239 => "0000111100000000", -- |         ADC     r3, 0
         240 => "0100110011110000", -- |         STR     r2, [r0]            ; Save the 16 bit result
         241 => "0100111011110001", -- |         STR     r3, [r1]
         242 => "1000001001000000", -- |     WAK
         243 => "1000001000010000", -- |     BX
                                    -- | 
                                    -- | 
                                    -- | 
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | ; Output_Data_Block
                                    -- | ;
                                    -- | ; Description:
                                    -- | ;
                                    -- | ;   The output function, which dumps 64 bytes of data out through the column
                                    -- | ;   data bus.  This can be called with different starting addresses to determine
                                    -- | ;   whether X, Y, Z or some other block is output.
                                    -- | ;
                                    -- | ; Inputs:
                                    -- | ;
                                    -- | ;   r0          The address to begin outputing.  The last byte will be output
                                    -- | ;               from address r0+63.
                                    -- | ;
                                    -- | ; Outputs:
                                    -- | ;
                                    -- | ;   -           64 bytes for each NP on column data bus.
                                    -- | ;
                                    -- | ; Registers Modified:
                                    -- | ;
                                    -- | ;   r0, r1
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
                                    -- | Output_Data_Block:
                                    -- | 
         244 => "0100001011010000", -- |     LDR     r1, r0          ; Calculate the final address.
         245 => "0000001101000000", -- |     ADD     r1, 64      
                                    -- | 
                                    -- | OUTPUT_DATA_BLOCK_LOOP:
                                    -- | 
         246 => "1010000011110000", -- |     OUT     [r0], 0          ; Row 0
         247 => "1010001011110000", -- |     OUT     [r0], 1          ; Row 1
         248 => "1010010011110000", -- |     OUT     [r0], 2          ; Row 2
         249 => "1010011011110000", -- |     OUT     [r0], 3          ; Row 3
         250 => "1010100011110000", -- |     OUT     [r0], 4          ; Row 4
         251 => "1010101011110000", -- |     OUT     [r0], 5          ; Row 5
         252 => "1010110011110000", -- |     OUT     [r0], 6          ; Row 6
         253 => "1010111011110000", -- |     OUT     [r0], 7          ; Row 7
                                    -- |     
         254 => "0000000100000001", -- |     ADD     r0, 1
         255 => "0011100011010001", -- |     CMP     r0, r1
         256 => "1100111101100001", -- |     BNE     OUTPUT_DATA_BLOCK_LOOP
         257 => "1000001000010000", -- |     BX 
      others => "0000000000000000");


signal a_as_int     : natural range 0 to 2**ADDR_WIDTH-1;
signal a_as_int_reg : natural range 0 to 2**ADDR_WIDTH-1;

begin

    a_as_int <= to_integer(unsigned(a));
    q <= rom(a_as_int_reg);
    a_as_int_reg <= a_as_int;

end behavioral;
