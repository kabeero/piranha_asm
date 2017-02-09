--------------------------------------------------------------------------------
-- Generated on 09/22/2014 at 23:19:10
-- Command: ./npasm -alr -o out src/img.npasm
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
                                    -- | ; img.npasm
                                    -- | ;-------------------------------------------------------------------------------
                                    -- | 
                                    -- | #define MASK_AF 01000000b   ; Mask for ADC active flag
                                    -- | #define BZ      BEQ         ; Alias branch if zero to branch if equal
                                    -- | #define BNZ     BNE         ; Alias branch if not zero to branch if not equal
                                    -- | 
                                    -- | START:
                                    -- | 
           0 => "1000001010000000", -- |     NOP                     ; Add dummy cycle since imaging immediately on 
                                    -- |                             ;   reset is awkward for simulation purposes.
                                    -- | 
                                    -- | CLEAN_SLATE_INIT:
                                    -- |  
           1 => "0100000100000000", -- |     LDR     r0, 0           ; Overwrite x ram with constant (note this is in gray code)
           2 => "0100010111111111", -- |     LDR     r2, 255      
           3 => "0100001011010000", -- |     LDR     r1, r0          ; Calculate the final address
           4 => "0000001101000000", -- |     ADD     r1, 64      
                                    -- | 
                                    -- | CLEAN_SLATE_LOOP:
                                    -- | 
           5 => "0100110011110000", -- |     STR     r2, [r0]
           6 => "0000000100000001", -- |     ADD     r0, 1
           7 => "0011100011010001", -- |     CMP     r0, r1
           8 => "1100000001010001", -- |     BNE     CLEAN_SLATE_LOOP
                                    -- |  
                                    -- | ACQUIRE_IMAGE:
                                    -- | 
           9 => "1000001000100000", -- |     IMG                     ; Acquire a new image from the ADCs
                                    -- | 
                                    -- | WAIT_FOR_ADC:
                                    -- | 
                                    -- |     ; At this point, the FSM for the ADC is running.  Read the status register
                                    -- |     ; to check if the conversion is finished. (Note: very inefficient)
                                    -- | 
          10 => "0100000011110100", -- |     LDR     r0, SR          ; Load the status register
          11 => "0010000101000000", -- |     AND     r0, MASK_AF     ; Check if the ADC is active
          12 => "1100000010100001", -- |     BNZ     WAIT_FOR_ADC    ; Loop while the ADC is converting
                                    -- | 
                                    -- | OUTPUT_DATA_BLOCK_INIT:
                                    -- | 
                                    -- |     ; Begin outputting all of the data in x, which is the newly captured image.
                                    -- | 
          13 => "0100000100000000", -- |     LDR     r0, 0           ; Output x ram
          14 => "0100001011010000", -- |     LDR     r1, r0          ; Calculate the final address
          15 => "0000001101000000", -- |     ADD     r1, 64      
                                    -- | 
                                    -- | OUTPUT_DATA_BLOCK_LOOP:
                                    -- | 
          16 => "0100010011110000", -- |     LDR     r2, [r0]        ; Convert gray to binary code
          17 => "1000110000100000", -- |     GTB     r2
          18 => "0100110011110000", -- |     STR     r2, [r0]
          19 => "1010000011110000", -- |     OUT     [r0], 0         ; Row 0
          20 => "1010001011110000", -- |     OUT     [r0], 1         ; Row 1
          21 => "1010010011110000", -- |     OUT     [r0], 2         ; Row 2
          22 => "1010011011110000", -- |     OUT     [r0], 3         ; Row 3
          23 => "1010100011110000", -- |     OUT     [r0], 4         ; Row 4
          24 => "1010101011110000", -- |     OUT     [r0], 5         ; Row 5
          25 => "1010110011110000", -- |     OUT     [r0], 6         ; Row 6
          26 => "1010111011110000", -- |     OUT     [r0], 7         ; Row 7
                                    -- |     
          27 => "0000000100000001", -- |     ADD     r0, 1
          28 => "0011100011010001", -- |     CMP     r0, r1
          29 => "1100000100000001", -- |     BNE     OUTPUT_DATA_BLOCK_LOOP
                                    -- | 
                                    -- | END:
                                    -- | 
          30 => "1100000000001111", -- |     B       START
      others => "0000000000000000");


signal a_as_int     : natural range 0 to 2**ADDR_WIDTH-1;
signal a_as_int_reg : natural range 0 to 2**ADDR_WIDTH-1;

begin

    a_as_int <= to_integer(unsigned(a));
    q <= rom(a_as_int_reg);
    a_as_int_reg <= a_as_int;

end behavioral;
