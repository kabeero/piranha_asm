--------------------------------------------------------------------------------
-- Generated on 01/06/2016 at 00:10:38
-- Command: ./npasm -alr -o out src/rcr_readout.npasm
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
                                    -- | ; rcr_readout.npasm
                                    -- | ;-------------------------------------------------------------------------------
                                    -- |   
                                    -- | ; Architecture constants
                                    -- | 
                                    -- | #define START_OF_X          00000000b   ; Starting address of X RAM.
                                    -- | #define START_OF_Y          01000000b   ; Starting address of Y RAM.
                                    -- | #define START_OF_Z          10000000b   ; Starting address of X RAM.
                                    -- | #define START_OF_V          11000000b   ; Starting address of V RAM.
                                    -- | #define NUM_PIXELS          64          ; The number of pixels in one NP.
                                    -- | 
                                    -- | ; Variables
                                    -- | 
                                    -- | #define addr                r0          ; The address to write to.
                                    -- | #define final_addr          r1          ; The last address to write to.
                                    -- | #define pattern             r3          ; The pattern to write.
                                    -- | 
                                    -- | START:
                                    -- | 
                                    -- | LOAD_PATTERN_INIT:
                                    -- |  
           0 => "0100000100000000", -- |     LDR     addr, START_OF_X        ; Load pattern into RAM.
           1 => "0100001011010000", -- |     LDR     final_addr, addr        ; Calculate the final address.
           2 => "0000001101000000", -- |     ADD     final_addr, NUM_PIXELS
           3 => "0100011011111000", -- |     LDR     pattern, rcr            ; Load each np with a unique pattern.
                                    -- | 
                                    -- | LOAD_PATTERN_LOOP:
                                    -- | 
           4 => "0100111011110000", -- |     STR     pattern, [addr]         ; Save the pattern in each location.
           5 => "0000011100000001", -- |     ADD     pattern, 1              ; Increment so each pixel is different.
           6 => "0000000100000001", -- |     ADD     addr, 1                 ; Check if all pixels are covered.
           7 => "0011100011010001", -- |     CMP     addr, final_addr
           8 => "1100000001000011", -- |     BLO     LOAD_PATTERN_LOOP
                                    -- |  
                                    -- | OUTPUT_DATA_BLOCK_SETUP:
                                    -- | 
           9 => "0100000100000000", -- |     LDR     addr, START_OF_X        ; Output the X RAM.
          10 => "0100001011010000", -- |     LDR     final_addr, addr        ; Calculate the final address.
          11 => "0000001101000000", -- |     ADD     final_addr, NUM_PIXELS
                                    -- |                 
                                    -- | OUTPUT_DATA_BLOCK_LOOP:
                                    -- | 
          12 => "1010000011110000", -- |     OUT     [addr], 0               ; Row 0
          13 => "1010001011110000", -- |     OUT     [addr], 1               ; Row 1
          14 => "1010010011110000", -- |     OUT     [addr], 2               ; Row 2
          15 => "1010011011110000", -- |     OUT     [addr], 3               ; Row 3
          16 => "1010100011110000", -- |     OUT     [addr], 4               ; Row 4
          17 => "1010101011110000", -- |     OUT     [addr], 5               ; Row 5
          18 => "1010110011110000", -- |     OUT     [addr], 6               ; Row 6
          19 => "1010111011110000", -- |     OUT     [addr], 7               ; Row 7
                                    -- |     
          20 => "0000000100000001", -- |     ADD     addr, 1                 ; Check if done outputting the RAM.
          21 => "0011100011010001", -- |     CMP     addr, final_addr
          22 => "1100000011000001", -- |     BNE     OUTPUT_DATA_BLOCK_LOOP
                                    -- | 
                                    -- | END:
                                    -- | 
          23 => "1100000000001111", -- |     B       START
                                    -- | 
                                    -- |  
      others => "0000000000000000");


signal a_as_int     : natural range 0 to 2**ADDR_WIDTH-1;
signal a_as_int_reg : natural range 0 to 2**ADDR_WIDTH-1;

begin

    a_as_int <= to_integer(unsigned(a));
    q <= rom(a_as_int_reg);
    a_as_int_reg <= a_as_int;

end behavioral;
