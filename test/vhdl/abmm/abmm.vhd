-- Arbitrary bit manipulation module
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com

-- TODO, use generators?

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity abmm is
  port(
  s_reg:          in  std_logic_vector(15 downto 0); -- Special bit manipulations

  addr_reg:       in  std_logic_vector(63 downto 0); -- Address registers, 16*4 bits

  din:            in  std_logic_vector(15 downto 0); -- Word to process
  dout:           out std_logic_vector(15 downto 0)  -- Result
      );
end;

architecture rtl of abmm is
begin

  abmm_generator: for i in 1 to 16 generate
  begin
   AX: entity work.abmm_multiplexer 
   port map(
             s      =>  s_reg(i-1),
             c_bit  =>  din(i-1),
             input  =>  din,
             addr   =>  addr_reg((i*4)-1 downto (i*4)-4),
             output =>  dout(i-1)
           );
  end generate abmm_generator;

end architecture;

