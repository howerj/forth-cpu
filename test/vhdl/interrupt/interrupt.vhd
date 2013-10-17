-------------------------------------------------------------------------------
--! @file interrupt.vhd
--! @brief Interrupt handler.
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity interrupt is
  port(
    clk:          in std_logic;
    rst:          in std_logic
  );
end entity;

architecture rtl of interrupt is
begin

end architecture;
