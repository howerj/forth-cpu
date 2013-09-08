-- Richard James Howe.
--
-- DAC driver module, this module takes some BRAM and
-- drives via SPI a DAC
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dac is
  port(
    clk:        in   std_logic;
    clk25MHz:   in   std_logic;
    rst:        in   std_logic
  );
end;

architecture behav of dac is

begin


end architecture;
