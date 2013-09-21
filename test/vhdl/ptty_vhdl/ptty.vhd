-- Richard James Howe.
--
-- ptty driver module, this is meant to simulate a 
-- terminal to uart interface for testing.
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ptty is
  port(
    clk:          in  std_logic;
    rst:          in  std_logic
  );
end;

architecture behav of ptty is
begin
end architecture;
