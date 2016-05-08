--------------------------------------------------------------------------------
--! @file irqh.vhd
--! @brief Interrupt request handler, while the CPU can handle interrupts
--!        it does not to a good job of it. This allows customization of
--!        prioritory and other such things.
--! 
--! @author     Richard James Howe.
--! @copyright  Copyright 2013 Richard James Howe.
--! @license    LGPL    
--! @email      howe.r.j.89@gmail.com
--------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity irqh is
	generic(
	number_of_interrupts: positive := 4
	);
	port(
	clk:     in   std_logic;
	rst:     in   std_logic;

	raw_irq: in   std_logic;
	raw_irc: in   std_logic_vector(number_of_interrupts - 1 downto 0);

	processed_irq:    out   std_logic;
	processed_irc:    out   std_logic_vector(number_of_interrupts - 1 downto 0)

	);
end;

architecture behav of irqh is
begin
	processed_irq <= raw_irq;
	processed_irc <= raw_irc;
end architecture;
