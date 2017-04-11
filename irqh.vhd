--------------------------------------------------------------------------------
--| @file irqh.vhd
--| @brief Interrupt request handler, while the CPU can handle interrupts
--|        it does not to a good job of it. This allows customization of
--|        priority and other such things.
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| @todo Add interrupt mask register
--------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- only needed for calculations relating to generics
use work.util.reg;
use work.util.n_bits;

entity irqh is
	generic(number_of_interrupts:   positive := 8;
		lowest_interrupt_first: boolean := true);
	port(
		clk:    in  std_logic;
		rst:    in  std_logic;

		irq_i:  in  std_logic;
		irc_i:  in  std_logic_vector(number_of_interrupts - 1 downto 0);

		irq_o:  out std_logic;
		addr_o: out std_logic_vector(n_bits(number_of_interrupts) - 1 downto 0)); 
end;

architecture behav of irqh is
	constant addr_length: natural := n_bits(number_of_interrupts) - 1;
	signal irq_n: std_logic := '0';
	signal irc_n: std_logic_vector(number_of_interrupts - 1 downto 0) := (others => '0');

	signal addr:  std_logic_vector(addr_length downto 0) := (others => '0');
	signal irq:   std_logic := '0';
begin

	irq_in: entity work.reg generic map(N => 1) 
		port map(clk => clk, rst => rst, we => '1', di(0) => irq_i, do(0) => irq_n);

	irc_in: entity work.reg generic map(N => number_of_interrupts) 
		port map(clk => clk, rst => rst, we => '1', di => irc_i, do => irc_n); 

	irq <= irq_n;
	process(irc_n)
	begin
		-- Simple priority encoding, last interrupt signal that is active in the
		-- loop is the address to jump to on an interrupt.

		addr <= (others => '0');
		if lowest_interrupt_first then
			LSB: for i in number_of_interrupts downto 1
			loop
				if irc_n(i - 1) = '1' then
					addr <= std_logic_vector(to_unsigned(i - 1, addr'length));
				end if;
			end loop;
		else
			MSB: for i in 1 to number_of_interrupts 
			loop
				if irc_n(i - 1) = '1' then
					addr <= std_logic_vector(to_unsigned(i - 1, addr'length));
				end if;
			end loop;
		end if;
	end process;

	irq_out: entity work.reg 
		generic map(N => 1) 
		port map(clk => clk, rst => rst, we => '1', di(0) => irq, do(0) => irq_o);

	addr_out: entity work.reg
		generic map(N => 3)
		port map(clk => clk, rst => rst, we => '1', di => addr, do => addr_o);


end architecture;
