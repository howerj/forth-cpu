--------------------------------------------------------------------------------
--| @file irqh.vhd
--| @brief Interrupt request handler, while the CPU can handle interrupts
--|        it does not to a good job of it. This allows customization of
--|        priority.
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| This is a simple interrupt handler, interrupts are decoded in priority
--| order which can be set by a generic. If an interrupt occurs and then
--| another interrupt of the same type occurs before it has been processed
--| the second interrupt will be lost.
--------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- only needed for calculations relating to generics
use work.util.reg;
use work.util.n_bits;
use work.util.select_bit;
use work.util.priority;

entity irqh is
	generic(
		number_of_interrupts:   positive := 8;
		lowest_interrupt_first: boolean  := true);
	port(
		clk:     in  std_logic;
		rst:     in  std_logic;

		irq_i:   in  std_logic;
		irc_i:   in  std_logic_vector(number_of_interrupts - 1 downto 0);

		mask:    in  std_logic_vector(number_of_interrupts - 1 downto 0);
		mask_we: in  std_logic;

		irq_o:   out std_logic;
		addr_o:  out std_logic_vector(n_bits(number_of_interrupts) - 1 downto 0));
end;

architecture rtl of irqh is
	constant addr_length: natural := n_bits(number_of_interrupts);
	signal irq_n: std_logic := '0';
	signal irc_n: std_logic_vector(irc_i'range) := (others => '0');

	signal addr:  std_logic_vector(addr_length - 1 downto 0) := (others => '0');
	signal irq:   std_logic := '0';

	signal mask_n: std_logic_vector(mask'range) := (others => '0');
begin

	irq_in: entity work.reg
		generic map(
			N      => 1)
		port map(
			clk    =>  clk,
			rst    =>  rst,
			we     =>  '1',
			di(0)  =>  irq_i,
			do(0)  =>  irq_n);

	irc_in: entity work.reg
		generic map(
			N    => number_of_interrupts)
		port map(
			clk  =>  clk,
			rst  =>  rst,
			we   =>  '1',
			di   =>  irc_i,
			do   =>  irc_n);

	irc_mask: entity work.reg generic map(
			N    => number_of_interrupts)
		port map(
			clk  =>  clk,
			rst  =>  rst,
			we   =>  mask_we,
			di   =>  mask,
			do   =>  mask_n);

	process(irc_n, irq_n, mask_n)
		variable addr_n: std_logic_vector(addr'range) := (others => '0');
	begin
		addr_n := priority(irc_n, not lowest_interrupt_first);
		addr <= addr_n;
		if select_bit(mask_n, addr_n) = '1' then
			irq <= irq_n;
		else
			irq <= '0';
		end if;
	end process;

	irq_out: entity work.reg
		generic map(
			N      => 1)
		port map(
			clk    =>  clk,
			rst    =>  rst,
			we     =>  '1',
			di(0)  =>  irq,
			do(0)  =>  irq_o);

	addr_out: entity work.reg
		generic map(
			N      => addr_length)
		port map(
			clk    =>  clk,
			rst    =>  rst,
			we     =>  '1',
			di     =>  addr,
			do     =>  addr_o);

end architecture;
