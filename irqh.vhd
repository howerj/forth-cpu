--------------------------------------------------------------------------------
--! @file irqh.vhd
--! @brief Interrupt request handler, while the CPU can handle interrupts
--!        it does not to a good job of it. This allows customization of
--!        priority and other such things.
--!
--! @author     Richard James Howe.
--! @copyright  Copyright 2017 Richard James Howe.
--! @license    MIT
--! @email      howe.r.j.89@gmail.com
--------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- only needed for calculations relating to generics
use work.util.reg;
use work.util.breg;


entity irqh is
	generic(number_of_interrupts: positive := 8;
	        addr_length:          positive := 3);
	port(
		clk:    in  std_logic;
		rst:    in  std_logic;

		irq_i:  in  std_logic;
		irc_i:  in  std_logic_vector(number_of_interrupts - 1 downto 0);

		irq_o:  out std_logic;
		addr_o: out std_logic_vector(addr_length - 1 downto 0)); 
end;

architecture behav of irqh is
	signal irq_n: std_logic := '0';
	signal irc_n: std_logic_vector(number_of_interrupts - 1 downto 0) := (others => '0');

	signal addr:  std_logic_vector(addr_length - 1 downto 0) := (others => '0');
	signal irq:   std_logic := '0';
begin
	assert 2 ** addr_length >= number_of_interrupts;

	irq_in: entity work.breg port map(clk => clk, rst => rst, we => '1', di => irq_i, do => irq_n);

	irc_in: entity work.reg generic map(N => number_of_interrupts) 
		port map(clk => clk, rst => rst, we => '1', di => irc_i, do => irc_n); 

	irq <= irq_n;
	process(irc_n)
	begin
		-- Simple priority encoding MSB first, for the moment other
		-- interrupts are simply dropped. To improve this a queue
		-- should be added.
		addr <= (others => '0');
		interrupts: for i in 1 to number_of_interrupts 
		loop
			if irc_n(i - 1) = '1' then
				addr <= std_logic_vector(to_unsigned(i - 1, addr'length));
			end if;
		end loop;
	end process;

	irq_out: entity work.breg port map(clk => clk, rst => rst, we => '1', di => irq, do => irq_o);

	addr_out: entity work.reg
		generic map(N => 3)
		port map(clk => clk, rst => rst, we => '1', di => addr, do => addr_o);


end architecture;
