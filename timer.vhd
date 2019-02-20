-------------------------------------------------------------------------------
--| @file timer.vhd
--| @brief General Purpose Timer. It is of customizable length,
--|        the minimum being 4-bits, one for the actual timing, the other
--|        three for control. (timer.vhd, original file name)
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2017 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| The control register contains both the value to compare the timer against
--| as well as three control bits. Given a "timer_length" value of eight the
--| control bits are:
--|
--| Bit     Input Description
--| 7       Clock enable
--| 6       Timer reset
--| 5       Interrupt enable
--| 4 - 0   Timer compare value
--|
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timer is
	generic(
		asynchronous_reset:  boolean := true; -- use asynchronous reset if true, synchronous if false
		delay:               time    := 0 ns; -- simulation only

		timer_length: positive := 16);
	port(
		clk:          in  std_ulogic;
		rst:          in  std_ulogic;

		we:           in  std_ulogic; -- write enable for control register
		control_i:    in  std_ulogic_vector(timer_length - 1 downto 0); -- control register
		counter_o:    out std_ulogic_vector(timer_length - 4 downto 0);
		irq:          out std_ulogic); -- generate interrupt
end entity;

architecture behav of timer is
	constant highest_bit:         positive := timer_length - 1;
	constant control_enable_bit:  positive := highest_bit;
	constant timer_reset_bit:     positive := highest_bit - 1;
	constant irq_enable_bit:      positive := highest_bit - 2;
	constant timer_highest_bit:   positive := highest_bit - 3;

	signal control_c, control_n:  std_ulogic_vector(highest_bit downto 0) := (others => '0');

	signal reset_timer:           std_ulogic  := '0';
	signal enabled:               std_ulogic  := '0';
	signal irq_en:                std_ulogic  := '0';

	signal timer_reset:           std_ulogic  := '0';

	signal compare:               std_ulogic_vector(timer_highest_bit downto 0) := (others => '0');
	signal count:                 unsigned(timer_highest_bit downto 0)         := (others => '0');
begin
	assert (timer_length >= 4) report "Timer needs to be at least 4 bits wide: 3 bits for control - 1 for counter" severity failure;

	enabled     <= control_c(control_enable_bit);
	reset_timer <= control_c(timer_reset_bit);
	irq_en      <= control_c(irq_enable_bit);
	compare     <= control_c(timer_highest_bit downto 0);

	counter_o   <= std_ulogic_vector(count);

	counter: process (clk, rst)
	begin
		if rst = '1' and asynchronous_reset then
			count     <= (others => '0') after delay;
			control_c <= (others => '0') after delay;
		elsif rising_edge(clk) then
			if rst = '1' and not asynchronous_reset then
				count     <= (others => '0') after delay;
				control_c <= (others => '0') after delay;
			else
				control_c <= control_n;
				if reset_timer = '1' or timer_reset = '1' then
					count <= (others => '0') after delay;
				elsif enabled = '1' then
					count <= count + 1 after delay;
				else
					count <= count after delay;
				end if;
			end if;
		end if;
	end process;

	output: process(count, we, control_i, control_c, compare, irq_en, enabled)
	begin
		if we = '1' then
			control_n <= control_i after delay;
		else
			control_n                   <= control_c after delay;
			control_n(timer_reset_bit)  <= '0' after delay; -- reset
		end if;

		if count = unsigned(compare) and enabled = '1' then
			irq         <= irq_en after delay;
			timer_reset <= '1' after delay;
		else
			irq         <= '0' after delay;
			timer_reset <= '0' after delay;
		end if;
	end process;
end architecture;

