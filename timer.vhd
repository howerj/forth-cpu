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
		timer_length: positive := 16);
	port(
		clk:          in  std_logic;
		rst:          in  std_logic;

		we:           in  std_logic; -- write enable for control register
		control_i:    in  std_logic_vector(timer_length - 1 downto 0); -- control register
		control_o:    out std_logic_vector(timer_length - 1 downto 0);
		counter_o:    out std_logic_vector(timer_length - 4 downto 0);
		irq:          out std_logic;  -- Interrupt signal
		Q:            out std_logic;  -- Timer signal
		NQ:           out std_logic); -- Timer signal inverted
end entity;

architecture behav of timer is
	constant highest_bit:         positive := timer_length - 1;
	constant control_enable_bit:  positive := highest_bit;
	constant timer_reset_bit:     positive := highest_bit - 1;
	constant irq_enable_bit:      positive := highest_bit - 2;
	constant timer_highest_bit:   positive := highest_bit - 3;

	signal control_c, control_n:  std_logic_vector(highest_bit downto 0) := (others => '0');
	signal q_c, q_n:              std_logic:= '0';

	signal reset_timer:           std_logic  := '0';
	signal enabled:               std_logic  := '0';
	signal irq_en:                std_logic  := '0';

	signal timer_reset:           std_logic  := '0';

	signal compare:               std_logic_vector(timer_highest_bit downto 0) := (others => '0');
	signal count:                 unsigned(timer_highest_bit downto 0)         := (others => '0');
begin
	assert (timer_length >= 4) report "Timer needs to be at least 4 bits wide: 3 bits for control - 1 for counter" severity failure;

	Q  <= q_c;
	NQ <= not q_c;

	enabled     <= control_c(control_enable_bit);
	reset_timer <= control_c(timer_reset_bit);
	irq_en      <= control_c(irq_enable_bit);
	compare     <= control_c(timer_highest_bit downto 0);

	control_o   <= control_c;
	counter_o   <= std_logic_vector(count);

	clockRegisters: process(clk, rst)
	begin
		if rst = '1' then
			q_c       <= '0';
			control_c <= (others => '0');
		elsif rising_edge(clk) then
			q_c       <= q_n;
			control_c <= control_n;
		end if;
	end process;

	counter: process (clk, rst)
	begin
		if rst = '1' then
			count <= (others => '0');
		elsif rising_edge(clk) then
			if reset_timer = '1' or timer_reset = '1' then
				count <= (others => '0');
			elsif enabled = '1' then
				count <= count + 1;
			else
				count <= count;
			end if;
		end if;
	end process;

	output: process(count, we, control_i, control_c, compare, q_c, irq_en, enabled)
	begin
		irq         <= '0';
		q_n         <= q_c;
		control_n      <= control_c;
		timer_reset <= '0';

		control_n(timer_reset_bit)  <= '0'; -- reset

		if we = '1' then
			control_n <= control_i;
		end if;

		if count = unsigned(compare) and enabled = '1' then
			if irq_en = '1' then
				irq <= '1';
			end if;
			timer_reset <= '1';
			q_n <= not q_c;
		end if;
	end process;
end architecture;

