-------------------------------------------------------------------------------
--! @file timer.vhd
--! @brief General Purpose Timer. It is of customizable length,
--!        the minimum being 4-bits, one for the actual timing, the other
--!        three for control. (timer.vhd, original file name)
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        MIT
--! @email          howe.r.j.89@gmail.com
--! @todo Check this synthesizes correctly.
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timer is
	generic(timer_length: positive := 16);
	port(
	clk:          in std_logic;
	rst:          in std_logic;

	we:           in std_logic; -- write enable for control register
	control:      in std_logic_vector(timer_length - 1 downto 0); -- control register

	-- Timer interrupts
	irq:          out std_logic;  -- Compare Interrupt
	Q:            out std_logic;  -- Timer signal
	NQ:           out std_logic); -- Timer signal inverted
end entity;

architecture behav of timer is
	constant highest_bit:         positive := timer_length - 1;
	constant control_enable_bit:  positive := highest_bit;
	constant timer_reset_bit:     positive := highest_bit - 1;
	constant irq_enable_bit:      positive := highest_bit - 2;
	constant timer_highest_bit:   positive := highest_bit - 3;

	signal ctrl_c, ctrl_n:  std_logic_vector(highest_bit downto 0) := (others => '0');
	signal q_c, q_n:        std_logic:= '0';

	signal ctr_localrst:    std_logic  := '0';
	signal ctr_enabled:     std_logic  := '0';
	signal ctr_irq_en:      std_logic  := '0';

	signal timer_reset:     std_logic  := '0';

	signal compare:         std_logic_vector(timer_highest_bit downto 0) := (others => '0');
	signal count:           unsigned(timer_highest_bit downto 0)         := (others => '0');
begin
	assert (timer_length >= 4) report "Timer needs to be at least 4 bits wide: 3 bits for control - 1 for counter" severity failure;

	Q  <= q_c;
	NQ <= not q_c;

	ctr_enabled     <= ctrl_c(control_enable_bit);
	ctr_localrst    <= ctrl_c(timer_reset_bit);
	ctr_irq_en      <= ctrl_c(irq_enable_bit);
	compare         <= ctrl_c(timer_highest_bit downto 0);

	clockRegisters: process(clk, rst)
	begin
	if rst = '1' then
		q_c    <= '0';
		ctrl_c <= (others => '0');
	elsif rising_edge(clk) then
		q_c    <= q_n;
		ctrl_c <= ctrl_n;
	end if;
	end process;

	counter: process (clk, rst)
	begin
		if rst = '1' then
			count <= (others => '0');
		elsif rising_edge(clk) then
			if ctr_localrst = '1' or timer_reset = '1' then
				count <= (others => '0');
			elsif ctr_enabled = '1' then
				count <= count + 1;
			else
				count <= count;
			end if;
		end if;
	end process;

	output: process(count, we, control, ctrl_c, compare, q_c, ctr_irq_en, ctr_enabled)
	begin
		irq         <= '0';
		q_n         <= q_c;
		ctrl_n      <= ctrl_c;
		timer_reset <= '0';

		ctrl_n(timer_reset_bit)  <= '0'; -- reset!

		if we = '1' then
			ctrl_n <= control;
		end if;

		if count = unsigned(compare) and ctr_enabled = '1' then
			if ctr_irq_en = '1' then
				irq <= '1';
			end if;
			timer_reset <= '1';
			q_n <= not q_c;
		end if;
	end process;
end architecture;

