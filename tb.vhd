-------------------------------------------------------------------------------
--| @file tb.vhd
--| @brief Main test bench.
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2013 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| @todo Optionally read expected inputs and commands from a file.
--|
-------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.util.shift_register_tb;
use work.util.timer_us_tb;

entity tb is
end tb;

architecture testing of tb is
	constant clock_frequency:      positive := 100000000;
	constant number_of_interrupts: positive := 8;
	constant uart_baud_rate:       positive := 115200;
	constant number_of_iterations: positive := 256;

	constant clk_period: time   :=  1000 ms / clock_frequency;

	signal  wait_flag:   std_logic :=  '0';
	signal  debug_irq:   std_logic :=  '0';
	signal  debug_irc:   std_logic_vector(3 downto 0) := (others => '0');
	signal  debug_pc:    std_logic_vector(12 downto 0);
	signal  debug_insn:  std_logic_vector(15 downto 0);
	signal  debug_dwe:   std_logic := '0';
	signal  debug_dre:   std_logic := '0';
	signal  debug_din:   std_logic_vector(15 downto 0);
	signal  debug_dout:  std_logic_vector(15 downto 0);
	signal  debug_daddr: std_logic_vector(12 downto 0);

	signal  clk:   std_logic := '0';
	signal  rst:   std_logic := '0';

--  signal  cpu_wait: std_logic := '0'; -- CPU wait flag

	-- Basic I/O
	signal  btnu:  std_logic := '0';  -- button up
	signal  btnd:  std_logic := '0';  -- button down
	signal  btnc:  std_logic := '0';  -- button centre
	signal  btnl:  std_logic := '0';  -- button left
	signal  btnr:  std_logic := '0';  -- button right
	signal  sw:    std_logic_vector(7 downto 0) := (others => '0'); -- switches
	signal  an:    std_logic_vector(3 downto 0) := (others => '0'); -- anodes   8 segment display
	signal  ka:    std_logic_vector(7 downto 0) := (others => '0'); -- kathodes 8 segment display
	signal  ld:    std_logic_vector(7 downto 0) := (others => '0'); -- leds

	-- UART
	signal  rx:    std_logic := '0'; 
	signal  tx:    std_logic := '0';

	-- VGA
	signal  red:   std_logic_vector(2 downto 0) := (others => '0');
	signal  green: std_logic_vector(2 downto 0) := (others => '0');
	signal  blue:  std_logic_vector(1 downto 0) := (others => '0');
	signal  hsync: std_logic := '0';
	signal  vsync: std_logic := '0';

	-- HID
	signal  ps2_keyboard_data: std_logic := '0';
	signal  ps2_keyboard_clk:  std_logic := '0';

begin
---- Units under test ----------------------------------------------------------

	uut: entity work.top
	generic map(
		clock_frequency      => clock_frequency,
		number_of_interrupts => number_of_interrupts,
		uart_baud_rate       => uart_baud_rate)
	port map(
		debug_irq   => debug_irq,
		debug_irc   => debug_irc,
		debug_pc    => debug_pc,
		debug_insn  => debug_insn,
		debug_dwe   => debug_dwe,
		debug_dre   => debug_dre,
		debug_din   => debug_din,
		debug_dout  => debug_dout,
		debug_daddr => debug_daddr,
		clk  => clk,
		btnu => btnu,
		btnd => btnd,
		btnc => btnc,
		btnl => btnl,
		btnr => btnr,
		sw => sw,
		an => an,
		ka => ka,
		ld => ld,
		rx => rx,
		tx => tx,
		red => red,
		green => green,
		blue  => blue,
		hsync => hsync,
		vsync => vsync,

		ps2_keyboard_data => ps2_keyboard_data,
		ps2_keyboard_clk  => ps2_keyboard_clk);

	uut_shiftReg: entity work.shift_register_tb generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag);
	uut_timer_us: entity work.timer_us_tb generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag);

------ Simulation only processes ----------------------------------------------
	clk_process: process
	begin
		while wait_flag = '0' loop
			clk <= '1';
			wait for clk_period / 2;
			clk <= '0';
			wait for clk_period / 2;
		end loop;
		wait;
	end process;

	-- I/O settings go here.
	stimulus_process: process
		variable rt: boolean;

		-- @todo write numbers out as hex, with a cleaner output format
		function reportln(pc, insn: std_logic_vector) return boolean is
		begin
			report
				"pc("   & integer'image(to_integer(unsigned(pc)))    &") " &
				"insn(" & integer'image(to_integer(unsigned(insn)))  & ") ";
			return true;
		end reportln;

	begin
		rst <= '1';
		wait for clk_period * 2;
		rst <= '0';
		for i in 0 to number_of_iterations loop
			rt := reportln(debug_pc, debug_insn);
			assert rt = true;
			wait for clk_period * 1;
		end loop;

		assert hsync = '1' report "HSYNC not active - H2 failed to initialize VGA module";
		assert vsync = '1' report "VSYNC not active - H2 failed to initialize VGA module";

		wait_flag   <=  '1';
		wait;
	end process;

------ END ---------------------------------------------------------------------
end architecture;

