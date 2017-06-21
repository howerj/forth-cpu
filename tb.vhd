-------------------------------------------------------------------------------
--| @file tb.vhd
--| @brief Main test bench.
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2013 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| @todo Optionally read expected inputs and commands from a file, which
--| should include options for: Clock jitter, list of signals to log, number
--| of iterations to run for and number of iterations to log.
--|
-------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.util.shift_register_tb;
use work.util.timer_us_tb;
use work.util.full_adder_tb;
use work.util.function_tb;
use work.util.fifo_tb;
use work.cpu_pkg.all;
use work.vga_pkg.all;

entity tb is
end tb;

architecture testing of tb is
	constant clock_frequency:      positive := 100_000_000;
	constant number_of_interrupts: positive := 8;
	constant uart_baud_rate:       positive := 115200;
	constant number_of_iterations: positive := 1000;
	constant report_number:        positive := 256;

	constant clk_period: time   :=  1000 ms / clock_frequency;

	signal  wait_flag: std_logic :=  '0';
	signal  debug:     cpu_debug_interface;

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
	signal  o_vga: vga_physical_interface;

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
		debug       => debug,
		clk         => clk,
		-- reset    => rst,
		btnu        => btnu,
		btnd        => btnd,
		btnc        => btnc,
		btnl        => btnl,
		btnr        => btnr,
		sw          => sw,
		an          => an,
		ka          => ka,
		ld          => ld,
		rx          => rx,
		tx          => tx,
		o_vga       => o_vga,

		ps2_keyboard_data => ps2_keyboard_data,
		ps2_keyboard_clk  => ps2_keyboard_clk);

	uut_shiftReg: entity work.shift_register_tb generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag);
	uut_timer_us: entity work.timer_us_tb       generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag);
	uut_edge:     entity work.edge_tb           generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag);
	uut_full_add: entity work.full_adder_tb     generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag);
	uut_function: entity work.function_tb       generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag); 
	uut_fifo:     entity work.fifo_tb           generic map(clock_frequency => clock_frequency) port map(clk => clk, rst => rst, stop => wait_flag);

	-- @note a more advanced test bench would send out a string and expect
	-- the same one back using a loopback circuit. For the moment this
	-- is just used for testing the SoC.
	uut_uart: entity work.uart generic map(baud_rate => uart_baud_rate, clock_frequency => clock_frequency) 
	port map(clk => clk, rst => rst, data_stream_in => x"AA", data_stream_in_stb => '1', tx => rx, rx => tx, data_stream_out_ack => '0');

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
		variable w: line;
		variable count: integer := 0;

		function stringify(slv: std_logic_vector) return string is
		begin
			return integer'image(to_integer(unsigned(slv)));
		end stringify;

		-- @todo write numbers out as hexadecimal
		function reportln(debug: cpu_debug_interface; cycles: integer) return line is
			variable l: line;
		begin
			write(l, integer'image(cycles) & ": ");
			write(l, "pc("    & stringify(debug.pc)    & ") ");
			write(l, "insn("  & stringify(debug.insn)  & ") ");
			write(l, "daddr(" & stringify(debug.daddr) & ") ");
			write(l, "dout("  & stringify(debug.dout)  & ") ");
			return l;
		end reportln;
	begin
		rst  <= '1';
		wait for clk_period * 2;
		rst  <= '0';
		for i in 0 to number_of_iterations loop
			if count < report_number then
				w := reportln(debug, count);
				writeline(OUTPUT, w);
				count := count + 1;
			elsif count < report_number + 1 then
				report "Simulation continuing: Reporting turned off";
				count := count + 1;
			end if;
			wait for clk_period * 1;
		end loop;

		assert o_vga.hsync = '1' report "HSYNC not active - H2 failed to initialize VGA module";
		assert o_vga.vsync = '1' report "VSYNC not active - H2 failed to initialize VGA module";

		wait_flag   <=  '1';
		wait;
	end process;

------ END ---------------------------------------------------------------------
end architecture;

