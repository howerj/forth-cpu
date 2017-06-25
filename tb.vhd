-------------------------------------------------------------------------------
--| @file tb.vhd
--| @brief Main test bench.
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2013,2017 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| This test bench does quite a lot. It is not like normal VHDL test benches
--| in the fact that it uses configurable variables that it read in from a
--| file, which it does in an awkward but usable fashion.
--|
--| It also tests multiple modules.
--|
-------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;
use work.util.all;
use work.cpu_pkg.all;
use work.vga_pkg.all;
use work.uart_pkg.uart_core;

entity tb is
end tb;

architecture testing of tb is
	constant clock_frequency:         positive := 100_000_000;
	constant number_of_interrupts:    positive := 8;
	constant uart_baud_rate:          positive := 115200;
	constant configuration_file_name: string   := "tb.cfg";
	constant clk_period:              time     :=  1000 ms / clock_frequency;

	-- Test bench configurable options --

	type configurable_items is record
		number_of_iterations: positive;
		verbose:              boolean;
		report_number:        positive;
	end record;

	function set_configuration_items(ci: configuration_items) return configurable_items is
		variable r: configurable_items;
	begin
		r.number_of_iterations := ci(0).value;
		r.verbose              := ci(1).value > 0;
		r.report_number        := ci(2).value;
		return r;
	end function;

	constant configuration_default: configuration_items(0 to 2) := (
		(name => "Cycles  ", value => 1000),
		(name => "Verbose ", value => 1),
		(name => "LogFor  ", value => 256));

	-- Test bench configurable options --

	signal wait_flag: std_logic :=  '0';
	signal debug:     cpu_debug_interface;

	signal clk:          std_logic := '0';
	signal jitter_clk:   std_logic := '0';
	signal jitter_delay: time := 0 ns;
	signal rst:          std_logic := '0';

--	signal  cpu_wait: std_logic := '0'; -- CPU wait flag

	-- Basic I/O
	signal btnu:  std_logic := '0';  -- button up
	signal btnd:  std_logic := '0';  -- button down
	signal btnc:  std_logic := '0';  -- button centre
	signal btnl:  std_logic := '0';  -- button left
	signal btnr:  std_logic := '0';  -- button right
	signal sw:    std_logic_vector(7 downto 0) := (others => '0'); -- switches
	signal an:    std_logic_vector(3 downto 0) := (others => '0'); -- anodes   8 segment display
	signal ka:    std_logic_vector(7 downto 0) := (others => '0'); -- kathodes 8 segment display
	signal ld:    std_logic_vector(7 downto 0) := (others => '0'); -- leds

	-- UART
	signal rx:    std_logic := '0';
	signal tx:    std_logic := '0';

	-- VGA
	signal o_vga: vga_physical_interface;

	-- HID
	signal ps2_keyboard_data: std_logic := '0';
	signal ps2_keyboard_clk:  std_logic := '0';

	-- UART FIFO
	signal uart_fifo_rx_data:         std_logic_vector(7 downto 0) := (others => '0');
	signal uart_fifo_rx_fifo_empty:   std_logic := '0';
	signal uart_fifo_rx_fifo_full:    std_logic := '0';
	signal uart_fifo_rx_data_re:      std_logic := '0';
	signal uart_fifo_tx_fifo_full:    std_logic := '0';
	signal uart_fifo_tx_fifo_empty:   std_logic := '0';
	signal uart_fifo_tx_data_we:      std_logic := '0';
	signal uart_fifo_tx:              std_logic := '0';
	signal uart_fifo_rx_fifo_not_empty:   std_logic := '0';

	-- Wave form generator
	signal gen_dout:     std_logic_vector(15 downto 0) := (others => '0');

begin
---- Units under test ----------------------------------------------------------


	uut: entity work.top
	generic map(
		clock_frequency      => clock_frequency,
		uart_baud_rate       => uart_baud_rate)
	port map(
		debug       => debug,
		clk         => clk,
		-- rst      => rst,
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

	uut_shiftReg: entity work.shift_register_tb generic map(clock_frequency => clock_frequency) port map(stop => wait_flag);
	uut_timer_us: entity work.timer_us_tb       generic map(clock_frequency => clock_frequency) port map(stop => wait_flag);
	uut_edge:     entity work.edge_tb           generic map(clock_frequency => clock_frequency) port map(stop => wait_flag);
	uut_full_add: entity work.full_adder_tb     generic map(clock_frequency => clock_frequency) port map(stop => wait_flag);
	uut_function: entity work.function_tb       generic map(clock_frequency => clock_frequency) port map(stop => wait_flag);
	uut_fifo:     entity work.fifo_tb           generic map(clock_frequency => clock_frequency) port map(stop => wait_flag);
	uut_counter:  entity work.counter_tb        generic map(clock_frequency => clock_frequency) port map(stop => wait_flag);

	-- @note a more advanced test bench would send out a string and expect
	-- the same one back using a loopback circuit. For the moment this
	-- is just used for testing the SoC.
	uut_uart: work.uart_pkg.uart_core
		generic map(
			baud_rate            =>  uart_baud_rate,
			clock_frequency      =>  clock_frequency)
		port map(
			clk       =>  clk,
			rst       =>  rst,
			din       =>  x"AA",
			din_stb   =>  '1',
			tx        =>  rx,
			rx        =>  tx,
			dout_ack  =>  '0');

	-- Example usage of wave form generator
	uut_gen: entity work.gen
		generic map(
			addr_length  =>  13,
			data_length  =>  16,
			file_name    =>  "text.bin",
			file_type    =>  "bin")
		port map(
			clk          =>  clk,
			rst          =>  rst,
			ce           =>  '1',
			cr           =>  '0',
			dout         =>  gen_dout);


	uart_fifo_rx_fifo_not_empty <= not uart_fifo_rx_fifo_empty;
	uut_uart_fifo: work.uart_pkg.uart_top
	generic map(
		baud_rate => uart_baud_rate,
		clock_frequency => clock_frequency)
	port map(
		clk            =>  clk,
		rst            =>  rst,
		rx_data        =>  uart_fifo_rx_data,
		rx_fifo_empty  =>  uart_fifo_rx_fifo_empty,
		rx_fifo_full   =>  uart_fifo_rx_fifo_full,
		rx_data_re     =>  uart_fifo_rx_fifo_not_empty,
		tx_data        =>  uart_fifo_rx_data,
		tx_fifo_full   =>  uart_fifo_tx_fifo_full,
		tx_fifo_empty  =>  uart_fifo_tx_fifo_empty,
		tx_data_we     =>  uart_fifo_rx_fifo_not_empty,
		tx             =>  uart_fifo_tx,
		rx             =>  tx);

------ Simulation only processes ----------------------------------------------
	clk_process: process
		variable seed1, seed2 : positive;
		variable r : real;
	begin
		while wait_flag = '0' loop
			uniform(seed1, seed2, r);
			jitter_delay <= r * 1 ns;

			clk <= '1';
			wait for clk_period / 2;
			clk <= '0';
			wait for clk_period / 2;
		end loop;
		wait;
	end process;

	jitter_clk <= transport clk after jitter_delay;

	-- I/O settings go here.
	stimulus_process: process
		variable w: line;
		variable count: integer := 0;

		function stringify(slv: std_logic_vector) return string is
		begin
			return integer'image(to_integer(unsigned(slv)));
		end stringify;

		procedure element(l: inout line; we: boolean; name: string; slv: std_logic_vector) is
		begin
			if we then
				write(l, name & "(" & stringify(slv) & ") ");
			end if;
		end procedure;

		procedure element(l: inout line; name: string; slv: std_logic_vector) is
		begin
			element(l, true, name, slv);
		end procedure;

		-- @todo write numbers out as hexadecimal
		function reportln(debug: cpu_debug_interface; cycles: integer) return line is
			variable l: line;
		begin
			write(l, integer'image(cycles) & ": ");
			element(l, "pc",    debug.pc);
			element(l, "insn",  debug.insn);
			element(l, "daddr", debug.daddr);
			element(l, "dout",  debug.dout);
			return l;
		end function;

		variable configuration_values: configuration_items(configuration_default'range) := configuration_default;
		variable cfg: configurable_items := set_configuration_items(configuration_default);
	begin
		-- write_configuration_tb(configuration_file_name, configuration_default);
		read_configuration_tb(configuration_file_name, configuration_values);
		cfg := set_configuration_items(configuration_values);

		rst  <= '1';
		wait for clk_period * 2;
		rst  <= '0';
		for i in 0 to cfg.number_of_iterations loop
			if cfg.verbose then
				if count < cfg.report_number then
					w := reportln(debug, count);
					writeline(OUTPUT, w);
					count := count + 1;
				elsif count < cfg.report_number + 1 then
					report "Simulation continuing: Reporting turned off";
					count := count + 1;
				end if;
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

