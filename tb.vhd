-------------------------------------------------------------------------------
--| @file tb.vhd
--| @brief Main test bench.
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2013-2019 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| This test bench does quite a lot. It is not like normal VHDL test benches
--| in the fact that it uses configurable variables that it reads in from a
--| file, which it does in an awkward but usable fashion. It also has a
--| partially working way of connecting a simulated UART to STDIN/STDOUT, which
--| is a work in progress.
--|
--| It also tests multiple modules.
--|
--| @todo Optionally, read in from standard input and send the character
--| over the UART, then print out any received characters to standard out.
-------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;
use work.util.all;
use work.core_pkg.all;
use work.vga_pkg.all;

entity tb is
end tb;

architecture testing of tb is
	constant g: common_generics := (
		clock_frequency    => 100_000_000,
		asynchronous_reset => true,
		delay              => 0 ns
	);

	constant number_of_interrupts:    positive := 8;
	constant uart_baud:          positive := 115200;
	constant configuration_file_name: string   := "tb.cfg";
	constant uart_tx_time:            time     := (10*1000 ms) / 115200;
	constant uart_default_input:      std_ulogic_vector(7 downto 0) := x"AA";
	constant reset_period_us:         natural  := 1;
	constant jitter_on:               boolean  := false;

	constant clock_period:            time     := 1000 ms / g.clock_frequency;

	-- Test bench configurable options --

	type configurable_items is record
		number_of_iterations: natural; -- 0 == loop forever
		verbose:              boolean;
		report_number:        natural;
		interactive:          boolean;
		input_wait_for:       time;
	end record;

	function set_configuration_items(ci: configuration_items) return configurable_items is
		variable r: configurable_items;
	begin
		r.number_of_iterations := ci(0).value;
		r.verbose              := ci(1).value > 0;
		r.interactive          := ci(2).value > 0;
		r.input_wait_for       := ci(3).value * 1 ms;
		r.report_number        := ci(4).value;
		return r;
	end function;

	constant configuration_default: configuration_items(0 to 4) := (
		(name => "Cycles  ", value => 1000),
		(name => "Verbose ", value => 1),
		(name => "Interact", value => 0),
		(name => "InWaitMs", value => 8),
		(name => "LogFor  ", value => 256));

	-- Test bench configurable options --

	signal stop:  boolean := false;
	signal dbgi:  cpu_debug_interface;

	signal clk:          std_ulogic := '0';
	signal rst:          std_ulogic := '0';

	-- Basic I/O
	signal btnu:  std_ulogic := '0';  -- button up
	signal btnd:  std_ulogic := '0';  -- button down
	signal btnc:  std_ulogic := '0';  -- button centre
	signal btnl:  std_ulogic := '0';  -- button left
	signal btnr:  std_ulogic := '0';  -- button right
	signal sw:    std_ulogic_vector(7 downto 0) := (others => '0'); -- switches
	signal an:    std_ulogic_vector(3 downto 0) := (others => '0'); -- anodes   8 segment display
	signal ka:    std_ulogic_vector(7 downto 0) := (others => '0'); -- kathodes 8 segment display
	signal ld:    std_ulogic_vector(7 downto 0) := (others => '0'); -- leds

	-- VGA
	signal o_vga: vga_physical_interface;
	signal hsync_gone_high: boolean := false;
	signal vsync_gone_high: boolean := false;

	-- HID
	signal ps2_keyboard_data: std_ulogic := '0';
	signal ps2_keyboard_clk:  std_ulogic := '0';

	-- UART
	signal rx:                 std_ulogic := '0';
	signal tx:                 std_ulogic := '0';
	signal dout_ack, dout_stb: std_ulogic := '0';
	signal din_ack, din_stb:   std_ulogic := '0';
	signal dout:               std_ulogic_vector(7 downto 0) := (others => '0');
	signal din:                std_ulogic_vector(7 downto 0) := (others => '0');

	-- Wave form generator
	signal gen_dout:     std_ulogic_vector(15 downto 0) := (others => '0');

	shared variable cfg: configurable_items := set_configuration_items(configuration_default);

	signal configured: boolean := false;

	signal ram_cs:     std_ulogic := 'X';
	signal mem_oe:     std_ulogic := 'X'; -- negative logic
	signal mem_wr:     std_ulogic := 'X'; -- negative logic
	signal mem_adv:    std_ulogic := 'X'; -- negative logic
	signal mem_wait:   std_ulogic := 'X'; -- positive!
	signal flash_cs:   std_ulogic := 'X';
	signal flash_rp:   std_ulogic := 'X';
	signal mem_addr:   std_ulogic_vector(26 downto 1) := (others => 'X');
	signal mem_data:   std_logic_vector(15 downto 0)  := (others => 'X');

begin
---- Units under test ----------------------------------------------------------

	mem_data <= (others => '0') when mem_oe = '1' else (others => 'Z');

	uut: entity work.top
	generic map(
		g               => g,
		reset_period_us => reset_period_us,
		uart_baud  => uart_baud)
	port map(
		debug       => dbgi,
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
		ps2_keyboard_clk  => ps2_keyboard_clk,

		ram_cs    =>  ram_cs,
		mem_oe    =>  mem_oe,
		mem_wr    =>  mem_wr,
		mem_adv   =>  mem_adv,
		mem_wait  =>  mem_wait,
		flash_cs  =>  flash_cs,
		flash_rp  =>  flash_rp,
		mem_addr  =>  mem_addr,
		mem_data  =>  mem_data);

	uut_util: work.util.util_tb generic map(g => g);
	uut_vga:  work.vga_pkg.vt100_tb generic map(g => g);

	-- The "io_pins_tb" works correctly, however in GHDL 0.29, compiled under
	-- Windows, fails to simulate this component correctly, resulting
	-- in a crash. This does not affect the Linux build of GHDL. It has
	-- something to do with 'Z' values for std_logic types.

	uut_io_pins: work.util.io_pins_tb  generic map(g => g);

	uart_0_blk: block 
		signal uart_clock_rx_we, uart_clock_tx_we, uart_control_we: std_ulogic := '0';
		signal uart_reg: std_ulogic_vector(15 downto 0);	
	begin
		uart_0: work.uart_pkg.uart_core
			generic map (g => g, baud => uart_baud)
			port map (
				clk   => clk,
				rst   => rst,
				tx_di => din,
				tx_we => din_stb,
				tx_ok => din_ack,
				tx    => rx,
			
				rx    => tx,
				rx_ok => open,
				rx_nd => dout_stb,
				rx_do => dout,
				rx_re => dout_ack,
			
				reg             => uart_reg,
				clock_reg_tx_we => uart_clock_tx_we,
				clock_reg_rx_we => uart_clock_rx_we,
				control_reg_we  => uart_control_we);
	end block;

------ Simulation only processes ----------------------------------------------
	clk_process: process
		variable seed1, seed2 : positive;
		variable r : real;
		variable jit_high, jit_low: time  := 0 ns;
	begin
		while not stop loop
			if jitter_on then
				uniform(seed1, seed2, r);
				jit_high := r * g.delay;
				uniform(seed1, seed2, r);
				jit_low := r * g.delay;
				uniform(seed1, seed2, r);
				if r < 0.5 then jit_high := -jit_high; end if;
				uniform(seed1, seed2, r);
				if r < 0.5 then jit_low := -jit_low; end if;
			else
				jit_high := 0 ns;
				jit_low  := 0 ns;
			end if;
			clk <= '1';
			wait for (clock_period / 2) + jit_high;
			clk <= '0';
			wait for (clock_period / 2) + jit_low;
		end loop;
		report "clk_process end";
		wait;
	end process;

	output_process: process
		variable oline: line;
		variable c: character;
		variable have_char: boolean := true;
	begin
		wait until configured;

		if not cfg.interactive then
			report "Output turned off";
			report "output_process end";
			wait;
		end if;

		report "Writing to STDOUT";
		while not stop loop
			wait until (dout_stb = '1' or stop);
			if not stop then
				c := character'val(to_integer(unsigned(dout)));
				write(oline, c);
				have_char := true;
				if dout = x"0d" then
					writeline(output, oline);
					have_char := false;
				end if;
				wait for clock_period;
				dout_ack <= '1';
				wait for clock_period;
				dout_ack <= '0';
			end if;
		end loop;
		if have_char then
			writeline(output, oline);
		end if;
		report "output_process end";
		wait;
	end process;


	-- The Input and Output mechanism that allows the tester to
	-- interact with the running simulation needs more work, it is buggy
	-- and experimental, but demonstrates the principle - that a VHDL
	-- test bench can be interacted with at run time.
	input_process: process
		variable c: character := ' ';
		variable iline: line;
		-- variable oline: line;
		variable good: boolean := true;
		variable eoi:  boolean := false;
	begin
		din_stb <= '0';
		din     <= x"00";
		wait until configured;
		if not cfg.interactive then
			din_stb <= '1';
			din     <= uart_default_input;
			report "input process non-interactive";
			report "input_process end";
			wait;
		end if;

		report "Waiting for " & time'image(cfg.input_wait_for) & " (before reading from STDIN)";
		wait for cfg.input_wait_for;
		report "Reading from STDIN (Hit EOF/CTRL-D/CTRL-Z After entering a line)";
		while (not endfile(input)) and not stop and eoi = false loop
			report "readline...";
			readline(input, iline);
			good := true;
			while good and not stop loop
				read(iline, c, good);
				if good then
					report "" & c;
				else
					report "EOL/EOI";
					c   := LF;
					eoi := true;
				end if;
				din <= std_ulogic_vector(to_unsigned(character'pos(c), din'length));
				din_stb <= '1';
				wait for clock_period;
				din_stb <= '0';
				wait for 100 us;
			end loop;
		end loop;
		report "input_process end";
		wait;
	end process;

	hsync_gone_high <= true when o_vga.hsync = '1' else hsync_gone_high;
	vsync_gone_high <= true when o_vga.vsync = '1' else vsync_gone_high;

	-- I/O settings go here.
	stimulus_process: process
		variable w: line;
		variable count: integer := 0;

		function stringify(slv: std_ulogic_vector) return string is
		begin
			return integer'image(to_integer(unsigned(slv)));
		end stringify;

		procedure element(l: inout line; we: boolean; name: string; slv: std_ulogic_vector) is
		begin
			if we then
				write(l, name & "(" & stringify(slv) & ") ");
			end if;
		end procedure;

		procedure element(l: inout line; name: string; slv: std_ulogic_vector) is
		begin
			element(l, true, name, slv);
		end procedure;

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
	begin
		-- write_configuration_tb(configuration_file_name, configuration_default);
		read_configuration_tb(configuration_file_name, configuration_values);
		cfg := set_configuration_items(configuration_values);
		configured <= true;

		rst  <= '1';
		wait for clock_period * 2;
		rst  <= '0';

		if cfg.number_of_iterations = 0 then
			report "RUNNING FOREVER: number of iterations is zero" severity warning;
			report "stimulus_process end";
			wait;
		end if;

		for i in 0 to cfg.number_of_iterations loop
			if cfg.verbose then
				if count < cfg.report_number then
					w := reportln(dbgi, count);
					writeline(OUTPUT, w);
					count := count + 1;
				elsif count < cfg.report_number + 1 then
					report "Simulation continuing: Reporting turned off";
					count := count + 1;
				end if;
			end if;
			wait for clock_period * 1;
		end loop;

		-- It would be nice to test the other peripherals as
		-- well, the CPU-ID should be written to the LED 7 Segment
		-- displays, however we only get the cathode and anode
		-- values out of the unit.

		assert hsync_gone_high report "HSYNC not active - H2 failed to initialize VGA module";
		assert vsync_gone_high report "VSYNC not active - H2 failed to initialize VGA module";

		stop   <=  true;
		report "stimulus_process end";
		wait;
	end process;

end architecture;
------ END ---------------------------------------------------------------------

