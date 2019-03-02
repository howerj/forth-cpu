-------------------------------------------------------------------------------
--| @file vga.vhd
--| @brief      Text Mode Video Controller VHDL Module and VT100
--|             Terminal Emulator
--| @author     Javier Valcarce Garc�a
--| @author     Richard James Howe
--| @copyright  Copyright 2007 Javier Valcarce Garc�a, 2017, 2019 Richard James Howe
--| @license    LGPL version 3
--| @email      javier.valcarce@gmail.com
--| @note       (Modifications and repackaging by Richard James Howe)
--|
--| This is a modified version of the text terminal available at
--| <https://opencores.org/project,interface_vga80x40>. Additions include per
--| character attribute information (color, bold, reverse video) and a VT100 
--| terminal interface.
--|
--| See also <http://www.javiervalcarce.eu/html/vhdl-vga80x40-en.html>.
--|
--| @todo Add scrolling by changing the base address "text_a", adding
--| a line.
--| @todo Rewrite so the code is cleaner, and FSM are used.
--| @todo SGR 11-19 could be used to select from multiple fonts in font
--| memory. We have enough space to store 5 fonts in an 16KiB block RAM.
-------------------------------------------------------------------------------

----- VGA Package -------------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

package vga_pkg is
	type vga_physical_interface is record
		red:   std_ulogic_vector(2 downto 0);
		green: std_ulogic_vector(2 downto 0);
		blue:  std_ulogic_vector(1 downto 0);
		hsync: std_ulogic;
		vsync: std_ulogic;
	end record;

	type vga_control_registers_we_interface is record
		crx: std_ulogic; -- Write enable for cursor X position register
		cry: std_ulogic; -- Write enable for VGA control register
		ctl: std_ulogic; -- Write enable for cursor Y position register
	end record;

	type vga_control_registers_interface is record
		crx:    std_ulogic_vector(6 downto 0); -- Cursor position X
		cry:    std_ulogic_vector(5 downto 0); -- Cursor position Y
		ctl:    std_ulogic_vector(4 downto 0); -- Control register
	end record;

	constant vga_control_registers_initialize: vga_control_registers_interface := (
			cry => (others => '0'),
			crx => (others => '0'),
			ctl => (others => '0'));

	constant vga_control_registers_we_initialize: vga_control_registers_we_interface := (
			cry => '0',
			crx => '0',
			ctl => '0');

	component vga_top is
	generic(g: common_generics);
	port(
		clk:         in  std_ulogic;
		clk25MHz:    in  std_ulogic;
		rst:         in  std_ulogic;

		-- VGA Text buffer interface
		vga_we_ram:  in  std_ulogic; -- Write enable RAM
		vga_din:     in  std_ulogic_vector(15 downto 0);
		vga_addr:    in  std_ulogic_vector(12 downto 0);

		-- VGA control registers
		i_vga_control_we: in vga_control_registers_we_interface;
		i_vga_control:    in vga_control_registers_interface;

		o_vga:    out vga_physical_interface);
	end component;

	-- VGA test bench, not-synthesizeable
	component vga_core is
	generic(g: common_generics);
	port (
		rst:       in  std_ulogic;
		clk25MHz:  in  std_ulogic;
		text_a:    out std_ulogic_vector(11 downto 0); -- text buffer
		text_d:    in  std_ulogic_vector(15 downto 0);
		font_a:    out std_ulogic_vector(11 downto 0); -- font buffer
		font_d:    in  std_ulogic_vector( 7 downto 0);
		 --
		ocrx:      in  std_ulogic_vector(6 downto 0);
		ocry:      in  std_ulogic_vector(5 downto 0);
		octl:      in  std_ulogic_vector(3 downto 0);
		--
		o_vga: out vga_physical_interface);
	end component;

	component losr is
	generic (g: common_generics; N: positive := 4);
	port
	(
		rst:  in  std_ulogic;
		clk:  in  std_ulogic;
		load: in  std_ulogic;
		ce:   in  std_ulogic;
		do:   out std_ulogic := '0';
		di:   in  std_ulogic_vector(N - 1 downto 0));
	end component;

	component ctrm is
	generic (g: common_generics; M: positive := 8);
	port (
		rst: in  std_ulogic; -- asynchronous rst
		clk: in  std_ulogic;
		ce:  in  std_ulogic; -- enable counting
		rs:  in  std_ulogic; -- synchronous rst
		do:  out integer range (M-1) downto 0 := 0);
	end component;

	component vt100 is
	generic (g: common_generics);
	port(
		clk:        in  std_ulogic;
		clk25MHz:   in  std_ulogic;
		rst:        in  std_ulogic;
		we:         in  std_ulogic;
		char:       in  std_ulogic_vector(7 downto 0);

		busy:       out std_ulogic;
		o_vga:      out vga_physical_interface);
	end component;

	component vt100_tb is
		generic(g: common_generics);
	end component;

	component atoi is
		generic(g: common_generics; N: positive := 16);
		port(
			clk:    in  std_ulogic;
			rst:    in  std_ulogic;
			we:     in  std_ulogic;
			init:   in  std_ulogic;
			char:   in  std_ulogic_vector(7 downto 0);
			char_o: out std_ulogic_vector(7 downto 0);
			done_o: out std_ulogic;
			ready:  out std_ulogic;
			n_o:    out unsigned(N - 1 downto 0));
	end component;
end package;

----- VGA Package -------------------------------------------------------------

----- VGA Test Bench ----------------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.vga_pkg.all;
use work.util.all;

entity vt100_tb is
	generic(g: common_generics);
end entity;

architecture behav of vt100_tb is
	constant clock_period: time := 1000 ms / g.clock_frequency;
	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
	signal clk25MHz, rst25MHz: std_ulogic := '0';

	signal char:     std_ulogic_vector(7 downto 0);
	signal we:       std_ulogic := '0';
	signal busy:     std_ulogic := '0';
	signal physical: vga_physical_interface;

	-- NB. 'string' range is (1 to X), not (X-1 downto 0)
	-- HT = Horizontal Tab
	-- LF = Line Feed
	-- CR = Carriage Return
	-- ESC = Escape
	constant CSI:   string := ESC & "[";
	constant RESET: string := CSI & "0m";
	constant RED:   string := CSI & "31m";
	constant GREEN: string := CSI & "32m";
	constant NL:    string := CR & LF;
	constant test_string: string := "Hello!" & HT & "World!" & RED & NL & "How are you?" & RESET & NL ;
	shared variable test_vector: ulogic_string(test_string'range) := to_std_ulogic_vector(test_string);
	shared variable index: integer := 1; -- starts at '1' due to string range

	constant g_cs25MHz: common_generics := (clock_frequency => 25_000_000, asynchronous_reset => g.asynchronous_reset, delay => g.delay);
begin
	cs: entity work.clock_source_tb
		generic map(g => g, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	cs25MHz: entity work.clock_source_tb
		generic map(g => g_cs25MHz, hold_rst => 2)
		port map(stop => stop, clk => clk25MHz, rst => rst25MHz);


	uut: work.vga_pkg.vt100
	generic map(g => g)
	port map(
		clk      => clk,
		clk25MHz => clk25MHz,
		rst      => rst,
		we       => we,
		char     => char,
		busy     => busy,
		o_vga    => physical);

	stimulus_process: process
	begin
		char <= test_vector(index);
		we <= '0';
		wait for clock_period * 20;

		for i in test_vector'range loop
			we <= '1';
			char <= test_vector(index);
			wait for clock_period;
			we <= '0';
			wait for clock_period;

			while busy = '1' loop
				wait until busy = '0';
				wait for clock_period;
			end loop;

			-- wait for clock_period * 20;
			-- wait until busy = '0';
			-- report integer'image(index);
			if index < (test_vector'high - 1) then
				index := index + 1;
			else
				index := 1;
			end if;
		end loop;

		stop <= '1';
		wait;
	end process;
end architecture;

----- VGA Test Bench ----------------------------------------------------------

----- ATOI --------------------------------------------------------------------
-- The purpose of this module is to read in numeric characters ('0' through
-- '9') and convert the string into a binary number ('n_o'). On the first non-
-- numeric character the module stops and outputs that non-numeric character
-- as well as the converted number string. The module waits in the FINISHED
-- state until the module is reset by an external signal ('init').
--
-- The module is named after the C function, 'atoi', for converting a string
-- into an integer.
--

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vga_pkg.all;
use work.util.common_generics;

entity atoi is
	generic(g: common_generics; N: positive := 16);
	port(
		clk:    in  std_ulogic;
		rst:    in  std_ulogic;
		we:     in  std_ulogic;
		init:   in  std_ulogic;
		char:   in  std_ulogic_vector(7 downto 0);
		char_o: out std_ulogic_vector(7 downto 0);
		done_o: out std_ulogic;
		ready:  out std_ulogic;
		n_o:    out unsigned(N - 1 downto 0));
end entity;

architecture rlt of atoi is
	type state_type is (RESET, WAITING, COMMAND, ACCUMULATE, WRITE, FINISHED);
	signal state_c, state_n: state_type   := RESET;
	signal c_c, c_n: unsigned(char'range) := (others => '0');
	signal n_c, n_n: unsigned(n_o'range)  := (others => '0');
	signal f_c, f_n: boolean              := true;
begin
	char_o <= std_ulogic_vector(c_c);
	n_o    <= n_c;
	ready  <= '1' when state_c = WAITING else '0';

	process(clk, rst)
		variable akk: unsigned((2 * N) - 1 downto 0) := (others => '0');
	begin
		if rst = '1' and g.asynchronous_reset then
			state_n   <= RESET;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				state_n <= RESET;
			else
				state_c <= state_n;
				c_c     <= c_n;
				n_c     <= n_n;
				f_c     <= f_n;
				done_o  <= '0';

				if state_c = RESET then
					c_n     <= (others => '0');
					n_n     <= (others => '0');
					f_n     <= true;
					state_n <= WAITING;
				elsif state_c = WAITING then
					if we = '1' then
						c_n     <= unsigned(char);
						state_n <= COMMAND;
					end if;
				elsif state_c = COMMAND then
					state_n <= ACCUMULATE;
					case c_c is
					when x"30"  =>
					when x"31"  =>
					when x"32"  =>
					when x"33"  =>
					when x"34"  =>
					when x"35"  =>
					when x"36"  =>
					when x"37"  =>
					when x"38"  =>
					when x"39"  =>
					when others =>
						state_n <= WRITE;
					end case;
				elsif state_c = ACCUMULATE then
					if f_c then
						f_n <= false;
						n_n <= n_c + (c_c - x"30");
					else
						akk := n_c * to_unsigned(10, N);
						n_n <= akk(N - 1 downto 0) + (c_c - x"30");
					end if;
					state_n <= WAITING;
				elsif state_c = WRITE then
					done_o  <= '1';
					state_n <= FINISHED;
				elsif state_c = FINISHED then
					null; -- wait for a reset
				else
					state_n <= RESET;
					report "Invalid State" severity failure;
				end if;

				if init = '1' then
					assert state_c = FINISHED report "Lost Conversion" severity error;
					state_n <= RESET;
				end if;
			end if;
		end if;
	end process;

end architecture;
----- ATOI --------------------------------------------------------------------

----- VT100 Terminal Emulator -------------------------------------------------
-- This module contains the VGA module and wraps it in a terminal emulator,
-- based on the VT100 <https://en.wikipedia.org/wiki/VT100>, it only implements
-- a subset of the commands supplied by the VT100. This simplifies the usage
-- of the VGA text mode display, other VHDL components only have to write bytes
-- and do not have to worry about cursor position or implementing new lines,
-- tabs, and other very basic features.
--
-- The interface is designed to act like a UART, simply write a byte to it
-- and so long as the interface is not busy, it will be written to the screen
-- (or interpreter as part of a command sequence).

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vga_pkg.all;
use work.util.common_generics;

entity vt100 is
	generic(g: common_generics);
	port(
		clk:        in  std_ulogic;
		clk25MHz:   in  std_ulogic;
		rst:        in  std_ulogic;
		we:         in  std_ulogic;
		char:       in  std_ulogic_vector(7 downto 0);

		busy:       out std_ulogic;
		o_vga:      out vga_physical_interface);
end entity;

-- A better way of structuring this would be to process numbers in parallel
-- with different components, one processing potential attributes, one holding
-- one to two numbers - the results are then used. This would deal with runs
-- of attributes, which is allowed.
architecture rtl of vt100 is
	constant width:  positive := 80;
	constant height: positive := 40;
	constant number: positive := 8;

	type state_type is (RESET, ACCEPT, NORMAL, WRAP, LIMIT, CSI, COMMAND,
	NUMBER1, NUMBER2, COMMAND1, COMMAND2, WRITE, ERASING, ATTRIB, ADVANCE);
	signal state_c, state_n: state_type := RESET;

	constant esc:          unsigned(char'range) := x"1b";
	constant lsqb:         unsigned(char'range) := x"5b"; -- '['
	constant tab:          unsigned(char'range) := x"09";
	constant cr:           unsigned(char'range) := x"0d";
	constant lf:           unsigned(char'range) := x"0a";
	constant backspace:    unsigned(char'range) := x"08";
	constant blank:        unsigned(char'range) := x"20";
	constant asterisk:     unsigned(char'range) := x"2A";
	constant ascii_c:      unsigned(char'range) := x"63"; -- c
	constant attr_default: unsigned(7 downto 0) := "00111000";
	constant ctl_default:  unsigned(4 downto 0) := "01111";

	signal addr:           std_ulogic_vector(12 downto 0) := (others => '0');
	signal data_we:        std_ulogic                     := '0';
	signal x:              std_ulogic_vector(6 downto 0)  := (others => '0');
	signal y:              std_ulogic_vector(5 downto 0)  := (others => '0');
	signal cursor_we:      std_ulogic                     := '0';

	signal x_n, x_c:       unsigned(x'range)    := (others => '0');
	signal y_n, y_c:       unsigned(y'range)    := (others => '0');
	signal c_n, c_c:       unsigned(char'range) := (others => '0');

	signal attr_c, attr_n: unsigned(attr_default'range) := attr_default;
	signal ctl_c, ctl_n:   unsigned(ctl_default'range)  := ctl_default;

	signal n1_n, n1_c:     unsigned(7 downto 0) := (others => '0');
	signal n2_n, n2_c:     unsigned(6 downto 0) := (others => '0');

	signal conceal_n, conceal_c: boolean := false;

	signal x_minus_one:         unsigned(x'range) := (others => '0');
	signal x_minus_one_limited: unsigned(x'range) := (others => '0');
	signal x_underflow:         boolean           := false;
	signal x_plus_one:          unsigned(x'range) := (others => '0');
	signal x_plus_one_limited:  unsigned(x'range) := (others => '0');
	signal x_overflow:          boolean           := false;

	signal y_minus_one:         unsigned(y'range) := (others => '0');
	signal y_minus_one_limited: unsigned(y'range) := (others => '0');
	signal y_underflow:         boolean           := false;
	signal y_plus_one:          unsigned(y'range) := (others => '0');
	signal y_plus_one_limited:  unsigned(y'range) := (others => '0');
	signal y_overflow:          boolean           := false;

	signal count_c, count_n:    unsigned(addr'range) := (others => '0');
	signal limit_c, limit_n:    unsigned(addr'high - 3 downto 0) := (others => '0');
	-- signal base_c, base_n:      std_ulogic_vector(addr'range) := (others => '0');

	signal akk_done_o:  std_ulogic := '0';
	signal akk_ready_o: std_ulogic := '0';
	signal akk_init:    std_ulogic := '0';
	signal n_o:         unsigned(number - 1 downto 0) := (others => '0');
	signal akk_char_o:  std_ulogic_vector(char'range)  := (others => '0');

begin
	accumulator_0: work.vga_pkg.atoi
		generic map(g => g, N => number)
		port map(
			clk    => clk,
			rst    => rst,
			we     => we,
			init   => akk_init,
			char   => char,
			char_o => akk_char_o,
			done_o => akk_done_o,
			ready  => akk_ready_o,
			n_o    => n_o);

	address: block
		signal addr_int: unsigned(addr'range) := (others => '0');
		signal mul: unsigned(15 downto 0)     := (others => '0');
	begin
		mul      <= to_unsigned(to_integer(y_c) * width, mul'length);
		addr_int <= mul(addr_int'range) + ("000000" & x_c);
		-- addr_int <= mul(addr_int'range) + ("000000" & x_c) + unsigned(base_c);
		addr     <= std_ulogic_vector(addr_int) when state_c /= ERASING else std_ulogic_vector(count_c);
	end block;

	x_minus_one         <= x_c - 1;
	x_plus_one          <= x_c + 1;
	x_underflow         <= x_minus_one >= (width  - 1);
	x_overflow          <= x_c         >  (width  - 1);
	x_minus_one_limited <= (others => '0') when x_underflow else x_minus_one;
	x_plus_one_limited  <= to_unsigned(width - 1, x_c'length) when x_overflow else x_plus_one;

	y_plus_one          <= y_c + 1;
	y_minus_one         <= y_c - 1;
	y_overflow          <= y_c         >= (height - 1);
	y_underflow         <= y_minus_one >  (height - 1);
	y_minus_one_limited <= (others => '0') when y_underflow else y_minus_one;
	y_plus_one_limited  <= to_unsigned(height - 1, y_c'length) when y_overflow else y_plus_one;

	busy                <= '0' when akk_ready_o = '0'
				or state_c = ACCEPT
				or state_c = CSI
				or state_c = NUMBER1
				or state_c = NUMBER2 else '1';

	vga_blk: block
		signal vga_din:    std_ulogic_vector(15 downto 0)     := (others => '0');
		signal vga_ctr_we: vga_control_registers_we_interface := vga_control_registers_we_initialize;
		signal vga_ctr:    vga_control_registers_interface    := vga_control_registers_initialize;
		signal attr:       unsigned(attr_c'range)             := attr_default;
		signal ch:         std_ulogic_vector(c_c'range)       := (others => '0');
	begin
		ch             <= std_ulogic_vector(asterisk) when conceal_c else std_ulogic_vector(c_c);
		attr           <= attr_c when state_c /= ERASING else attr_default;
		vga_din        <= std_ulogic_vector(attr) & ch;
		vga_ctr.crx    <= std_ulogic_vector(x_plus_one); -- not limited, goes off screen edge
		vga_ctr.cry    <= std_ulogic_vector(y_c);
		vga_ctr.ctl    <= std_ulogic_vector(ctl_c);
		vga_ctr_we.crx <= cursor_we;
		vga_ctr_we.cry <= cursor_we;
		vga_ctr_we.ctl <= cursor_we;

		vga_0: work.vga_pkg.vga_top
		generic map(g => g)
		port map(
			clk               =>  clk,
			clk25MHz          =>  clk25MHz,
			rst               =>  rst,
			vga_we_ram        =>  data_we,
			vga_din           =>  vga_din,
			vga_addr          =>  addr,
			i_vga_control_we  =>  vga_ctr_we,
			i_vga_control     =>  vga_ctr,
			o_vga             =>  o_vga);
	end block;

	-- Subset of commands implemented:
	-- ED  - Erase Display, CSI n 'J'
	-- RIS - Erase Display, ESC 'c'
	-- SGR - Select Graphic Rendition - for colors, CSI n 'm'
	-- HVP - Horizontal and Vertical Position - CSI n ; m 'f'
	-- The cursor commands are also supported: CUU, CUD, CUF,
	-- CUB, CNL, CPL and CHA
	fsm: process(clk, rst)
		variable limit_value: unsigned(addr'range) := (others => '0');
		variable repeat:      boolean    := false;
		variable exit_repeat: state_type := RESET;
	begin
		if rst = '1' and g.asynchronous_reset then
			state_c <= RESET;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				state_c <= RESET;
			else
				x_c       <= x_n;
				y_c       <= y_n;
				c_c       <= c_n;
				count_c   <= count_n;
				limit_c   <= limit_n;
				state_c   <= state_n;
				n1_c      <= n1_n;
				n2_c      <= n2_n;
				data_we   <= '0';
				cursor_we <= '0';
				akk_init  <= '0';
				attr_c    <= attr_n;
				ctl_c     <= ctl_n;
				conceal_c <= conceal_n;
				-- base_c    <= base_n;

				if state_c = RESET then
					n1_n      <= (others => '0');
					n2_n      <= (others => '0');
					x_n       <= (others => '0');
					y_n       <= (others => '0');
					c_n       <= (others => '0');
					count_n   <= (others => '0');
					limit_n   <= (others => '0');
					state_n   <= ACCEPT;
					attr_n    <= attr_default;
					ctl_n     <= ctl_default;
					conceal_n <= false;
					-- base_n    <= (others => '0');
				elsif state_c = ACCEPT then
					if we = '1' then
						c_n   <= unsigned(char);
						state_n <= NORMAL;
					end if;
				elsif state_c = NORMAL then
					case c_c is
					when tab =>
						x_n     <= (x_c and "1111000") + 8;
						state_n <= ERASING;
						c_n     <= blank;
						limit_value := unsigned(addr and "1111111111000") + 8;
						limit_n <= limit_value(limit_n'high + 3 downto limit_n'low + 3);
						count_n <= unsigned(addr);
					when cr =>
						y_n     <= y_plus_one;
						state_n <= WRAP;
					when lf =>
						x_n     <= (others => '0');
						state_n <= WRITE;
					when backspace =>
						x_n     <= x_minus_one_limited;
						state_n <= WRITE;
					when esc =>
						state_n <= CSI;
					when others =>
						data_we <= '1';
						state_n <= ADVANCE;
					end case;
				elsif state_c = ADVANCE then
					x_n     <= x_plus_one;
					state_n <= WRAP;
				elsif state_c = CSI then
					if we = '1' then
						c_n <= unsigned(char);
						state_n <= COMMAND;
					end if;
				elsif state_c = COMMAND then
					case c_c is
					when ascii_c => -- @todo Erase screen as well
						state_n <= RESET;
					when lsqb =>
						state_n  <= NUMBER1;
						akk_init <= '1';
					when others => -- Error
						state_n <= ACCEPT;
					end case;
				elsif state_c = NUMBER1 then
					if akk_done_o = '1' then
						state_n <= COMMAND1;
						n1_n    <= n_o(n1_n'range);
					end if;
				elsif state_c = COMMAND1 then
					repeat := false;
					case akk_char_o is
					when x"41" => -- CSI n 'A' : CUU Cursor up
						y_n         <= y_minus_one_limited;
						exit_repeat := WRITE;
						repeat      := true;
					when x"42" => -- CSI n 'B' : CUD Cursor Down
						y_n         <= y_plus_one_limited;
						exit_repeat := LIMIT;
						repeat      := true;
					when x"43" => -- CSI n 'C' : CUF Cursor Forward
						x_n         <= x_plus_one_limited;
						exit_repeat := LIMIT;
						repeat      := true;
					when x"44" => -- CSI n 'D' : CUB Cursor Back
						x_n         <= x_minus_one_limited;
						exit_repeat := WRITE;
						repeat      := true;
					when x"45" => -- CSI n 'E'
						y_n         <= y_minus_one_limited;
						x_n         <= (others => '0');
						exit_repeat := WRITE;
						repeat      := true;
					when x"46" => -- CSI n 'F'
						y_n         <= y_plus_one_limited;
						x_n         <= (others => '0');
						exit_repeat := LIMIT;
						repeat      := true;
					when x"47" => -- CSI n 'G' : CHA Cursor Horizontal Absolute
						x_n      <= n1_c(x_n'range);
						state_n  <= LIMIT;
					when x"4a" => -- CSI n 'J'
						x_n       <= (others => '0');
						y_n       <= (others => '0');
						cursor_we <= '1';
						state_n   <= ERASING;
						c_n       <= blank;
						count_n   <= (others => '0');
						limit_n   <= "1000000000";
					when x"3b" => -- ESC n ';' ...
						state_n <= NUMBER2;
						akk_init <= '1';
					when x"6d" => -- CSI n 'm' : SGR
						state_n <= ATTRIB;
					when x"53" => -- CSI n 'S' : scroll up
						ctl_n(4) <= '0';
						state_n  <= WRITE;
					when x"54" => -- CSI n 'T' : scroll down
						ctl_n(4) <= '1';
						state_n  <= WRITE;

					-- CSI ? NUMBER ('h' or 'l')
					-- when x"3f" => -- ESC ? 25 (l,h)
					--	state_n  <= NUMBER2;
					--	akk_init <= '1';

					-- @warning This is an extension! It is for setting the
					-- control lines of the VGA module directly.
					when x"78" => -- ESC n 'x' : Set VGA control registers directly
						ctl_n    <= n1_c(ctl_n'range);
						state_n  <= WRITE;
					when others => -- Error
						state_n <= ACCEPT;
					end case;

					if repeat then
						if n1_c = 0 then
							state_n <= exit_repeat;
						else
							n1_n <= n1_c - 1;
						end if;
					end if;
				elsif state_c = NUMBER2 then
					if akk_done_o = '1' then
						state_n <= COMMAND2;
						n2_n    <= n_o(n2_n'range);
					end if;
				elsif state_c = COMMAND2 then
					case akk_char_o is
					when x"66" => -- f
						y_n       <= n1_c(y_n'range) - 1;
						x_n       <= n2_c(x_n'range) - 1;
						cursor_we <= '1';
						state_n   <= LIMIT;
					when x"48" => -- H
						y_n       <= n1_c(y_n'range) - 1;
						x_n       <= n2_c(x_n'range) - 1;
						cursor_we <= '1';
						state_n   <= LIMIT;
					when x"6d" => -- ESC n ';' m 'm'
						state_n <= ATTRIB;
					when others =>
						state_n <= ACCEPT;
					end case;
				elsif state_c = WRAP then
					if x_overflow then
						x_n <= (others => '0');
						y_n <= y_plus_one;
					elsif y_overflow then
						x_n     <= (others => '0');
						y_n     <= (others => '0');
						state_n <= ERASING;
						c_n     <= blank;
						count_n <= (others => '0');
						limit_n <= "1000000000";
						-- base_n  <= std_ulogic_vector(unsigned(base_c) + 80);
					else
						state_n <= WRITE;
					end if;
				elsif state_c = LIMIT then
					if x_overflow then
						x_n <= to_unsigned(width - 1, x_n'high + 1);
					end if;
					if y_overflow then
						y_n <= to_unsigned(height - 1, y_n'high + 1);
					end if;
					state_n <= WRITE;
				elsif state_c = WRITE then
					state_n   <= ACCEPT;
					cursor_we <= '1';
				elsif state_c = ERASING then
					if count_c(limit_c'high + 3 downto limit_c'low + 3) /= limit_c then
						count_n <= count_c + 1;
						data_we <= '1';
					else
						state_n <= WRAP;
					end if;
				elsif state_c = ATTRIB then
					case n1_c is
					when x"00"  =>
						attr_n    <= attr_default;
						conceal_n <= false;
					when x"01"  => attr_n(6) <= '1'; -- bold
					when x"07"  => attr_n(7) <= '1'; -- reverse video
					when x"08"  => conceal_n <= true;
					when x"1C"  => conceal_n <= false;

					-- CSI ? NUMBER ('h' or 'l')
					-- when x"6c" => if n2_c = x"19" then ctl_n(2) <= '0'; end if; -- l, hide cursor
					-- when x"68" => if n2_c = x"19" then ctl_n(2) <= '1'; end if; -- h, show cursor

					-- NOTE: This should make the _text_ blink, not the cursor!
					when x"05"  => ctl_n(1) <= '1'; -- blink slow
					when x"19"  => ctl_n(1) <= '0'; -- blink off

					when x"1E"  => attr_n(5 downto 3) <= "000"; -- 30
					when x"1F"  => attr_n(5 downto 3) <= "001";
					when x"20"  => attr_n(5 downto 3) <= "010";
					when x"21"  => attr_n(5 downto 3) <= "011";
					when x"22"  => attr_n(5 downto 3) <= "100";
					when x"23"  => attr_n(5 downto 3) <= "101";
					when x"24"  => attr_n(5 downto 3) <= "110";
					when x"25"  => attr_n(5 downto 3) <= "111";

					when x"28"  => attr_n(2 downto 0) <= "000"; -- 40
					when x"29"  => attr_n(2 downto 0) <= "001";
					when x"2A"  => attr_n(2 downto 0) <= "010";
					when x"2B"  => attr_n(2 downto 0) <= "011";
					when x"2C"  => attr_n(2 downto 0) <= "100";
					when x"2D"  => attr_n(2 downto 0) <= "101";
					when x"2E"  => attr_n(2 downto 0) <= "110";
					when x"2F"  => attr_n(2 downto 0) <= "111";
					when others =>
					end case;
					state_n <= ACCEPT;
				else
					state_n <= RESET;
					report "Invalid State" severity failure;
				end if;
			end if;
		end if;
	end process;
end architecture;
----- VT100 Terminal Emulator -------------------------------------------------

----- VGA Top Level Component -------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vga_pkg.all;
use work.util.file_format;
use work.util.file_hex;
use work.util.file_binary;
use work.util.common_generics;

entity vga_top is
	generic(g: common_generics);
	port(
		clk:              in  std_ulogic;
		clk25MHz:         in  std_ulogic;
		rst:              in  std_ulogic;

		-- VGA Text buffer interface
		vga_we_ram:       in  std_ulogic; -- Write enable RAM
		vga_din:          in  std_ulogic_vector(15 downto 0);
		vga_addr:         in  std_ulogic_vector(12 downto 0);

		-- VGA control registers
		i_vga_control_we: in vga_control_registers_we_interface;
		i_vga_control:    in vga_control_registers_interface;

		o_vga:            out vga_physical_interface);
end;

architecture behav of vga_top is

	-- Setup for text buffer memory
	constant text_addr_length: positive    := 13;
	constant text_data_length: positive    := 16;
	constant text_file_name:   string      := "text.hex";
	constant text_file_type:   file_format := FILE_HEX;

	-- Setup for font buffer memory
	constant font_addr_length: positive    := 12;
	constant font_data_length: positive    := 8;
	constant font_file_name:   string      := "font.bin";
	constant font_file_type:   file_format := FILE_BINARY;

	-- Internal signals for mapping output <--> VGA module
	signal  foreground_draw_internal:      std_ulogic := '0';

	-- Text RAM signals, RAM <--> VGA module
	signal  text_dout:       std_ulogic_vector(15 downto 0) := (others => '0');
	signal  text_din:        std_ulogic_vector(15 downto 0) := (others => '0');
	signal  text_addr:       std_ulogic_vector(11 downto 0) := (others => '0');
	signal  text_addr_full:  std_ulogic_vector(12 downto 0) := (others => '0');

	-- Font ROM signals, ROM<-->VGA module
	signal  font_addr:       std_ulogic_vector(11 downto 0) := (others => '0');
	signal  font_dout:       std_ulogic_vector( 7 downto 0) := (others => '0');

	signal  control_c, control_n: vga_control_registers_interface := vga_control_registers_initialize;

	signal  foreground_draw: std_ulogic := '0';
	signal  background_draw: std_ulogic := '0';
begin

	vga_ns: process(clk, rst)
	begin
		if rst = '1' and g.asynchronous_reset then
			control_c   <= vga_control_registers_initialize;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				control_c <= vga_control_registers_initialize;
			else
				control_c <= control_n;
			end if;
		end if;
	end process;

	vga_creg_we: process(
		control_c,
		i_vga_control,
		i_vga_control_we)
	begin
		control_n <= control_c;

		if i_vga_control_we.crx = '1' then control_n.crx <= i_vga_control.crx; end if;
		if i_vga_control_we.cry = '1' then control_n.cry <= i_vga_control.cry; end if;
		if i_vga_control_we.ctl = '1' then control_n.ctl <= i_vga_control.ctl; end if;
	end process;

	u_vga: work.vga_pkg.vga_core 
		generic map(g => g)
		port map (
		rst       => rst,
		clk25MHz  => clk25MHz,

		text_a    => text_addr,
		text_d    => text_dout,

		font_a    => font_addr,
		font_d    => font_dout,

		ocrx      => control_c.crx,
		ocry      => control_c.cry,
		octl      => control_c.ctl(3 downto 0),

		o_vga     => o_vga);

	text_addr_full <= control_c.ctl(4) & text_addr;

	u_text_memory: entity work.dual_port_block_ram -- holds at least 80x40 characters
	generic map(g => g,
		addr_length => text_addr_length,
		data_length => text_data_length,
		file_name   => text_file_name,
		file_type   => text_file_type)
	port map (
		a_clk   => clk,
		-- External interface
		a_dwe   => vga_we_ram,
		a_dre   => '1',
		a_addr  => vga_addr,
		a_din   => vga_din,
		a_dout  => open,
		-- Internal interface
		b_clk   => clk25MHz,
		b_dwe   => '0',
		b_dre   => '1',
		b_addr  => text_addr_full,
		b_din   => (others => '0'),
		b_dout  => text_dout);

	u_font_memory: entity work.single_port_block_ram
	generic map(g => g,
		addr_length => font_addr_length,
		data_length => font_data_length,
		file_name   => font_file_name,
		file_type   => font_file_type)
	port map (
		clk  => clk25MHz,
		dwe  => '0',
		dre  => '1',
		addr => font_addr,
		din  => (others => '0'),
		dout => font_dout);

end architecture;

----- VGA Top Level Component -------------------------------------------------

----- VGA Core ----------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vga_pkg.all;
use work.util.common_generics;

entity vga_core is
	generic(g: common_generics);
	port (
		rst:      in  std_ulogic;
		clk25MHz: in  std_ulogic;
		text_a:   out std_ulogic_vector(11 downto 0); -- text buffer
		text_d:   in  std_ulogic_vector(15 downto 0);
		font_a:   out std_ulogic_vector(11 downto 0); -- font buffer
		font_d:   in  std_ulogic_vector( 7 downto 0);
		 --
		ocrx:     in  std_ulogic_vector(6 downto 0);
		ocry:     in  std_ulogic_vector(5 downto 0);
		octl:     in  std_ulogic_vector(3 downto 0);
		--
		o_vga:    out vga_physical_interface);
end entity;

architecture rtl of vga_core is

	signal foreground_draw_int, background_draw_int: std_ulogic := '0';
	signal hsync_int: std_ulogic := '1';
	signal vsync_int: std_ulogic := '1';

	signal blank: std_ulogic := '0';
	signal hctr:  integer range 793 downto 0 := 0;
	signal vctr:  integer range 524 downto 0 := 0;

	-- character/pixel position on the screen
	signal scry:  integer range 39 downto 0 := 0; -- chr row   < 40 (6 bits)
	signal scrx:  integer range 79 downto 0 := 0; -- chr col   < 80 (7 bits)
	signal chry:  integer range 11 downto 0 := 0; -- chr high  < 12 (4 bits)
	signal chrx:  integer range 7  downto 0 := 0; -- chr width < 08 (3 bits)

	signal losr_ce: std_ulogic := '0';
	signal losr_ld: std_ulogic := '0';
	signal losr_do: std_ulogic := '0';
	signal y:       std_ulogic := '0';  -- character luminance pixel value (0 or 1)

	-- control io register
	signal ctl:       std_ulogic_vector(7 downto 0):= (others =>'0');
	signal vga_en:    std_ulogic := '0';
	signal cur_en:    std_ulogic := '0';
	signal cur_mode:  std_ulogic := '0';
	signal cur_blink: std_ulogic := '0';

	signal background_draw, foreground_draw: std_ulogic := '0';
	signal attr:      std_ulogic_vector(7 downto 0) := (others =>'0');
	signal attr_we_delay: std_ulogic := '0';
begin

	-- hsync generator, initialized with '1'
	process (rst, clk25MHz)
	begin
		if rst = '1' and g.asynchronous_reset then
			hsync_int <= '1';
		elsif rising_edge(clk25MHz) then
			if rst = '1' and not g.asynchronous_reset then
				hsync_int <= '1';
			else
				if (hctr > 663) and (hctr < 757) then
					hsync_int <= '0';
				else
					hsync_int <= '1';
				end if;
			end if;
		end if;
	end process;

	-- vsync generator, initialized with '1'
	process (rst, clk25MHz)
	begin
		if rst = '1' and g.asynchronous_reset then
			vsync_int <= '1';
		elsif rising_edge(clk25MHz) then
			if rst = '1' and not g.asynchronous_reset then
				vsync_int <= '1';
			else
				if (vctr > 499) and (vctr < 502) then
					vsync_int <= '0';
				else
					vsync_int <= '1';
				end if;
			end if;
		end if;
	end process;

	-- Blank signal, 0 = no draw, 1 = visible/draw zone

	-- Proboscide99 31/08/08
	blank <= '0' when (hctr < 8) or (hctr > 647) or (vctr > 479) else '1';

	-- flip-flips for sync of R, G y B signal, initialized with '0'
	process (rst, clk25MHz)
	begin
		if rst = '1' and g.asynchronous_reset then
			foreground_draw <= '0';
		elsif rising_edge(clk25MHz) then
			if rst = '1' and not g.asynchronous_reset then
				foreground_draw <= '0';
			else
				foreground_draw <= foreground_draw_int;
				background_draw <= background_draw_int;
			end if;
		end if;
	end process;

	-- Control register. Individual control signal

	vga_en    <= octl(3);
	cur_en    <= octl(2);
	cur_blink <= octl(1);
	cur_mode  <= octl(0);

	-- counters, hctr, vctr, srcx, srcy, chrx, chry
	counters: block

		signal hctr_ce: std_ulogic := '0';
		signal hctr_rs: std_ulogic := '0';
		signal vctr_ce: std_ulogic := '0';
		signal vctr_rs: std_ulogic := '0';

		signal chrx_ce: std_ulogic := '0';
		signal chrx_rs: std_ulogic := '0';
		signal chry_ce: std_ulogic := '0';
		signal chry_rs: std_ulogic := '0';
		signal scrx_ce: std_ulogic := '0';
		signal scrx_rs: std_ulogic := '0';
		signal scry_ce: std_ulogic := '0';
		signal scry_rs: std_ulogic := '0';

		signal hctr_639: std_ulogic := '0';
		signal vctr_479: std_ulogic := '0';
		signal chrx_007: std_ulogic := '0';
		signal chry_011: std_ulogic := '0';

		-- RAM read, ROM read
		signal ram_tmp: unsigned(11 downto 0) := (others => '0'); -- range 3199 downto 0
		signal mul:     unsigned(15 downto 0) := (others => '0');
		signal rom_tmp: unsigned(11 downto 0) := (others => '0'); -- range 3071 downto 0;

		signal text_d_tmp: std_ulogic_vector(7 downto 0) := (others => '0');

		-- @todo Rename these signals to something more sensible
	begin
		u_hctr: work.vga_pkg.ctrm generic map (g => g, M => 794) 
					port map (rst, clk25MHz, hctr_ce, hctr_rs, hctr);
		u_vctr: work.vga_pkg.ctrm generic map (g => g, M => 525) 
					port map (rst, clk25MHz, vctr_ce, vctr_rs, vctr);

		hctr_ce <= '1';
		hctr_rs <= '1' when hctr = 793 else '0';
		vctr_ce <= '1' when hctr = 663 else '0';
		vctr_rs <= '1' when vctr = 524 else '0';

		u_chrx: work.vga_pkg.ctrm generic map (g => g, M => 8)  
					port map (rst, clk25MHz, chrx_ce, chrx_rs, chrx);
		u_chry: work.vga_pkg.ctrm generic map (g => g, M => 12) 
					port map (rst, clk25MHz, chry_ce, chry_rs, chry);
		u_scrx: work.vga_pkg.ctrm generic map (g => g, M => 80) 
					port map (rst, clk25MHz, scrx_ce, scrx_rs, scrx);
		u_scry: work.vga_pkg.ctrm generic map (g => g, M => 40) 
					port map (rst, clk25MHz, scry_ce, scry_rs, scry);

		hctr_639 <= '1' when hctr = 639 else '0';
		vctr_479 <= '1' when vctr = 479 else '0';
		chrx_007 <= '1' when chrx = 7 else '0';
		chry_011 <= '1' when chry = 11 else '0';

		chrx_rs <= chrx_007 or hctr_639;
		chry_rs <= chry_011 or vctr_479;
		scrx_rs <= hctr_639;
		scry_rs <= vctr_479;

		chrx_ce <= '1' and blank;
		scrx_ce <= chrx_007;
		chry_ce <= hctr_639 and blank;
		scry_ce <= chry_011 and hctr_639;

		ram_tmp <=  (to_unsigned(scry, ram_tmp'length) sll 4) +
				(to_unsigned(scry, ram_tmp'length) sll 6) +
				to_unsigned(scrx, ram_tmp'length);

		text_d_tmp <= text_d(7 downto 0);
		text_a  <= std_ulogic_vector(ram_tmp);
		mul     <= unsigned(text_d_tmp) * 12;
		rom_tmp <= mul(rom_tmp'range) + chry;
		font_a  <= std_ulogic_vector(rom_tmp);
	end block;

	color_block: block
		signal red_on:   std_ulogic := '0';
		signal green_on: std_ulogic := '0';
		signal blue_on:  std_ulogic := '0';
		signal bold:     std_ulogic := '0';
		signal reverse:  std_ulogic := '0';
		signal set:      std_ulogic_vector(2 downto 0) := "110";
		signal final:    std_ulogic_vector(2 downto 0) := "111";
	begin
		bold        <= attr(6);
		reverse     <= attr(7);
		set         <= "100" when bold = '0' else "111";
		final       <= set   when reverse = '0' else not set;

		red_on      <= attr(3) when foreground_draw = '1' else
			       attr(0) when background_draw = '1' else '0';
		green_on    <= attr(4) when foreground_draw = '1' else
			       attr(1) when background_draw = '1' else '0';
		blue_on     <= attr(5) when foreground_draw = '1' else
			       attr(2) when background_draw = '1' else '0';

		o_vga.red   <= final             when red_on    = '1' else "000";
		o_vga.green <= final             when green_on  = '1' else "000";
		o_vga.blue  <= final(2 downto 1) when blue_on   = '1' else "00";
	end block;

	u_reg_delay: work.util.reg generic map (g => g, N => 1)
	port map(clk => clk25MHz, rst => rst, we => '1', di(0) => losr_ld, do(0) => attr_we_delay);

	u_reg_attr: work.util.reg generic map (g => g, N => 8)
	port map(clk => clk25MHz, rst => rst, we => attr_we_delay, di => text_d(15 downto 8), do => attr);

	u_losr: work.vga_pkg.losr generic map (g => g, N => font_d'length)
	port map (rst => rst, clk => clk25MHz, load => losr_ld, ce => losr_ce, do => losr_do, di => font_d);

	losr_ce <= blank;
	losr_ld <= '1' when (chrx = 7) else '0';

	-- video out, vga_en control signal enable/disable vga signal
	foreground_draw_int <=      y  and blank;
	background_draw_int <= (not y) and blank;

	o_vga.hsync <= hsync_int and vga_en;
	o_vga.vsync <= vsync_int and vga_en;

	-- Hardware Cursor
	hw_cursor: block
		signal small:   std_ulogic := '0';
		signal curen2:  std_ulogic := '0';
		signal slowclk: std_ulogic := '0';
		signal curpos:  std_ulogic := '0';
		signal yint:    std_ulogic := '0';
		-- signal crx:     integer range 79 downto 0;
		-- signal cry:     integer range 39 downto 0;
		signal crx:     integer range 127 downto 0;
		signal cry:     integer range 64 downto 0;
		signal counter: unsigned(22 downto 0) := (others => '0');
	begin
		-- slowclk for blink hardware cursor
		counter <= counter + 1 when rising_edge(clk25MHz) else counter;
		slowclk <= counter(22); --2.98Hz

		crx <= to_integer(unsigned(ocrx(6 downto 0)));
		cry <= to_integer(unsigned(ocry(5 downto 0)));

		--
		curpos <= '1' when scry = cry and scrx = crx else '0';
		small  <= '1' when (chry > 8)                else '0';
		curen2 <= (slowclk or (not cur_blink)) and cur_en;
		yint   <= '1' when cur_mode = '0'            else small;
		y      <= (yint and curpos and curen2) xor losr_do;
	end block;
end;
----- VGA Core ----------------------------------------------------------------

-------------------------------------------------------------------------------
--| @file ctrm.vhd
--| @brief Counter, asynchronous *and* synchronous reset, up only.
--|        (ctrm.vhd, original filename)
--| @author         Javier Valcarce García
--| @copyright      Copyright 2007 Javier Valcarce García
--| @license        LGPL version 3
--| @email          javier.valcarce@gmail.com
-------------------------------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity ctrm is
	generic (g: common_generics; M: positive := 8);
	port (
		rst: in  std_ulogic; -- asynchronous rst
		clk: in  std_ulogic;
		ce:  in  std_ulogic; -- enable counting
		rs:  in  std_ulogic; -- synchronous rst
		do:  out integer range (M-1) downto 0 := 0);
end entity;

architecture rtl of ctrm is
	signal c: integer range (M-1) downto 0:= 0;
begin
	do <= c;
	process(rst, clk)
	begin
		if rst = '1' and g.asynchronous_reset then
			c <= 0 after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				c <= 0 after g.delay;
			else
				if ce = '1' then
					if rs = '1' then
						c <= 0 after g.delay;
					else
						c <= c + 1 after g.delay;
					end if;
				end if;
			end if;
		end if;
	end process;
end;

-------------------------------------------------------------------------------
--| @file util.vhd
--| @brief Shift register N-bit, asynchronous reset, synchronous load,
--|        and enable
--| @author         Javier Valcarce García
--| @copyright      Copyright 2007 Javier Valcarce García
--| @license        LGPL version 3
--| @email          javier.valcarce@gmail.com
-------------------------------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity losr is
	generic (g: common_generics; N: positive := 4);
	port
	(
		rst:  in  std_ulogic;
		clk:  in  std_ulogic;
		load: in  std_ulogic;
		ce:   in  std_ulogic;
		do:   out std_ulogic := '0';
		di:   in  std_ulogic_vector(N - 1 downto 0));
end entity;

architecture rtl of losr is
begin
	process(rst, clk)
		variable data: std_ulogic_vector(N - 1 downto 0) := (others => '0');
	begin
		if rst = '1' and g.asynchronous_reset then
			data := (others => '0');
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				data := (others => '0');
			else
				if load = '1' then
					data := di;
				elsif ce = '1' then
					data := data(N-2 downto 0) & "0";
				end if;
			end if;
		end if;
		do <= data(N-1) after g.delay;
	end process;
end;

