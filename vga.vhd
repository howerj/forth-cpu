-------------------------------------------------------------------------------
--| @file vga.vhd
--| @brief      Monochrome Text Mode Video Controller VHDL Module and VT100
--|             Terminal Emulator
--| @author     Javier Valcarce Garc�a
--| @author     Richard James Howe
--| @copyright  Copyright 2007 Javier Valcarce Garc�a, 2017 Richard James Howe
--| @license    LGPL version 3
--| @email      javier.valcarce@gmail.com
--| @note       (Modifications and repackaging by Richard James Howe)
--|
--| @todo Add scrolling by changing the base address "text_a", adding
--| a line
-------------------------------------------------------------------------------

----- VGA Package -------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package vga_pkg is
	type vga_physical_interface is record
		red:   std_logic_vector(2 downto 0);
		green: std_logic_vector(2 downto 0);
		blue:  std_logic_vector(1 downto 0);
		hsync: std_logic;
		vsync: std_logic;
	end record;

	type vga_control_registers_we_interface is record
		crx: std_logic; -- Write enable for cursor X position register
		cry: std_logic; -- Write enable for VGA control register
		ctl: std_logic; -- Write enable for cursor Y position register
	end record;

	type vga_control_registers_interface is record
		crx:    std_logic_vector(6 downto 0); -- Cursor position X
		cry:    std_logic_vector(5 downto 0); -- Cursor position Y
		ctl:    std_logic_vector(7 downto 0); -- Control register
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
	port(
		clk:         in  std_logic;
		clk25MHz:    in  std_logic;
		rst:         in  std_logic;

		-- VGA Text buffer interface
		vga_we_ram:  in  std_logic; -- Write enable RAM
		vga_din:     in  std_logic_vector(15 downto 0);
		vga_addr:    in  std_logic_vector(12 downto 0);
		vga_dout:    out std_logic_vector(15 downto 0):= (others => '0');

		-- VGA control registers
		i_vga_control_we: in vga_control_registers_we_interface;
		i_vga_control:    in vga_control_registers_interface;

		o_vga:    out vga_physical_interface);
	end component;

	component vga_core is
	port (
		rst:      in  std_logic;
		clk25MHz: in  std_logic;
		text_a:   out std_logic_vector(11 downto 0); -- text buffer
		text_d:   in  std_logic_vector( 7 downto 0);
		font_a:   out std_logic_vector(11 downto 0); -- font buffer
		font_d:   in  std_logic_vector( 7 downto 0);
		 --
		ocrx:     in  std_logic_vector(6 downto 0);
		ocry:     in  std_logic_vector(5 downto 0);
		octl:     in  std_logic_vector(6 downto 0);
		--
		R:        out std_logic;
		G:        out std_logic;
		B:        out std_logic;
		hsync:    out std_logic;
		vsync:    out std_logic);
	end component;

	component losr is
	generic (N : integer := 4);
	port
	(
		rst:  in  std_logic;
		clk:  in  std_logic;
		load: in  std_logic;
		ce:   in  std_logic;
		do:   out std_logic := '0';
		di:   in  std_logic_vector(N - 1 downto 0));
	end component;

	component ctrm is
		generic (M : integer := 8);
		port (
			rst: in  std_logic; -- asynchronous rst
			clk: in  std_logic;
			ce:  in  std_logic; -- enable counting
			rs:  in  std_logic; -- synchronous rst
			do:  out integer range (M-1) downto 0 := 0);
	end component;

	component vt100 is
	port(
		clk:        in  std_logic;
		clk25MHz:   in  std_logic;
		rst:        in  std_logic;
		we:         in  std_logic;
		char:       in  std_logic_vector(7 downto 0);

		busy:       out std_logic;
		o_vga:      out vga_physical_interface);
	end component;

	component accumulator is
		generic(
			N:      positive := 16);
		port(
			clk:    in  std_logic;
			rst:    in  std_logic;
			we:     in  std_logic;
			init:   in  std_logic;
			char:   in  std_logic_vector(7 downto 0);
			char_o: out std_logic_vector(7 downto 0);
			done_o: out std_logic;
			ready:  out std_logic;
			n_o:    out unsigned(N - 1 downto 0));
	end component;

end package;

----- VGA Package -------------------------------------------------------------

----- Accumulator -------------------------------------------------------------
-- The purpose of this module is to read in numeric characters ('0' through 
-- '9') and convert the string into a binary number ('n_o'). On the first non-
-- numeric character the module stops and outputs that non-numeric character
-- as well as the converted number string. The module waits in the FINISHED
-- state until the module is reset by an external signal ('init').
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vga_pkg.all;

entity accumulator is
	generic(
		N:      positive := 16);
	port(
		clk:    in  std_logic;
		rst:    in  std_logic;
		we:     in  std_logic;
		init:   in  std_logic;
		char:   in  std_logic_vector(7 downto 0);
		char_o: out std_logic_vector(7 downto 0);
		done_o: out std_logic;
		ready:  out std_logic;
		n_o:    out unsigned(N - 1 downto 0));
end entity;

architecture rlt of accumulator is
	type state_type is (RESET, WAITING, COMMAND, ACCUMULATE, WRITE, FINISHED);
	signal state_c, state_n: state_type   := RESET;
	signal c_c, c_n: unsigned(char'range) := (others => '0');
	signal n_c, n_n: unsigned(n_o'range)  := (others => '0');
	signal f_c, f_n: boolean              := true;
begin
	char_o <= std_logic_vector(c_c);
	n_o    <= n_c;
	ready  <= '1' when state_c = WAITING else '0';

	process(clk, rst)
		variable akk: unsigned((2 * N) - 1 downto 0) := (others => '0');
	begin
		if rst = '1' then
			state_n   <= RESET;
		elsif rising_edge(clk) then
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
	end process;

end architecture;
----- Accumulator -------------------------------------------------------------

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

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vga_pkg.all;

entity vt100 is
	port(
		clk:        in  std_logic;
		clk25MHz:   in  std_logic;
		rst:        in  std_logic;
		we:         in  std_logic;
		char:       in  std_logic_vector(7 downto 0);

		busy:       out std_logic;
		o_vga:      out vga_physical_interface);
end entity;

architecture rtl of vt100 is
	constant width:  positive := 80;
	constant height: positive := 40;
	constant number: positive := 8;

	type state_type is (RESET, ACCEPT, NORMAL, WRAP, LIMIT, CSI, COMMAND, NUMBER1, NUMBER2, COMMAND1, COMMAND2, WRITE, ERASING);
	signal state_c, state_n: state_type := RESET;

	constant esc:       unsigned(char'range) := x"1b";
	constant lsqb:      unsigned(char'range) := x"5b"; -- '['
	constant tab:       unsigned(char'range) := x"09";
	constant cr:        unsigned(char'range) := x"0d";
	constant lf:        unsigned(char'range) := x"0a";
	constant backspace: unsigned(char'range) := x"08";
	constant blank:     unsigned(char'range) := x"20";
	constant ascii_c:   unsigned(char'range) := x"63"; -- c

	signal addr:       std_logic_vector(12 downto 0) := (others => '0');
	signal attr:       std_logic_vector(7 downto 0)  := (others => '0');
	signal data_we:    std_logic                     := '0';
	signal x:          std_logic_vector(6 downto 0)  := (others => '0');
	signal y:          std_logic_vector(5 downto 0)  := (others => '0');
	signal cursor_we:  std_logic                     := '0';

	signal x_n, x_c:     unsigned(x'range)    := (others => '0');
	signal y_n, y_c:     unsigned(y'range)    := (others => '0');
	signal c_n, c_c:     unsigned(char'range) := (others => '0');
	--signal ctl_c, ctl_n: unsigned(6 downto 0) := (others => '0');

	signal n1_n, n1_c: unsigned(y'range) := (others => '0');
	signal n2_n, n2_c: unsigned(x'range) := (others => '0');

	signal x_minus_one:         unsigned(x'range) := (others => '0');
	signal x_minus_one_limited: unsigned(x'range) := (others => '0');
	signal x_underflow:         boolean           := false;
	signal x_plus_one:          unsigned(x'range) := (others => '0');
	signal x_overflow:          boolean           := false;

	signal y_plus_one:          unsigned(y'range) := (others => '0');
	signal y_overflow:          boolean           := false;

	signal count_c, count_n:    unsigned(addr'range) := (others => '0');
	signal limit_c, limit_n:    unsigned(addr'range) := (others => '0');

	signal akk_done_o:  std_logic := '0';
	signal akk_ready_o: std_logic := '0';
	signal akk_init:    std_logic := '0';
	signal n_o:         unsigned(number - 1 downto 0) := (others => '0');
	signal akk_char_o:  std_logic_vector(char'range)  := (others => '0');
begin
	accumulator_0: work.vga_pkg.accumulator
		generic map(N => number)
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
		signal addr_int: integer range 8191 downto 0 := 0; --12 bits
		signal x_int:    integer range  127 downto 0 := 0; -- 7 bits
	begin
		x_int    <=  to_integer(x_n);
		addr_int <= (to_integer(y_n) * width) + x_int;
		addr <= std_logic_vector(to_unsigned(addr_int, 13)) when state_c /= ERASING else std_logic_vector(count_c);
	end block;

	x_minus_one         <= x_c - 1;
	x_plus_one          <= x_c + 1;
	x_underflow         <= true when x_minus_one > width  - 1 else false;
	x_overflow          <= true when x_c         > width  - 1 else false;
	x_minus_one_limited <= (others => '0') when x_underflow else x_minus_one;
	y_plus_one          <= y_c + 1;
	y_overflow          <= true when y_c         > height - 1 else false;

	busy                <= '0' when state_c = ACCEPT 
				       or state_c = CSI
				       or state_c = NUMBER1
				       or state_c = NUMBER2 else '1';

	vga_blk: block
		signal vga_din:    std_logic_vector(15 downto 0)      := (others => '0');
		signal vga_ctr_we: vga_control_registers_we_interface := vga_control_registers_we_initialize;
		signal vga_ctr:    vga_control_registers_interface    := vga_control_registers_initialize;
	begin
		vga_din        <= attr & std_logic_vector(c_c);
		vga_ctr.crx    <= std_logic_vector(x_plus_one);
		vga_ctr.cry    <= std_logic_vector(y_c);
		vga_ctr.ctl    <= x"7F"; 
		vga_ctr_we.crx <= cursor_we;
		vga_ctr_we.cry <= cursor_we;
		vga_ctr_we.ctl <= cursor_we;

		vga_0: work.vga_pkg.vga_top
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

	-- @todo A small subset of the ANSI escape codes should be implement
	--
	-- Subset to implement:
	-- ED  - Erase Display, CSI n 'J'  
	-- SGR - Select Graphic Rendition - for colors, CSI n 'm'
	-- HVP - Horizontal and Vertical Position - CSI n ; m 'f'
	--
	-- It would also be interesting to implement an ASCII bell character
	-- with some audio output (or perhaps just inverting the colors on
	-- the screen for a second).
	--
	-- @todo Fix the bugs and test this module! 
	fsm: process(clk, rst)
	begin
		if rst = '1' then
			state_c <= RESET;
		elsif rising_edge(clk) then
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

			if state_c = RESET then
				n1_n      <= (others => '0');
				n2_n      <= (others => '0');
				x_n       <= (others => '0');
				y_n       <= (others => '0');
				c_n       <= (others => '0');
				count_n   <= (others => '0');
				limit_n   <= (others => '0');
				state_n   <= ACCEPT;
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
					limit_n <= unsigned(addr and "1111111111000") + 8;
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
					x_n     <= x_plus_one;
					data_we <= '1';
					state_n <= WRAP;
				end case;
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
				case akk_char_o is
				when x"4a" => -- ESC CSI n 'J'
					x_n       <= (others => '0');
					y_n       <= (others => '0');
					cursor_we <= '1';
					state_n   <= ERASING;
					c_n       <= blank;
					count_n   <= (others => '0');
					limit_n   <= "1000000000000";
				when x"3b" =>
					state_n <= NUMBER2;
					akk_init <= '1';
				when others => -- Error
					state_n <= ACCEPT;
				end case;
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
					limit_n <= "1000000000000";
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
				if count_c /= limit_c then
					count_n <= count_c + 1;
					data_we <= '1';
				else
					state_n <= WRAP;
				end if;
			else
				state_n <= RESET;
				report "Invalid State" severity failure;
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

entity vga_top is
	port(
		clk:              in  std_logic;
		clk25MHz:         in  std_logic;
		rst:              in  std_logic;

		-- VGA Text buffer interface
		vga_we_ram:       in  std_logic; -- Write enable RAM
		vga_din:          in  std_logic_vector(15 downto 0);
		vga_addr:         in  std_logic_vector(12 downto 0);
		vga_dout:         out std_logic_vector(15 downto 0) := (others => '0');

		-- VGA control registers
		i_vga_control_we: in vga_control_registers_we_interface;
		i_vga_control:    in vga_control_registers_interface;

		o_vga:            out vga_physical_interface);
end;

architecture behav of vga_top is

	-- Setup for text buffer memory
	constant text_addr_length: positive := 13;
	constant text_data_length: positive := 16;
	constant text_file_name:   string   := "text.hex";
	constant text_file_type:   string   := "hex";

	-- Setup for font buffer memory
	constant font_addr_length: positive := 12;
	constant font_data_length: positive := 8;
	constant font_file_name:   string   := "font.bin";
	constant font_file_type:   string   := "bin";

	-- Internal signals for mapping output <--> VGA module
	signal  R_internal:      std_logic := '0';
	signal  G_internal:      std_logic := '0';
	signal  B_internal:      std_logic := '0';

	-- Text RAM signals, RAM <--> VGA module
	signal  text_dout:       std_logic_vector(15 downto 0) := (others => '0');
	signal  text_din:        std_logic_vector(15 downto 0) := (others => '0');
	signal  text_addr:       std_logic_vector(11 downto 0) := (others => '0');
	signal  text_addr_full:  std_logic_vector(12 downto 0) := (others => '0');

	-- Font ROM signals, ROM<-->VGA module
	signal  font_addr:       std_logic_vector(11 downto 0) := (others => '0');
	signal  font_dout:       std_logic_vector( 7 downto 0) := (others => '0');

	signal  control_c, control_n: vga_control_registers_interface := vga_control_registers_initialize;

begin
	-- Output assignments, syncs elsewhere
	-- o_vga.red   <= (others => R_internal);
	-- o_vga.green <= (others => G_internal);
	-- o_vga.blue  <= (others => B_internal);
	--
	-- @note This is an experimental interface, that allows coloring of individual
	-- characters, it is not usable at the moment
	--
	o_vga.red   <= text_dout(10 downto 8)  when R_internal = '1' and text_dout(10 downto  8) /= "000" else (others => R_internal);
	o_vga.green <= text_dout(13 downto 11) when G_internal = '1' and text_dout(13 downto 11) /= "000" else (others => G_internal);
	o_vga.blue  <= text_dout(15 downto 14) when B_internal = '1' and text_dout(15 downto 14) /=  "00" else (others => B_internal);

	-- Internal control registers
	-- Next state on clk edge rising
	vga_ns: process(clk, rst)
	begin
		if rst = '1' then
			control_c   <= vga_control_registers_initialize;
		elsif rising_edge(clk) then
			control_c   <= control_n;
		end if;
	end process;

	-- Internal control register
	-- We write into the registers here.
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

	-- The actual VGA module
	u_vga: work.vga_pkg.vga_core port map (
		rst       => rst,
		clk25MHz  => clk25MHz,

		text_a    => text_addr,
		text_d    => text_dout(7 downto 0),

		font_a    => font_addr,
		font_d    => font_dout,

		ocrx      => control_c.crx,
		ocry      => control_c.cry,
		octl      => control_c.ctl(6 downto 0),

		R         => R_internal,
		G         => G_internal,
		B         => B_internal,
		hsync     => o_vga.hsync,
		vsync     => o_vga.vsync);

	text_addr_full <= control_c.ctl(7) & text_addr;

	--| @brief This RAM module holds the text we want to display on to the
	--| monitor. The text buffer holds at least 80*40 characters.
	u_text: entity work.dual_port_block_ram
	generic map(
	    addr_length   => text_addr_length,
	    data_length   => text_data_length,
	    file_name     => text_file_name,
	    file_type     => text_file_type)
	port map (
		a_clk  => clk,
		-- External interface
		a_dwe  => vga_we_ram,
		a_dre  => '1',
		a_addr => vga_addr,
		a_din  => vga_din,
		a_dout => vga_dout,
		-- Internal interface
		b_clk  => clk25MHz,
		b_dwe  => '0',
		b_dre  => '1',
		b_addr => text_addr_full,
		b_din  => (others => '0'),
		b_dout => text_dout);

	--| VGA Font memory
	u_font: entity work.single_port_block_ram
	generic map(
		addr_length   => font_addr_length,
		data_length   => font_data_length,
		file_name     => font_file_name,
		file_type     => font_file_type)
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
use work.vga_pkg.ctrm;
use work.vga_pkg.losr;


entity vga_core is
	port (
		rst:      in  std_logic;
		clk25MHz: in  std_logic;
		text_a:   out std_logic_vector(11 downto 0); -- text buffer
		text_d:   in  std_logic_vector( 7 downto 0);
		font_a:   out std_logic_vector(11 downto 0); -- font buffer
		font_d:   in  std_logic_vector( 7 downto 0);
		 --
		ocrx:     in  std_logic_vector(6 downto 0);
		ocry:     in  std_logic_vector(5 downto 0);
		octl:     in  std_logic_vector(6 downto 0);
		--
		R:        out std_logic;
		G:        out std_logic;
		B:        out std_logic;
		hsync:    out std_logic;
		vsync:    out std_logic);
end entity;

architecture rtl of vga_core is

	signal R_int:     std_logic := '0';
	signal G_int:     std_logic := '0';
	signal B_int:     std_logic := '0';
	signal hsync_int: std_logic := '1';
	signal vsync_int: std_logic := '1';

	signal blank: std_logic := '0';
	signal hctr:  integer range 793 downto 0 := 0;
	signal vctr:  integer range 524 downto 0 := 0;

	-- character/pixel position on the screen
	signal scry:  integer range 39 downto 0 := 0;  -- chr row   < 40 (6 bits)
	signal scrx:  integer range 79 downto 0 := 0;  -- chr col   < 80 (7 bits)
	signal chry:  integer range 11 downto 0 := 0;  -- chr high  < 12 (4 bits)
	signal chrx:  integer range 7  downto 0 := 0;  -- chr width < 08 (3 bits)

	signal losr_ce: std_logic := '0';
	signal losr_ld: std_logic := '0';
	signal losr_do: std_logic := '0';
	signal y:       std_logic := '0';  -- character luminance pixel value (0 or 1)

	-- control io register
	signal ctl:       std_logic_vector(7 downto 0):= (others =>'0');
	signal vga_en:    std_logic := '0';
	signal cur_en:    std_logic := '0';
	signal cur_mode:  std_logic := '0';
	signal cur_blink: std_logic := '0';
	signal ctl_r:     std_logic := '0';
	signal ctl_g:     std_logic := '0';
	signal ctl_b:     std_logic := '0';

begin

	-- hsync generator, initialized with '1'
	process (rst, clk25MHz)
	begin
		if rst = '1' then
			hsync_int <= '1';
		elsif rising_edge(clk25MHz) then
			if (hctr > 663) and (hctr < 757) then
				hsync_int <= '0';
			else
				hsync_int <= '1';
			end if;
		end if;
	end process;


	-- vsync generator, initialized with '1'
	process (rst, clk25MHz)
	begin
		if rst = '1' then
			vsync_int <= '1';
		elsif rising_edge(clk25MHz) then
			if (vctr > 499) and (vctr < 502) then
				vsync_int <= '0';
			else
				vsync_int <= '1';
			end if;
		end if;
	end process;

	-- Blank signal, 0 = no draw, 1 = visible/draw zone

	-- Proboscide99 31/08/08
	blank <= '0' when (hctr < 8) or (hctr > 647) or (vctr > 479) else '1';

	-- flip-flips for sync of R, G y B signal, initialized with '0'
	process (rst, clk25MHz)
	begin
		if rst = '1' then
			R <= '0';
			G <= '0';
			B <= '0';
		elsif rising_edge(clk25MHz) then
			R <= R_int;
			G <= G_int;
			B <= B_int;
		end if;
	end process;

	-- Control register. Individual control signal

	vga_en    <= octl(6);
	cur_en    <= octl(5);
	cur_blink <= octl(4);
	cur_mode  <= octl(3);
	ctl_b     <= octl(2);
	ctl_g     <= octl(1);
	ctl_r     <= octl(0);

	-- counters, hctr, vctr, srcx, srcy, chrx, chry
	counters: block

		signal hctr_ce: std_logic;
		signal hctr_rs: std_logic;
		signal vctr_ce: std_logic;
		signal vctr_rs: std_logic;

		signal chrx_ce: std_logic;
		signal chrx_rs: std_logic;
		signal chry_ce: std_logic;
		signal chry_rs: std_logic;
		signal scrx_ce: std_logic;
		signal scrx_rs: std_logic;
		signal scry_ce: std_logic;
		signal scry_rs: std_logic;

		signal hctr_639: std_logic;
		signal vctr_479: std_logic;
		signal chrx_007: std_logic;
		signal chry_011: std_logic;

		-- RAM read, ROM read
		signal ram_tmp: integer range 3200 downto 0;  --12 bits
		signal rom_tmp: integer range 3071 downto 0;

	begin
		u_hctr: work.vga_pkg.ctrm generic map (M => 794) port map (rst, clk25MHz, hctr_ce, hctr_rs, hctr);
		u_vctr: work.vga_pkg.ctrm generic map (M => 525) port map (rst, clk25MHz, vctr_ce, vctr_rs, vctr);

		hctr_ce <= '1';
		hctr_rs <= '1' when hctr = 793 else '0';
		vctr_ce <= '1' when hctr = 663 else '0';
		vctr_rs <= '1' when vctr = 524 else '0';

		u_chrx: work.vga_pkg.ctrm generic map (M => 8)  port map (rst, clk25MHz, chrx_ce, chrx_rs, chrx);
		u_chry: work.vga_pkg.ctrm generic map (M => 12) port map (rst, clk25MHz, chry_ce, chry_rs, chry);
		u_scrx: work.vga_pkg.ctrm generic map (M => 80) port map (rst, clk25MHz, scrx_ce, scrx_rs, scrx);
		u_scry: work.vga_pkg.ctrm generic map (M => 40) port map (rst, clk25MHz, scry_ce, scry_rs, scry);

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

		ram_tmp <=  to_integer(to_unsigned(scry,12) sll 4) +
				to_integer(to_unsigned(scry,12) sll 6) +
				scrx;

		text_a  <= std_logic_vector(to_unsigned(ram_tmp, 12));
		rom_tmp <= to_integer(unsigned(text_d)) * 12 + chry;
		font_a  <= std_logic_vector(to_unsigned(rom_tmp, 12));
	end block;

	u_losr: work.vga_pkg.losr generic map (N => 8)
	port map (rst, clk25MHz, losr_ld, losr_ce, losr_do, FONT_D);

	losr_ce <= blank;
	losr_ld <= '1' when (chrx = 7) else '0';

	-- video out, vga_en control signal enable/disable vga signal
	R_int <= ctl_r and y and blank;
	G_int <= ctl_g and y and blank;
	B_int <= ctl_b and y and blank;

	hsync <= hsync_int and vga_en;
	vsync <= vsync_int and vga_en;

	-- Hardware Cursor
	hw_cursor: block
		signal small:   std_logic;
		signal curen2:  std_logic;
		signal slowclk: std_logic;
		signal curpos:  std_logic;
		signal yint:    std_logic;
		signal crx:     integer range 79 downto 0;
		signal cry:     integer range 39 downto 0;
		signal counter: unsigned(22 downto 0);
	begin

		-- slowclk for blink hardware cursor
		counter <= counter + 1 when rising_edge(clk25MHz);
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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ctrm is
	generic (M : integer := 8);
	port (
		rst: in  std_logic; -- asynchronous rst
		clk: in  std_logic;
		ce:  in  std_logic; -- enable counting
		rs:  in  std_logic; -- synchronous rst
		do:  out integer range (M-1) downto 0 := 0
	);
end ctrm;

architecture rtl of ctrm is
	signal c : integer range (M-1) downto 0:= 0;
begin
	do <= c;
	process(rst, clk)
	begin
		if rst = '1' then
			c <= 0;
		elsif rising_edge(clk) then
			if ce = '1' then
				if rs = '1' then
					c <= 0;
				else
					c <= c + 1;
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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity losr is
	generic (N : integer := 4);
	port
	(
		rst:  in  std_logic;
		clk:  in  std_logic;
		load: in  std_logic;
		ce:   in  std_logic;
		do:   out std_logic := '0';
		di:   in  std_logic_vector(N - 1 downto 0));
end losr;

architecture rtl of losr is
begin

	process(rst, clk)
		variable data : std_logic_vector(N - 1 downto 0) := (others => '0');
	begin
		if rst = '1' then
			data := (others => '0');
		elsif rising_edge(clk) then
			if load = '1' then
				data := di;
			elsif ce = '1' then
				data := data(N-2 downto 0) & "0";
			end if;
		end if;

		do <= data(N-1);
	end process;
end;

