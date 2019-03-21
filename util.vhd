-------------------------------------------------------------------------------
--| @file util.vhd
--| @brief A collection of utilities and simple components. The components
--| should be synthesizable, and the functions can be used within synthesizable
--| components, unless marked with a "_tb" suffix (or if the function n_bits).
--|
--| @todo Add communication modules for SPI and UART, also a VGA controller,
--| CORDIC, and if this were to be its own project - a H2 Core with an 
--| eForth image ready to go. 
--|
--| Other modules to implement; CRC core (reuse LFSR), Count
--| Leading Zeros, Count Trailing Zeros, Manchester CODEC, Wishbone interface
--| types and Wishbone Bus Arbitrator.
--|
--| More exotic modules would include; encryption, compression, sorting networks,
--| switching networks, Reed-Solomon CODEC, Discrete Fourier Transform/Discrete 
--| Cosine Transform, Pulse Width/Code/Position Modulation modules, so long as
--| they are fairly generic and synthesizable.
--|
--| Potential improvements to the library:
--| - Optional registers on either input or output, selectable by a generic
--| - Better timing models
--| - More assertions
--| - See 'A Structured VHDL design' by Jiri Gaisler, 
--|   <http://gaisler.com/doc/vhdl2proc.pdf> and apply methodology.
--|
--| @author         Richard James Howe
--| @copyright      Copyright 2017, 2019 Richard James Howe
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package util is
	-- Not all modules will need every generic specified here, even so it
	-- is easier to group the common generics in one structure.
	type common_generics is record
		clock_frequency:    positive; -- clock frequency of module clock
		delay:              time;     -- gate delay for simulation purposes
		asynchronous_reset: boolean;  -- use asynchronous reset if true
	end record;

	constant default_settings: common_generics := (
		clock_frequency    => 100_000_000,
		delay              => 10 ns,
		asynchronous_reset => true
	);

	component util_tb is
		generic (g: common_generics);
	end component;

	component clock_source_tb is
		generic (g: common_generics; hold_rst: positive := 1);
		port (
			stop:            in     std_ulogic := '0';
			clk:             out    std_ulogic;
			clk_with_jitter: out    std_ulogic := '0';
			rst:             out    std_ulogic := '0');
	end component;

	component reg
		generic (g: common_generics; N: positive);
		port (
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			we:  in  std_ulogic;
			di:  in  std_ulogic_vector(N - 1 downto 0);
			do:  out std_ulogic_vector(N - 1 downto 0));
	end component;

	component shift_register
		generic (g: common_generics; N: positive);
		port (
			clk:     in  std_ulogic;
			rst:     in  std_ulogic;
			we:      in  std_ulogic;
			di:      in  std_ulogic;
			do:      out std_ulogic;

			-- optional
			load_we: in  std_ulogic := '0';
			load_i:  in  std_ulogic_vector(N - 1 downto 0) := (others => '0');
			load_o:  out std_ulogic_vector(N - 1 downto 0));
	end component;

	component shift_register_tb
		generic (g: common_generics);
	end component;

	component timer_us
		generic (g: common_generics; timer_period_us: natural);
		port (
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			co:  out std_ulogic);
	end component;

	component timer_us_tb
		generic (g: common_generics);
	end component;

	component rising_edge_detector is
		generic (g: common_generics);
		port (
			clk:    in  std_ulogic;
			rst:    in  std_ulogic;
			di:     in  std_ulogic;
			do:     out std_ulogic);
	end component;

	component rising_edge_detector_tb is
		generic (g: common_generics);
	end component;

	component rising_edge_detectors is
		generic (g: common_generics; N: positive);
		port (
			clk:    in  std_ulogic;
			rst:    in  std_ulogic;
			di:     in  std_ulogic_vector(N - 1 downto 0);
			do:     out std_ulogic_vector(N - 1 downto 0));
	end component;

	-- NB. 'half_adder' test bench is folded in to 'full_adder_tb'
	component half_adder is
		generic (g: common_generics); -- simulation only
		port (
			a:     in  std_ulogic;
			b:     in  std_ulogic;
			sum:   out std_ulogic;
			carry: out std_ulogic);
	end component;

	component full_adder is
		generic (g: common_generics); -- simulation only
		port (
			x:     in    std_ulogic;
			y:     in    std_ulogic;
			z:     in    std_ulogic;
			sum:   out   std_ulogic;
			carry: out   std_ulogic);
	end component;

	component full_adder_tb is
		generic (g: common_generics);
	end component;

	component fifo is
		generic (g: common_generics;
			data_width:  positive;
			fifo_depth:  positive;
			read_first:  boolean := true);
		port (
			clk:   in  std_ulogic;
			rst:   in  std_ulogic;
			di:    in  std_ulogic_vector(data_width - 1 downto 0);
			we:    in  std_ulogic;
			re:    in  std_ulogic;
			do:    out std_ulogic_vector(data_width - 1 downto 0);

			-- optional
			full:  out std_ulogic := '0';
			empty: out std_ulogic := '1');
	end component;

	component fifo_tb is
		generic (g: common_generics);
	end component;

	component counter is
		generic (g: common_generics; N: positive);
		port (
			clk:     in  std_ulogic;
			rst:     in  std_ulogic;
			ce:      in  std_ulogic;
			cr:      in  std_ulogic;
			dout:    out std_ulogic_vector(N - 1 downto 0);

			-- optional
			load_we: in  std_ulogic := '0';
			load_i:  in  std_ulogic_vector(N - 1 downto 0) := (others => '0'));
	end component;

	component counter_tb is
		generic (g: common_generics);
	end component;

	component lfsr is
		generic (g: common_generics; constant tap: std_ulogic_vector);
		port (
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			ce:  in  std_ulogic := '1';
			we:  in  std_ulogic;
			di:  in  std_ulogic_vector(tap'high + 1 downto tap'low);
			do:  out std_ulogic_vector(tap'high + 1 downto tap'low));
	end component;

	component lfsr_tb is
		generic (g: common_generics);
	end component;

	component io_pins is
		generic (g: common_generics; N: positive);
		port (
			clk:         in    std_ulogic;
			rst:         in    std_ulogic;
			control:     in    std_ulogic_vector(N - 1 downto 0);
			control_we:  in    std_ulogic;
			din:         in    std_ulogic_vector(N - 1 downto 0);
			din_we:      in    std_ulogic;
			dout:        out   std_ulogic_vector(N - 1 downto 0);
			pins:        inout std_logic_vector(N - 1 downto 0)); -- NB!
	end component;

	component io_pins_tb is
		generic (g: common_generics);
	end component;

	type file_format is (FILE_HEX, FILE_BINARY, FILE_NONE);

	component dual_port_block_ram is
	generic (g: common_generics;
		addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port (
		-- port A of dual port RAM
		a_clk:  in  std_ulogic;
		a_dwe:  in  std_ulogic;
		a_dre:  in  std_ulogic;
		a_addr: in  std_ulogic_vector(addr_length - 1 downto 0);
		a_din:  in  std_ulogic_vector(data_length - 1 downto 0);
		a_dout: out std_ulogic_vector(data_length - 1 downto 0) := (others => '0');
		-- port B of dual port RAM
		b_clk:  in  std_ulogic;
		b_dwe:  in  std_ulogic;
		b_dre:  in  std_ulogic;
		b_addr: in  std_ulogic_vector(addr_length - 1 downto 0);
		b_din:  in  std_ulogic_vector(data_length - 1 downto 0);
		b_dout: out std_ulogic_vector(data_length - 1 downto 0) := (others => '0'));
	end component;

	component single_port_block_ram is
	generic (g: common_generics;
		addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port (
		clk:  in  std_ulogic;
		dwe:  in  std_ulogic;
		dre:  in  std_ulogic;
		addr: in  std_ulogic_vector(addr_length - 1 downto 0);
		din:  in  std_ulogic_vector(data_length - 1 downto 0);
		dout: out std_ulogic_vector(data_length - 1 downto 0) := (others => '0'));
	end component;

	component data_source is
		generic (g: common_generics;
			addr_length: positive    := 12;
			data_length: positive    := 16;
			file_name:   string      := "memory.bin";
			file_type:   file_format := FILE_BINARY);
		port (
			clk:     in  std_ulogic;
			rst:     in  std_ulogic;

			ce:      in  std_ulogic := '1';
			cr:      in  std_ulogic;

			load:    in  std_ulogic_vector(addr_length - 1 downto 0) := (others => '0');
			load_we: in  std_ulogic := '0';

			dout:    out std_ulogic_vector(data_length - 1 downto 0));
	end component;

	component ucpu is
		generic (
			asynchronous_reset:  boolean := true; -- use asynchronous reset if true, synchronous if false
			delay:               time    := 0 ns; -- simulation only

			width:               positive range 8 to 32 := 8);
		port (
			clk, rst: in  std_ulogic;

			pc:       out std_ulogic_vector(width - 3 downto 0);
			op:       in  std_ulogic_vector(width - 1 downto 0);

			adr:      out std_ulogic_vector(width - 3 downto 0);
			di:       in  std_ulogic_vector(width - 1 downto 0);
			re, we:   out std_ulogic;
			do:       out std_ulogic_vector(width - 1 downto 0));
	end component;

	component ucpu_tb is
		generic (g: common_generics; file_name: string := "ucpu.bin");
	end component;

	component restoring_divider is
		generic (g: common_generics; N: positive);
		port (
			clk:   in  std_ulogic;
			rst:   in  std_ulogic := '0';

			a:     in  std_ulogic_vector(N - 1 downto 0);
			b:     in  std_ulogic_vector(N - 1 downto 0);
			start: in  std_ulogic;
			done:  out std_ulogic;
			c:     out std_ulogic_vector(N - 1 downto 0));
	end component;

	component restoring_divider_tb is
		generic (g: common_generics);
	end component;

	component debounce_us is
		generic (g: common_generics; timer_period_us: natural);
		port (
			clk:   in  std_ulogic;
			di:    in  std_ulogic;
			do:    out std_ulogic);
	end component;

	component debounce_block_us is
		generic (g: common_generics; N: positive; timer_period_us: natural);
		port (
			clk:   in  std_ulogic;
			di:    in  std_ulogic_vector(N - 1 downto 0);
			do:    out std_ulogic_vector(N - 1 downto 0));
	end component;

	component debounce_us_tb is
		generic (g: common_generics);
	end component;

	component state_changed is
		generic (g: common_generics);
		port (
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			di:  in  std_ulogic;
			do:  out std_ulogic);
	end component;

	component state_block_changed is
		generic (g: common_generics; N: positive);
		port (
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			di:  in  std_ulogic_vector(N - 1 downto 0);
			do:  out std_ulogic_vector(N - 1 downto 0));
	end component;

	component reset_generator is
		generic (g: common_generics; reset_period_us: natural := 1);
		port (
			clk: in  std_logic := 'X';
			rst: out std_logic := '0'); -- reset out!
	end component;

	component reset_generator_tb is
		generic (g: common_generics);
	end component;

	function n_bits(x: natural) return natural;           -- Not synthesizable
	function n_bits(x: std_ulogic_vector) return natural; -- Not synthesizable

	component bit_count is
		generic (g: common_generics; N: positive);
		port (
			bits:   in std_ulogic_vector(N - 1 downto 0);
			count: out std_ulogic_vector(n_bits(N) downto 0));
	end component;

	component bit_count_tb is
		generic (g: common_generics);
	end component;

	component majority is
		generic (g: common_generics; N: positive; even_wins: boolean := false);
		port (
			bits: in std_ulogic_vector(N - 1 downto 0);
			vote: out std_ulogic;
			tie: out std_ulogic);
	end component;

	component majority_tb is
		generic (g: common_generics);
	end component;

	component delay_line is
		generic (g: common_generics; width: positive; depth: natural);
		port (
			clk: in std_ulogic;
			rst: in std_ulogic;
			ce:  in std_ulogic := '1';
			di:  in std_ulogic_vector(width - 1 downto 0);
			do: out std_ulogic_vector(width - 1 downto 0));
	end component;

	component delay_line_tb is
		generic (g: common_generics);
	end component;

	component gray_encoder is
		generic (g: common_generics; N: positive);
		port (di: in std_ulogic_vector(N - 1 downto 0);
		     do: out std_ulogic_vector(N - 1 downto 0));
	end component;

	component gray_decoder is
		generic (g: common_generics; N: positive);
		port (di: in std_ulogic_vector(N - 1 downto 0);
		     do: out std_ulogic_vector(N - 1 downto 0));
	end component;

	component gray_tb is
		generic (g: common_generics);
	end component;

	component parity_module is
		generic (g: common_generics; N: positive; even: boolean);
		port (di: in std_ulogic_vector(N - 1 downto 0);
			do: out std_ulogic);
	end component;

	component hamming_7_4_encoder is
		generic (g: common_generics);
		port (
			di:      in std_ulogic_vector(3 downto 0);
			do:     out std_ulogic_vector(6 downto 0);
			parity: out std_ulogic -- parity over 'di'
		);
	end component;

	component hamming_7_4_decoder is
		generic (g: common_generics; secdec: boolean := true);
		port (
			di:      in std_ulogic_vector(6 downto 0);
			parity:  in std_ulogic;
			do:     out std_ulogic_vector(3 downto 0);
			single, double: out std_ulogic);
	end component;

	component hamming_7_4_tb is
		generic (g: common_generics);
	end component;

	type vga_configuration is record
		clock_frequency: positive;   -- pixel clock frequency
		h_pulse:         integer;    -- horizontal sync pulse width in pixels
		h_back_porch:    integer;    -- horizontal back porch width in pixels
		h_pixels:        integer;    -- horizontal display width in pixels
		h_front_porch:   integer;    -- horizontal front porch width in pixels
		h_polarity:      std_ulogic; -- horizontal sync pulse polarity (1 = positive, 0 = negative)

		v_pulse:         integer;    -- vertical sync pulse width in rows
		v_back_porch:    integer;    -- vertical back porch width in rows
		v_pixels:        integer;    -- vertical display width in rows
		v_front_porch:   integer;    -- vertical front porch width in rows
		v_polarity:      std_ulogic; -- vertical sync pulse polarity (1 = positive, 0 = negative)
	end record;

	constant vga_640x480: vga_configuration := (
		clock_frequency => 25_175_000,
		h_pulse =>  96, h_back_porch =>  48, h_pixels =>  640, h_front_porch =>  16, h_polarity => '0',
		v_pulse =>   2, v_back_porch =>  33, v_pixels =>  480, v_front_porch =>  10, v_polarity => '0');

	constant vga_800x600: vga_configuration := (
		clock_frequency => 40_000_000,
		h_pulse => 128, h_back_porch =>  88, h_pixels =>  800, h_front_porch =>  40, h_polarity => '1',
		v_pulse =>   4, v_back_porch =>  23, v_pixels =>  600, v_front_porch =>   1, v_polarity => '1');

	constant vga_1024x768: vga_configuration := (
		clock_frequency => 44_900_000,
		h_pulse => 176, h_back_porch =>  56, h_pixels => 1024, h_front_porch =>   8, h_polarity => '1',
		v_pulse => 8,   v_back_porch =>  41, v_pixels =>  800, v_front_porch =>   0, v_polarity => '1');

	constant vga_1920x1200: vga_configuration := (
		clock_frequency => 193_160_000,
		h_pulse => 208, h_back_porch => 336, h_pixels => 1920, h_front_porch => 128, h_polarity => '0',
		v_pulse => 3,   v_back_porch => 38,  v_pixels => 1200, v_front_porch =>   1, v_polarity => '1');

	component vga_controller is
	generic (
		g: common_generics;
		pixel_clock_frequency:  positive := 25_000_000;
		constant cfg: vga_configuration  := vga_640x480); 
	port (
		clk, rst:          in std_ulogic;  -- pixel clock, must run at configured frequency
		h_sync, v_sync:   out std_ulogic;  -- sync pulses
		h_blank, v_blank: out std_ulogic;
		column, row:      out integer);   -- pixel coordinates
	end component;

	component vga_tb is
		generic (g: common_generics; pixel_clock_frequency: positive := 25_000_000; simulation_us: time := 20000 us);
	end component;

	constant led_7_segment_character_length: positive := 4;
	subtype led_7_segment_character is std_ulogic_vector(led_7_segment_character_length - 1 downto 0);
	subtype led_7_segment is std_ulogic_vector(7 downto 0);

	component led_7_segment_display is
		generic (g: common_generics;
			use_bcd_not_hex:         boolean := true;
			refresh_rate_us:         natural := 1500;
			number_of_led_displays: positive := 4);
		port (
			clk:      in   std_ulogic;
			rst:      in   std_ulogic;

			leds_we:  in   std_ulogic;
			leds:     in   std_ulogic_vector((number_of_led_displays * led_7_segment_character_length) - 1 downto 0);

			-- Physical outputs
			an:       out  std_ulogic_vector(number_of_led_displays - 1 downto 0);  -- anodes, controls on/off
			ka:       out  std_ulogic_vector(7 downto 0)); -- cathodes, data on display
	end component;

	component led_7_segment_display_tb is
		generic (g: common_generics);
	end component;

	component sine is
		generic (g: common_generics; pipeline: boolean := true);
		port (
			clk, rst, xwe: in std_ulogic;
			x:  in  std_ulogic_vector(15 downto 0);
			s:  out std_ulogic_vector(15 downto 0));
	end component;

	component cosine is
		generic (g: common_generics; pipeline: boolean := true);
		port (
			clk, rst, xwe: in std_ulogic;
			x:  in  std_ulogic_vector(15 downto 0);
			s:  out std_ulogic_vector(15 downto 0));
	end component;

	component sine_tb is
		generic (g: common_generics);
	end component;

	function max(a: natural; b: natural) return natural;
	function min(a: natural; b: natural) return natural;
	function reverse (a: in std_ulogic_vector) return std_ulogic_vector;
	function invert(slv:std_ulogic_vector) return std_ulogic_vector;
	function parity(slv:std_ulogic_vector; even: boolean) return std_ulogic;
	function parity(slv:std_ulogic_vector; even: std_ulogic) return std_ulogic;
	function or_reduce(slv:std_ulogic_vector) return std_ulogic;
	function and_reduce(slv:std_ulogic_vector) return std_ulogic;
	function select_bit(indexed, selector: std_ulogic_vector) return std_ulogic;
	function priority(order: std_ulogic_vector; high: boolean) return natural;
	function priority(order: std_ulogic_vector; high: boolean) return std_ulogic_vector;
	function mux(a: std_ulogic_vector; b: std_ulogic_vector; sel: std_ulogic) return std_ulogic_vector;
	function mux(a: std_ulogic; b: std_ulogic; sel: std_ulogic) return std_ulogic;
	function mux(a, b : std_ulogic_vector) return std_ulogic;
	function decode(encoded: std_ulogic_vector) return std_ulogic_vector;
	function to_std_ulogic_vector(s: string) return std_ulogic_vector;
	function logical(b: boolean) return std_ulogic;
	function bit_count_f(s: std_ulogic_vector) return integer;
	function hex_char_to_std_ulogic_vector_tb(hc: character) return std_ulogic_vector;

	type ulogic_string is array(integer range <>) of std_ulogic_vector(7 downto 0);
  	function to_std_ulogic_vector(s: string) return ulogic_string;

	-- synthesis translate_off
	subtype configuration_name is string(1 to 8);

	type configuration_item is record
		name:  configuration_name;
		value: integer;
	end record;

	type configuration_items is array(integer range <>) of configuration_item;

	function search_configuration_tb(find_me: configuration_name; ci: configuration_items) return integer;
	procedure read_configuration_tb(file_name:  string; ci: inout configuration_items);
	procedure write_configuration_tb(file_name: string; ci: configuration_items);
	-- synthesis translate_on
end;

package body util is

	function max(a: natural; b: natural) return natural is
	begin
		if (a > b) then return a; else return b; end if;
	end function;

	function min(a: natural; b: natural) return natural is
	begin
		if (a < b) then return a; else return b; end if;
	end function;

	function n_bits(x: natural) return natural is -- Not synthesizable
		variable x1: natural := max(x, 1) - 1;
		variable n:  natural := 1;
	begin
		while x1 > 1 loop
			x1 := x1 / 2;
			n  := n + 1;
		end loop;
		return n;
	end function;

	function n_bits(x: std_ulogic_vector) return natural is -- Not synthesizable
	begin
		return n_bits(x'high);
	end function;

	-- <https://stackoverflow.com/questions/13584307>
	function reverse (a: in std_ulogic_vector) return std_ulogic_vector is
		variable result: std_ulogic_vector(a'range);
		alias aa: std_ulogic_vector(a'reverse_range) is a;
	begin
		for i in aa'range loop
			result(i) := aa(i);
		end loop;
		return result;
	end;

	function invert(slv: std_ulogic_vector) return std_ulogic_vector is
		variable z: std_ulogic_vector(slv'range);
	begin
		for i in slv'range loop
			z(i) := not(slv(i));
		end loop;
		return z;
	end;

	function parity(slv: std_ulogic_vector; even: boolean) return std_ulogic is
		variable z: std_ulogic := '0';
	begin
		if not even then
			z := '1';
		end if;
		for i in slv'range loop
			z := z xor slv(i);
		end loop;
		return z;
	end;

	function parity(slv:std_ulogic_vector; even: std_ulogic) return std_ulogic is
		variable z: boolean := false;
	begin
		if even = '1' then
			z := true;
		end if;
		return parity(slv, z);
	end;

	function or_reduce(slv:std_ulogic_vector) return std_ulogic is
		variable z: std_ulogic := '0';
	begin
		for i in slv'range loop
			z := z or slv(i);
		end loop;
		return z;
	end;

	function and_reduce(slv:std_ulogic_vector) return std_ulogic is
		variable z: std_ulogic := '1';
	begin
		for i in slv'range loop
			z := z and slv(i);
		end loop;
		return z;
	end;
	
	function select_bit(indexed, selector: std_ulogic_vector) return std_ulogic is
		variable z: std_ulogic := 'X';
	begin
		assert n_bits(indexed) = selector'high + 1 severity failure;
		for i in indexed'range loop
			if i = to_integer(unsigned(selector)) then
				z := indexed(i);
			end if;
		end loop;
		return z;
	end;

	function priority(order: std_ulogic_vector; high: boolean) return natural is
		variable p: natural := 0;
	begin
		if not high then
			for i in order'high + 1 downto 1 loop
				if order(i-1) = '1' then
					p := i - 1;
				end if;
			end loop;
		else
			for i in 1 to order'high + 1 loop
				if order(i-1) = '1' then
					p := i - 1;
				end if;
			end loop;
		end if;
		return p;
	end;

	function priority(order: std_ulogic_vector; high: boolean) return std_ulogic_vector is
		variable length: natural := n_bits(order'length);
	begin
		return std_ulogic_vector(to_unsigned(priority(order, high), length));
	end;

	function mux(a: std_ulogic_vector; b: std_ulogic_vector; sel: std_ulogic) return std_ulogic_vector is
		variable m: std_ulogic_vector(a'range) := (others => 'X');
	begin
		if sel = '0' then m := a; else m := b; end if;
		return m;
	end;

	function mux(a: std_ulogic; b: std_ulogic; sel: std_ulogic) return std_ulogic is
		variable m: std_ulogic := 'X';
	begin
		if sel = '0' then m := a; else m := b; end if;
		return m;
	end;

	function mux(a, b : std_ulogic_vector) return std_ulogic is
		variable r: std_ulogic_vector(b'length - 1 downto 0) := (others => 'X');
		variable i: integer;
	begin
		r := b;
		i := to_integer(unsigned(a));
		return r(i);
	end;

	function decode(encoded : std_ulogic_vector) return std_ulogic_vector is
		variable r: std_ulogic_vector((2 ** encoded'length) - 1 downto 0) := (others => '0');
		variable i: natural;
	begin
		i    := to_integer(unsigned(encoded));
		r(i) := '1';
		return r;
	end;

	function logical(b: boolean) return std_ulogic is
	begin
		if b then return '1'; else return '0'; end if;
	end;

	function hex_char_to_std_ulogic_vector_tb(hc: character) return std_ulogic_vector is
		variable slv: std_ulogic_vector(3 downto 0);
	begin
		case hc is
		when '0' => slv := "0000";
		when '1' => slv := "0001";
		when '2' => slv := "0010";
		when '3' => slv := "0011";
		when '4' => slv := "0100";
		when '5' => slv := "0101";
		when '6' => slv := "0110";
		when '7' => slv := "0111";
		when '8' => slv := "1000";
		when '9' => slv := "1001";
		when 'A' => slv := "1010";
		when 'a' => slv := "1010";
		when 'B' => slv := "1011";
		when 'b' => slv := "1011";
		when 'C' => slv := "1100";
		when 'c' => slv := "1100";
		when 'D' => slv := "1101";
		when 'd' => slv := "1101";
		when 'E' => slv := "1110";
		when 'e' => slv := "1110";
		when 'F' => slv := "1111";
		when 'f' => slv := "1111";
		when others => slv := "XXXX";
		end case;
		assert (slv /= "XXXX") report " not a valid hex character: " & hc  severity failure;
		return slv;
	end;

	function bit_count_f(s : std_ulogic_vector) return integer is
		variable count: natural := 0;
	begin
		for i in s'range loop
			if s(i) = '1' then 
				count := count + 1; 
			end if;
		end loop;
		return count;
	end;

	-- <https://stackoverflow.com/questions/30519849/vhdl-convert-string-to-std-logic-vector>
	function to_std_ulogic_vector(s: string) return std_ulogic_vector is
	    variable ret: std_ulogic_vector(s'length*8-1 downto 0);
	begin
	    for i in s'range loop
		ret(i*8+7 downto i*8) := std_ulogic_vector(to_unsigned(character'pos(s(i)), 8));
	    end loop;
	    return ret;
	end;

	function to_std_ulogic_vector(s: string) return ulogic_string is
	    variable ret: ulogic_string(s'range);
	begin
		for i in s'range loop
			ret(i) := std_ulogic_vector(to_unsigned(character'pos(s(i)), 8));
		end loop;
		return ret;
	end;

	-- synthesis translate_off

	-- Find a string in a configuration items array, or returns -1 on
	-- failure to find the string.
	function search_configuration_tb(find_me: configuration_name; ci: configuration_items) return integer is
	begin
		for i in ci'range loop
			if ci(i).name = find_me then
				return i;
			end if;
		end loop;
		return -1;
	end;

	-- VHDL provides quite a limited set of options for dealing with
	-- operations that are not synthesizeable but would be useful for
	-- in test benches. This method provides a crude way of reading
	-- in configurable options. It has a very strict format.
	--
	-- The format is line oriented, it expects a string on a line
	-- with a length equal to the "configuration_name" type, which
	-- is a subtype of "string". It finds the corresponding record
	-- in configuration_items if it exists. It then reads in an
	-- integer from the next line and sets the record for it.
	--
	-- Any deviation from this format causes an error and the simulation
	-- to halt, whilst not a good practice to do error checking with asserts
	-- there is no better way in VHDL in this case. The only sensible
	-- action on an error would for the configuration file to be fixed
	-- anyway.
	--
	-- Comment lines and variable length strings would be nice, but
	-- are too much of a hassle.
	--
	-- The configuration function only deal with part of the configuration
	-- process, it does not deal with deserialization into structures
	-- more useful to the user - like into individual signals.
	--
	procedure read_configuration_tb(file_name: string; ci: inout configuration_items) is
		file     in_file: text is in file_name;
		variable in_line: line;
		variable d:       integer;
		variable s:       configuration_name;
		variable index:   integer;
	begin
		while not endfile(in_file) loop

			readline(in_file, in_line);
			read(in_line, s);
			index := search_configuration_tb(s, ci);

			assert index >= 0 report "Unknown configuration item: " & s severity failure;

			readline(in_file, in_line);
			read(in_line, d);

			ci(index).value := d;

			report "Config Item: '" & ci(index).name & "' = " & integer'image(ci(index).value);
		end loop;
		file_close(in_file);
	end procedure;

	procedure write_configuration_tb(file_name: string; ci: configuration_items) is
		file     out_file: text is out file_name;
		variable out_line: line;
	begin
		for i in ci'range loop
			write(out_line, ci(i).name);
			writeline(out_file, out_line);
			write(out_line, ci(i).value);
			writeline(out_file, out_line);
		end loop;
	end procedure;

	-- synthesis translate_on
end;

------------------------- Utility Test Bench ----------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity util_tb is
	generic (g: common_generics);
end entity;

architecture behav of util_tb is
begin
	-- The "io_pins_tb" works correctly, however in GHDL 0.29, compiled under
	-- Windows, fails to simulate this component correctly, resulting
	-- in a crash. This does not affect the Linux build of GHDL. It has
	-- something to do with 'Z' values for std_logic types.
	uut_io_pins:  work.util.io_pins_tb              generic map (g => g);
	uut_timer_us: work.util.timer_us_tb             generic map (g => g);
	uut_full_add: work.util.full_adder_tb           generic map (g => g);
	uut_fifo:     work.util.fifo_tb                 generic map (g => g);
	uut_counter:  work.util.counter_tb              generic map (g => g);
	uut_ucpu:     work.util.ucpu_tb                 generic map (g => g);
	uut_rdivider: work.util.restoring_divider_tb    generic map (g => g);
	uut_debounce: work.util.debounce_us_tb          generic map (g => g);
	uut_rst_gen:  work.util.reset_generator_tb      generic map (g => g);
	uut_bit_cnt:  work.util.bit_count_tb            generic map (g => g);
	uut_majority: work.util.majority_tb             generic map (g => g);
	uut_delay_ln: work.util.delay_line_tb           generic map (g => g);
	uut_rising:   work.util.rising_edge_detector_tb generic map (g => g);
	uut_shiftReg: work.util.shift_register_tb       generic map (g => g);
	uut_lfsr:     work.util.lfsr_tb                 generic map (g => g);
	uut_gray:     work.util.gray_tb                 generic map (g => g);
	uut_ham:      work.util.hamming_7_4_tb          generic map (g => g); -- Oink!
	uut_vga:      work.util.vga_tb                  generic map (g => g, simulation_us => 1 us);
	uut_sine:     work.util.sine_tb                 generic map (g => g);
	uut_7_seg:   work.util.led_7_segment_display_tb generic map (g => g);

	stimulus_process: process
	begin
		assert max(5, 4)                 =  5      severity failure;
		assert work.util.min(5, 4)       =  4      severity failure;
		assert n_bits(1)                 =  1      severity failure;
		assert n_bits(2)                 =  1      severity failure;
		assert n_bits(7)                 =  3      severity failure;
		assert n_bits(8)                 =  3      severity failure;
		assert n_bits(9)                 =  4      severity failure;
		assert reverse("1")              =  "1"    severity failure;
		assert reverse("0")              =  "0"    severity failure;
		assert reverse("10")             =  "01"   severity failure;
		assert reverse("11")             =  "11"   severity failure;
		assert reverse("0101")           =  "1010" severity failure;
		assert invert("1")               =  "0"    severity failure;
		assert invert("0")               =  "1"    severity failure;
		assert invert("0101")            =  "1010" severity failure;
		assert select_bit("01000", "01") =  '1'    severity failure;
		assert parity("0", true)         =  '0'    severity failure;
		assert parity("1", true)         =  '1'    severity failure;
		assert parity("11", true)        =  '0'    severity failure;
		assert parity("1010001", true)   =  '1'    severity failure;
		assert parity("0", false)        =  '1'    severity failure;
		assert parity("1", false)        =  '0'    severity failure;
		assert parity("11", false)       =  '1'    severity failure;
		assert parity("1010001", false)  =  '0'    severity failure;
		assert or_reduce("0000")         =  '0'    severity failure;
		assert or_reduce("0")            =  '0'    severity failure;
		assert or_reduce("1")            =  '1'    severity failure;
		assert or_reduce("11")           =  '1'    severity failure;
		assert or_reduce("10")           =  '1'    severity failure;
		assert or_reduce("01")           =  '1'    severity failure;
		assert and_reduce("01")          =  '0'    severity failure;
		assert and_reduce("11")          =  '1'    severity failure;
		assert and_reduce("1")           =  '1'    severity failure;
		assert and_reduce("0")           =  '0'    severity failure;
		assert and_reduce("10")          =  '0'    severity failure;
		assert priority("01001", false)  =  1      severity failure;
		assert mux("1010", "0101", '0')  =  "1010" severity failure;
		assert mux("1010", "0101", '1')  =  "0101" severity failure;
		assert decode("00")              =  "0001" severity failure;
		assert decode("01")              =  "0010" severity failure;
		assert decode("10")              =  "0100" severity failure;
		assert decode("11")              =  "1000" severity failure;
		-- n_bits(x: std_ulogic_vector) return natural;
		-- mux(a, b : std_ulogic_vector) return std_ulogic;
		wait;
	end process;
end architecture;

------------------------- Function Test Bench ---------------------------------------

------------------------- Test bench clock source -----------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.util.common_generics;

entity clock_source_tb is
	generic (g: common_generics; hold_rst: positive);
	port (
		stop:            in     std_ulogic := '0';
		clk:             out    std_ulogic;
		clk_with_jitter: out    std_ulogic := '0';
		rst:             out    std_ulogic := '0');
end entity;

architecture rtl of clock_source_tb is
	constant clock_period: time      :=  1000 ms / g.clock_frequency;
	signal jitter_delay:   time      := 0 ns;
	signal jitter_clk:     std_ulogic := '0';
begin
	jitter_clk_process: process
		variable seed1, seed2: positive;
		variable r: real;
		variable jit_high, jit_low: time  := 0 ns;
	begin
		while stop = '0' loop
			uniform(seed1, seed2, r);
			jit_high := r * g.delay;
			uniform(seed1, seed2, r);
			jit_low := r * g.delay;
			uniform(seed1, seed2, r);
			if r < 0.5 then jit_high := -jit_high; end if;
			uniform(seed1, seed2, r);
			if r < 0.5 then jit_low := -jit_low; end if;
			clk_with_jitter <= '1';
			wait for (clock_period / 2) + jit_high;
			clk_with_jitter <= '0';
			wait for (clock_period / 2) + jit_low;
		end loop;
		wait;
	end process;

	clk_process: process
	begin
		while stop = '0' loop
			clk <= '1';
			wait for clock_period / 2;
			clk <= '0';
			wait for clock_period / 2;
		end loop;
		wait;
	end process;

	rst_process: process
	begin
		rst <= '1';
		wait for clock_period * hold_rst;
		rst <= '0';
		wait;
	end process;

end architecture;

------------------------- Generic Register of std_ulogic_vector ----------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity reg is
	generic (g: common_generics; N: positive);
	port (
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		we:  in  std_ulogic;
		di:  in  std_ulogic_vector(N - 1 downto 0);
		do:  out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture rtl of reg is
	signal r_c, r_n: std_ulogic_vector(N - 1 downto 0) := (others => '0');
begin
	do <= r_c after g.delay;

	process(rst, clk)
	begin
		if rst = '1' and g.asynchronous_reset then
			r_c <= (others => '0') after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				r_c <= (others => '0') after g.delay;
			else
				r_c <= r_n after g.delay;
			end if;
		end if;
	end process;

	process(r_c, di, we)
	begin
		if we = '1' then
			r_n <= di after g.delay;
		else
			r_n <= r_c after g.delay;
		end if;
	end process;
end;

------------------------- Generic Register of std_ulogic_vector ----------------------

------------------------- Shift register --------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

-- https://stackoverflow.com/questions/36342960/optional-ports-in-vhdl
entity shift_register is
	generic (g: common_generics; N: positive);
	port (
		clk:     in  std_ulogic;
		rst:     in  std_ulogic;
		we:      in  std_ulogic;
		di:      in  std_ulogic;
		do:      out std_ulogic;

		-- optional
		load_we: in  std_ulogic := '0';
		load_i:  in  std_ulogic_vector(N - 1 downto 0) := (others => '0');
		load_o:  out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture rtl of shift_register is
	signal r_c, r_n : std_ulogic_vector(N - 1 downto 0) := (others => '0');
begin
	do     <= r_c(0);
	load_o <= r_c;

	process(rst, clk)
	begin
		if rst = '1' and g.asynchronous_reset then
			r_c <= (others => '0') after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				r_c <= (others => '0') after g.delay;
			else
				r_c <= r_n after g.delay;
			end if;
		end if;
	end process;

	process(r_c, di, we, load_i, load_we)
	begin
		if load_we = '1' then
			r_n <= load_i after g.delay;
		else
			r_n <= "0" & r_c(N - 1 downto 1) after g.delay;
			if we = '1' then
				r_n(N-1) <= di after g.delay;
			end if;
		end if;
	end process;
end;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity shift_register_tb is
	generic (g: common_generics);
end entity;

architecture behav of shift_register_tb is
	constant N: positive := 8;
	constant clock_period: time :=  1000 ms / g.clock_frequency;
	signal we: std_ulogic := '0';
	signal di: std_ulogic := '0';
	signal do: std_ulogic := '0';

	signal clk, rst: std_ulogic := '0';
	signal stop: std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.shift_register
	generic map (g => g, N => N) port map (clk => clk, rst => rst, we => we, di => di, do => do);

	stimulus_process: process
	begin
		-- put a bit into the shift register and wait
		-- for it to come out the other size
		wait until rst = '0';
		di <= '1';
		we <= '1';
		wait for clock_period;
		di <= '0';
		we <= '0';
		for I in 0 to 7 loop
			assert do = '0' report "bit appeared to quickly";
			wait for clock_period;
		end loop;
		assert do = '1' report "bit disappeared in shift register";
		wait for clock_period * 1;
		assert do = '0' report "extra bit set in shift register";
		stop <= '1';
		wait;
	end process;
end;
------------------------- Shift register --------------------------------------------

------------------------- Microsecond Timer -----------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.max;
use work.util.n_bits;
use work.util.common_generics;

entity timer_us is
	generic (g: common_generics; timer_period_us: natural);
	port (
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		co:  out std_ulogic);
end timer_us;

architecture rtl of timer_us is
	constant cycles:   natural := (g.clock_frequency / 1000000) * timer_period_us;
	subtype  counter is unsigned(max(1, n_bits(cycles) - 1) downto 0);
	signal   c_c, c_n: counter := (others => '0');
begin
	process (clk, rst)
	begin
		if rst = '1' and g.asynchronous_reset then
			c_c <= (others => '0') after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				c_c <= (others => '0') after g.delay;
			else
				c_c <= c_n after g.delay;
			end if;
		end if;
	end process;

	process (c_c)
	begin
		if c_c = (cycles - 1) then
			c_n <= (others => '0') after g.delay;
			co  <= '1' after g.delay;
		else
			c_n <= c_c + 1 after g.delay;
			co  <= '0' after g.delay;
		end if;
	end process;
end;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity timer_us_tb is
	generic (g: common_generics);
end;

architecture behav of timer_us_tb is
	constant clock_period: time := 1000 ms / g.clock_frequency;
	signal co: std_ulogic        := 'X';
	signal clk, rst: std_ulogic  := '0';
	signal stop: std_ulogic      := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.timer_us
		generic map (g => g, timer_period_us => 1)
		port map (clk => clk, rst => rst, co => co);

	stimulus_process: process
	begin
		wait for 1 us;
		assert co = '0' severity failure;
		wait for clock_period;
		assert co = '1' severity failure;
		stop <= '1';
		wait;
	end process;
end;

------------------------- Microsecond Timer -----------------------------------------

------------------------- Rising Edge Detector --------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity rising_edge_detector is
	generic (g: common_generics);
	port (
		clk:    in  std_ulogic;
		rst:    in  std_ulogic;
		di:     in  std_ulogic;
		do:     out std_ulogic);
end;

architecture rtl of rising_edge_detector is
	signal sin_0: std_ulogic := '0';
	signal sin_1: std_ulogic := '0';
begin
	red: process(clk, rst)
	begin
		if rst = '1' and g.asynchronous_reset then
			sin_0 <= '0' after g.delay;
			sin_1 <= '0' after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				sin_0 <= '0' after g.delay;
				sin_1 <= '0' after g.delay;
			else
				sin_0 <=    di after g.delay;
				sin_1 <= sin_0 after g.delay;
			end if;
		end if;
	end process;
	do <= not sin_1 and sin_0;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity rising_edge_detector_tb is
	generic (g: common_generics);
end;

architecture behav of rising_edge_detector_tb is
	constant clock_period: time := 1000 ms / g.clock_frequency;
	signal di:  std_ulogic := '0';
	signal do: std_ulogic := 'X';

	signal clk, rst: std_ulogic := '0';
	signal stop: std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.rising_edge_detector
		generic map (g => g)
		port map (clk => clk, rst => rst, di => di, do => do);

	stimulus_process: process
	begin
		wait for clock_period * 5;
		assert do = '0' severity failure;
		wait for clock_period;
		di <= '1';
		wait for clock_period * 0.5;
		assert do = '1' severity failure;
		wait for clock_period * 1.5;
		di <= '0';
		assert do = '0' severity failure;
		wait for clock_period;
		assert do = '0' severity failure;

		assert stop = '0' report "Test bench not run to completion";
		stop <= '1';
		wait;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity rising_edge_detectors is
	generic (g: common_generics; N: positive);
	port (
		clk:    in  std_ulogic;
		rst:    in  std_ulogic;
		di:     in  std_ulogic_vector(N - 1 downto 0);
		do:     out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture structural of rising_edge_detectors is
begin
	changes: for i in N - 1 downto 0 generate
		d_instance: work.util.rising_edge_detector
			generic map (g => g)
			port map (clk => clk, rst => rst, di => di(i), do => do(i));
	end generate;
end architecture;

------------------------- Rising Edge Detector --------------------------------------

------------------------- Half Adder ------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity half_adder is
	generic (g: common_generics);
	port (
		a:     in  std_ulogic;
		b:     in  std_ulogic;
		sum:   out std_ulogic;
		carry: out std_ulogic);
end entity;

architecture rtl of half_adder is
begin
	sum   <= a xor b after g.delay;
	carry <= a and b after g.delay;
end architecture;

------------------------- Half Adder ------------------------------------------------

------------------------- Full Adder ------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity full_adder is
	generic (g: common_generics);
	port (
		x:     in    std_ulogic;
		y:     in    std_ulogic;
		z:     in    std_ulogic;
		sum:   out   std_ulogic;
		carry: out   std_ulogic);
end entity;

architecture rtl of full_adder is
	signal carry1, carry2, sum1: std_ulogic;
begin
	ha1: entity work.half_adder generic map (g => g) port map (a => x,    b => y, sum => sum1, carry => carry1);
	ha2: entity work.half_adder generic map (g => g) port map (a => sum1, b => z, sum => sum,  carry => carry2);
	carry <= carry1 or carry2 after g.delay;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity full_adder_tb is
	generic (g: common_generics);
end entity;

architecture behav of full_adder_tb is
	constant clock_period: time  := 1000 ms / g.clock_frequency;
	signal x, y, z:    std_ulogic := '0';
	signal sum, carry: std_ulogic := '0';

	type stimulus_data   is array (0 to 7)              of std_ulogic_vector(2 downto 0);
	type stimulus_result is array (stimulus_data'range) of std_ulogic_vector(0 to     1);

	constant data: stimulus_data := (
		0 => "000", 1 => "001",
		2 => "010", 3 => "011",
		4 => "100", 5 => "101",
		6 => "110", 7 => "111");

	constant result: stimulus_result := (
		0 => "00",  1 => "10",
		2 => "10",  3 => "01",
		4 => "10",  5 => "01",
		6 => "01",  7 => "11");

	signal clk, rst: std_ulogic := '0';
	signal stop: std_ulogic     := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.full_adder 
		generic map (g => g)
		port map (x => x, y => y, z => z, sum => sum, carry => carry);

	stimulus_process: process
	begin
		wait for clock_period;
		for i in data'range loop
			x <= data(i)(0);
			y <= data(i)(1);
			z <= data(i)(2);
			wait for clock_period;
			assert sum = result(i)(0) and carry = result(i)(1)
				report
					"For: "       & std_ulogic'image(x) & std_ulogic'image(y) & std_ulogic'image(z) &
					" Got: "      & std_ulogic'image(sum)          & std_ulogic'image(carry) &
					" Expected: " & std_ulogic'image(result(i)(0)) & std_ulogic'image(result(i)(1))
				severity failure;
			wait for clock_period;
		end loop;

		stop <= '1';
		wait;
	end process;
end architecture;

------------------------- Full Adder ------------------------------------------------

------------------------- FIFO ------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity fifo is
	generic (g: common_generics; 
		data_width: positive; 
		fifo_depth: positive;
		read_first: boolean := true);
	port (
		clk:   in  std_ulogic;
		rst:   in  std_ulogic;
		di:    in  std_ulogic_vector(data_width - 1 downto 0);
		we:    in  std_ulogic;
		re:    in  std_ulogic;
		do:    out std_ulogic_vector(data_width - 1 downto 0);

		-- optional
		full:  out std_ulogic := '0';
		empty: out std_ulogic := '1');
end fifo;

architecture behavior of fifo is
	type fifo_data_t is array (0 to fifo_depth - 1) of std_ulogic_vector(di'range);
	signal data: fifo_data_t := (others => (others => '0'));
	function rindex_init return integer is
	begin
		if read_first then
			return 0;
		end if;
		return fifo_depth - 1;
	end function;

	signal count:  integer range 0 to fifo_depth := 0;
	signal windex: integer range 0 to fifo_depth - 1 := 0;
	signal rindex: integer range 0 to fifo_depth - 1 := rindex_init;

	signal is_full:  std_ulogic := '0';
	signal is_empty: std_ulogic := '1';
begin
	-- TODO: Allow read to be configurable to next or current rindex
	do       <= data(rindex) after g.delay;
	full     <= is_full after g.delay;  -- buffer these bad boys
	empty    <= is_empty after g.delay;
	is_full  <= '1' when count = fifo_depth else '0' after g.delay;
	is_empty <= '1' when count = 0          else '0' after g.delay;

	process (rst, clk) is
	begin
		if rst = '1' and g.asynchronous_reset then
			windex <= 0 after g.delay;
			count  <= 0 after g.delay;
			rindex <= rindex_init after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				windex <= 0 after g.delay;
				count  <= 0 after g.delay;
				rindex <= rindex_init after g.delay;
			else
				if we = '1' and re = '0' then
					if is_full = '0' then
						count <= count + 1 after g.delay;
					end if;
				elsif we = '0' and re = '1' then
					if is_empty = '0' then
						count <= count - 1 after g.delay;
					end if;
				end if;

				if re = '1' and is_empty = '0' then
					if rindex = (fifo_depth - 1) then
						rindex <= 0 after g.delay;
					else
						rindex <= rindex + 1 after g.delay;
					end if;
				end if;

				if we = '1' and is_full = '0' then
					if windex = (fifo_depth - 1) then
						windex <= 0 after g.delay;
					else
						windex <= windex + 1 after g.delay;
					end if;
					data(windex) <= di after g.delay;
				end if;
			end if;
		end if;
	end process;
end behavior;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity fifo_tb is
	generic (g: common_generics);
end entity;

architecture behavior of fifo_tb is
	constant clock_period: time  := 1000 ms / g.clock_frequency;
	constant data_width: positive := 8;
	constant fifo_depth: positive := 16;

	signal di:       std_ulogic_vector(data_width - 1 downto 0) := (others => '0');
	signal re:       std_ulogic := '0';
	signal we:       std_ulogic := '0';

	signal do:       std_ulogic_vector(data_width - 1 downto 0) := (others => '0');
	signal empty:    std_ulogic := '0';
	signal full:     std_ulogic := '0';

	signal clk, rst: std_ulogic := '0';
	signal stop_w:   std_ulogic := '0';
	signal stop_r:   std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	stop <= '1' when stop_w = '1' and stop_r = '1' else '0';

	uut: entity work.fifo
		generic map (g => g, data_width => data_width, fifo_depth => fifo_depth)
		port map (
			clk   => clk,
			rst   => rst,
			di    => di,
			we    => we,
			re    => re,
			do    => do,
			full  => full,
			empty => empty);

	write_process: process
		variable counter: unsigned (data_width - 1 downto 0) := (others => '0');
	begin
		wait for clock_period * 20;

		for i in 1 to 32 loop
			counter := counter + 1;
			di <= std_ulogic_vector(counter);
			wait for clock_period * 1;
			we <= '1';
			wait for clock_period * 1;
			we <= '0';
		end loop;

		wait for clock_period * 20;

		for i in 1 to 32 loop
			counter := counter + 1;
			di <= std_ulogic_vector(counter);
			wait for clock_period * 1;
			we <= '1';
			wait for clock_period * 1;
			we <= '0';
		end loop;

		stop_w <= '1';
		wait;
	end process;

	read_process: process
	begin
		wait for clock_period * 60;
		re <= '1';
		wait for clock_period * 60;
		re <= '0';
		wait for clock_period * 256 * 2;
		re <= '1';

		stop_r <= '1';
		wait;
	end process;
end architecture;

------------------------- FIFO ------------------------------------------------------

------------------------- Free running counter --------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity counter is
	generic (g: common_generics;
		N:                    positive);
	port (
		clk:     in  std_ulogic;
		rst:     in  std_ulogic;
		ce:      in  std_ulogic;
		cr:      in  std_ulogic;
		dout:    out std_ulogic_vector(N - 1 downto 0);

		-- optional
		load_we: in  std_ulogic := '0';
		load_i:  in  std_ulogic_vector(N - 1 downto 0) := (others => '0'));
end entity;

architecture rtl of counter is
	signal c_c, c_n: unsigned(N - 1 downto 0) := (others => '0');
begin
	dout <= std_ulogic_vector(c_c) after g.delay;

	process(clk, rst)
	begin
		if rst = '1' and g.asynchronous_reset then
			c_c <= (others => '0') after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				c_c <= (others => '0') after g.delay;
			else
				c_c <= c_n after g.delay;
			end if;
		end if;
	end process;

	process(c_c, cr, ce, load_we, load_i)
	begin
		c_n <= c_c;
		if load_we = '1' then
			c_n <= unsigned(load_i) after g.delay;
		else
			if cr = '1' then
				c_n <= (others => '0') after g.delay;
			elsif ce = '1' then
				c_n <= c_c + 1 after g.delay;
			end if;
		end if;
	end process;

end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity counter_tb is
	generic (g: common_generics);
end entity;

architecture behavior of counter_tb is
	constant clock_period: time     := 1000 ms / g.clock_frequency;
	constant length:       positive := 2;

	-- inputs
	signal ce: std_ulogic := '0';
	signal cr: std_ulogic := '0';

	-- outputs
	signal dout: std_ulogic_vector(length - 1 downto 0);

	-- test data
	type stimulus_data   is array (0 to 16)             of std_ulogic_vector(1 downto 0);
	type stimulus_result is array (stimulus_data'range) of std_ulogic_vector(0 to     1);

	constant data: stimulus_data := (
		 0 => "00",  1 => "00",
		 2 => "01",  3 => "01",
		 4 => "00",  5 => "00",
		 6 => "10",  7 => "00",
		 8 => "01",  9 => "01",
		10 => "11", 11 => "00",
		12 => "01", 13 => "01",
		14 => "01", 15 => "01",
		16 => "01");

	constant result: stimulus_result := (
		 0 => "00",  1 => "00",
		 2 => "00",  3 => "01",
		 4 => "10",  5 => "10",
		 6 => "10",  7 => "00",
		 8 => "00",  9 => "01",
		10 => "10", 11 => "00",
		12 => "00", 13 => "01",
		14 => "10", 15 => "11",
		16 => "00");

	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.counter
		generic map (g => g, N => length)
		port map (
			clk   => clk,
			rst   => rst,
			ce    => ce,
			cr    => cr,
			dout  => dout);

	stimulus_process: process
	begin
		wait for clock_period;
		for i in data'range loop
			ce <= data(i)(0);
			cr <= data(i)(1);
			wait for clock_period;
			assert dout = result(i)
				report
					"For: ce("    & std_ulogic'image(ce) & ") cr(" & std_ulogic'image(cr) & ") " &
					" Got: "      & integer'image(to_integer(unsigned(dout))) &
					" Expected: " & integer'image(to_integer(unsigned(result(i))))
				severity failure;
		end loop;
		stop <= '1';
		wait;
	end process;

end architecture;

------------------------- Free running counter --------------------------------------

------------------------- Linear Feedback Shift Register ----------------------------
-- For good sources on LFSR see
-- * https://sites.ualberta.ca/~delliott/ee552/studentAppNotes/1999f/Drivers_Ed/lfsr.html
-- * https://en.wikipedia.org/wiki/Linear-feedback_shift_register
--
-- Some optimal taps
--
-- Taps start at the left most std_ulogic element of tap at '0' and proceed to
-- the highest bit. To instantiate an instance of the LFSR set tap to a
-- standard logic vector one less the size of LFSR that you want. An 8-bit
-- LFSR can be made by setting it 'tap' to "0111001". The LFSR will need to
-- be loaded with a seed value, set 'do' to that value and assert 'we'. The
-- LFSR will only run when 'ce' is asserted, otherwise it will preserve the
-- current value.
--
-- Number of bits   Taps      Cycle Time
--      8           1,2,3,7    255
--      16          1,2,4,15   65535
--      32          1,5,6,31   4294967295
--
-- This component could also be used to calculate CRCs, which are basically 
-- the same computation. 
--

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity lfsr is
	generic (g: common_generics; constant tap: std_ulogic_vector);
	port
	(
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		ce:  in  std_ulogic := '1';
		we:  in  std_ulogic;
		di:  in  std_ulogic_vector(tap'high + 1 downto tap'low);
		do:  out std_ulogic_vector(tap'high + 1 downto tap'low));
end entity;

architecture rtl of lfsr is
	signal r_c, r_n : std_ulogic_vector(di'range) := (others => '0');
begin
	do <= r_c after g.delay;

	process(rst, clk)
	begin
		if rst = '1' and g.asynchronous_reset then
			r_c <= (others => '0') after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				r_c <= (others => '0') after g.delay;
			else
				r_c <= r_n after g.delay;
			end if;
		end if;
	end process;

	process(r_c, di, we, ce)
	begin
		if we = '1' then
			r_n <= di after g.delay;
		elsif ce = '1' then
			r_n(r_n'high) <= r_c(r_c'low);

			for i in tap'high downto tap'low loop
				if tap(i) = '1' then
					r_n(i) <= r_c(r_c'low) xor r_c(i+1) after g.delay;
				else
					r_n(i) <= r_c(i+1) after g.delay;
				end if;
			end loop;
		else
			r_n <= r_c;
		end if;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity lfsr_tb is
	generic (g: common_generics);
end entity;

architecture behavior of lfsr_tb is
	constant clock_period: time     := 1000 ms / g.clock_frequency;
	signal we: std_ulogic := '0';
	signal do, di: std_ulogic_vector(7 downto 0) := (others => '0');

	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.lfsr
		generic map (g => g, tap => "0111001")
		port map (clk => clk,
			 rst => rst,
			 we => we,
			 di => di,
			 do => do);

	stimulus_process: process
	begin
		wait for clock_period * 2;
		we   <= '1';
		di   <= "00000001";
		wait for clock_period;
		we   <= '0';
		stop <= '1';
		wait;
	end process;

end architecture;

------------------------- Linear Feedback Shift Register ----------------------------

------------------------- I/O Pin Controller ----------------------------------------
-- @todo Test this in hardware
--
-- This is a simple I/O pin control module, there is a control register which
-- sets whether the pins are to be read in (control = '0') or set to the value written to
-- "din" (control = '1').

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity io_pins is
	generic (g: common_generics; N: positive);
	port
	(
		clk:         in    std_ulogic;
		rst:         in    std_ulogic;
		control:     in    std_ulogic_vector(N - 1 downto 0);
		control_we:  in    std_ulogic;
		din:         in    std_ulogic_vector(N - 1 downto 0);
		din_we:      in    std_ulogic;
		dout:        out   std_ulogic_vector(N - 1 downto 0);
		pins:        inout std_logic_vector(N - 1 downto 0));
end entity;

architecture rtl of io_pins is
	signal control_o: std_ulogic_vector(control'range) := (others => '0');
	signal din_o:     std_ulogic_vector(din'range)     := (others => '0');
begin

	control_r: entity work.reg generic map (g => g, N => N) 
				port map (clk => clk, rst => rst, di => control, we => control_we, do => control_o);
	din_r:     entity work.reg generic map (g => g, N => N)
				port map (clk => clk, rst => rst, di => din,     we => din_we,     do => din_o);

	pins_i: for i in control_o'range generate
		dout(i) <= pins(i)  when control_o(i) = '0' else '0' after g.delay;
		pins(i) <= din_o(i) when control_o(i) = '1' else 'Z' after g.delay;
	end generate;

end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity io_pins_tb is
	generic (g: common_generics);
end entity;

architecture behavior of io_pins_tb is
	constant clock_period: time := 1000 ms / g.clock_frequency;
	constant N: positive := 8;

	signal control: std_ulogic_vector(N - 1 downto 0) := (others => '0');
	signal din:     std_ulogic_vector(N - 1 downto 0) := (others => '0');
	signal dout:    std_ulogic_vector(N - 1 downto 0) := (others => '0');
	signal pins:    std_logic_vector(N - 1 downto 0)  := (others => 'L'); -- !

	signal control_we: std_ulogic := '0';
	signal din_we: std_ulogic     := '0';


	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.io_pins
		generic map (g => g, N => N)
		port map (
			clk         =>  clk,
			rst         =>  rst,
			control     =>  control,
			control_we  =>  control_we,
			din         =>  din,
			din_we      =>  din_we,
			dout        =>  dout,
			pins        =>  pins);

	stimulus_process: process
	begin
		wait for clock_period * 2;
		control    <= x"0f"; -- write lower pins
		control_we <= '1';

		wait for clock_period;
		din        <= x"AA";
		din_we     <= '1';

		wait for clock_period * 2;
		pins <= (others => 'H'); -- !
		wait for clock_period * 2;
		stop <= '1';
		wait;
	end process;

end architecture;

------------------------- I/O Pin Controller ----------------------------------------

------------------------- Single and Dual Port Block RAM ----------------------------
--|
--| @warning The function initialize_ram has to be present in each architecture
--| block ram that uses it (as far as I am aware) which means they could fall
--| out of sync. This could be remedied with VHDL-2008.
---------------------------------------------------------------------------------

--- Dual Port Model ---

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.util.all;

entity dual_port_block_ram is

	-- The dual port Block RAM module can be initialized from a file,
	-- or initialized to all zeros. The model can be synthesized (with
	-- Xilinx's ISE) into BRAM.
	--
	-- Valid file_type options include:
	--
	-- FILE_BINARY - A binary file (ASCII '0' and '1', one number per line)
	-- FILE_HEX    - A Hex file (ASCII '0-9' 'a-f', 'A-F', one number per line)
	-- FILE_NONE   - RAM contents will be defaulted to all zeros, no file will
	--               be read from
	--
	-- The data length must be divisible by 4 if the "hex" option is
	-- given.
	--
	-- These default values for addr_length and data_length have been
	-- chosen so as to fill the block RAM available on a Spartan 6.
	--
	generic (g: common_generics;
		addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port (
		--| Port A of dual port RAM
		a_clk:  in  std_ulogic;
		a_dwe:  in  std_ulogic;
		a_dre:  in  std_ulogic;
		a_addr: in  std_ulogic_vector(addr_length - 1 downto 0);
		a_din:  in  std_ulogic_vector(data_length - 1 downto 0);
		a_dout: out std_ulogic_vector(data_length - 1 downto 0) := (others => '0');
		--| Port B of dual port RAM
		b_clk:  in  std_ulogic;
		b_dwe:  in  std_ulogic;
		b_dre:  in  std_ulogic;
		b_addr: in  std_ulogic_vector(addr_length - 1 downto 0);
		b_din:  in  std_ulogic_vector(data_length - 1 downto 0);
		b_dout: out std_ulogic_vector(data_length - 1 downto 0) := (others => '0'));
end entity;

architecture behav of dual_port_block_ram is
	constant ram_size: positive := 2 ** addr_length;

	type ram_type is array ((ram_size - 1 ) downto 0) of std_ulogic_vector(data_length - 1 downto 0);

	impure function initialize_ram(the_file_name: in string; the_file_type: in file_format) return ram_type is
		variable ram_data:   ram_type;
		file     in_file:    text is in the_file_name;
		variable input_line: line;
		variable tmp:        bit_vector(data_length - 1 downto 0);
		variable c:          character;
		variable slv:        std_ulogic_vector(data_length - 1 downto 0);
	begin
		for i in 0 to ram_size - 1 loop
			if the_file_type = FILE_NONE then
				ram_data(i):=(others => '0');
			elsif not endfile(in_file) then
				readline(in_file,input_line);
				if the_file_type = FILE_BINARY then
					read(input_line, tmp);
					ram_data(i) := std_ulogic_vector(to_stdlogicvector(tmp));
				elsif the_file_type = FILE_HEX then
					assert (data_length mod 4) = 0 report "(data_length%4)!=0" severity failure;
					for j in 1 to (data_length/4) loop
						c:= input_line((data_length/4) - j + 1);
						slv((j*4)-1 downto (j*4)-4) := hex_char_to_std_ulogic_vector_tb(c);
					end loop;
					ram_data(i) := slv;
				else
					report "Incorrect file type given: " & file_format'image(the_file_type) severity failure;
				end if;
			else
				ram_data(i) := (others => '0');
			end if;
		end loop;
		file_close(in_file);
		return ram_data;
	end function;

	shared variable ram: ram_type := initialize_ram(file_name, file_type);

begin
	a_ram: process(a_clk)
	begin
		if rising_edge(a_clk) then
			if a_dwe = '1' then
				ram(to_integer(unsigned(a_addr))) := a_din;
			end if;
			if a_dre = '1' then
				a_dout <= ram(to_integer(unsigned(a_addr))) after g.delay;
			else
				a_dout <= (others => '0') after g.delay;
			end if;
		end if;
	end process;

	b_ram: process(b_clk)
	begin
		if rising_edge(b_clk) then
			if b_dwe = '1' then
				ram(to_integer(unsigned(b_addr))) := b_din;
			end if;
			if b_dre = '1' then
				b_dout <= ram(to_integer(unsigned(b_addr))) after g.delay;
			else
				b_dout <= (others => '0') after g.delay;
			end if;
		end if;
	end process;
end architecture;

--- Single Port Model ---

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.util.all;

entity single_port_block_ram is
	generic (g: common_generics;
		addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port (
		clk:  in  std_ulogic;
		dwe:  in  std_ulogic;
		dre:  in  std_ulogic;
		addr: in  std_ulogic_vector(addr_length - 1 downto 0);
		din:  in  std_ulogic_vector(data_length - 1 downto 0);
		dout: out std_ulogic_vector(data_length - 1 downto 0) := (others => '0'));
end entity;

architecture behav of single_port_block_ram is
	constant ram_size: positive := 2 ** addr_length;

	type ram_type is array ((ram_size - 1 ) downto 0) of std_ulogic_vector(data_length - 1 downto 0);

	impure function initialize_ram(the_file_name: in string; the_file_type: in file_format) return ram_type is
		variable ram_data:   ram_type;
		file     in_file:    text is in the_file_name;
		variable input_line: line;
		variable tmp:        bit_vector(data_length - 1 downto 0);
		variable c:          character;
		variable slv:        std_ulogic_vector(data_length - 1 downto 0);
	begin
		for i in 0 to ram_size - 1 loop
			if the_file_type = FILE_NONE then
				ram_data(i):=(others => '0');
			elsif not endfile(in_file) then
				readline(in_file,input_line);
				if the_file_type = FILE_BINARY then
					read(input_line, tmp);
					ram_data(i) := std_ulogic_vector(to_stdlogicvector(tmp));
				elsif the_file_type = FILE_HEX then -- hexadecimal
					assert (data_length mod 4) = 0 report "(data_length%4)!=0" severity failure;
					for j in 1 to (data_length/4) loop
						c:= input_line((data_length/4) - j + 1);
						slv((j*4)-1 downto (j*4)-4) := hex_char_to_std_ulogic_vector_tb(c);
					end loop;
					ram_data(i) := slv;
				else
					report "Incorrect file type given: " & file_format'image(the_file_type) severity failure;
				end if;
			else
				ram_data(i) := (others => '0');
			end if;
		end loop;
		file_close(in_file);
		return ram_data;
	end function;

	shared variable ram: ram_type := initialize_ram(file_name, file_type);

begin
	block_ram: process(clk)
	begin
		if rising_edge(clk) then
			if dwe = '1' then
				ram(to_integer(unsigned(addr))) := din;
			end if;

			if dre = '1' then
				dout <= ram(to_integer(unsigned(addr))) after g.delay;
			else
				dout <= (others => '0') after g.delay;
			end if;
		end if;
	end process;
end architecture;

------------------------- Single and Dual Port Block RAM ----------------------------

------------------------- Data Source -----------------------------------------------
--|
--| This module spits out a bunch of data
--|
--| @todo Create a single module that can be used to capture and replay data at
--| a configurable rate. This could be used as a logger or as a waveform
--| generator. Depending on the generics used this should synthesize to either
--| logger, or a data source, or both. A pre-divider could also be supplied as
--| generic options, to lower the clock rate.
--|

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.single_port_block_ram;
use work.util.counter;
use work.util.all;

entity data_source is
	generic (g: common_generics;
		addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port (
		clk:     in  std_ulogic;
		rst:     in  std_ulogic;

		ce:      in  std_ulogic := '1';
		cr:      in  std_ulogic;

		load:    in  std_ulogic_vector(addr_length - 1 downto 0) := (others => '0');
		load_we: in  std_ulogic := '0';

		dout:    out std_ulogic_vector(data_length - 1 downto 0));
end entity;

architecture structural of data_source is
	signal addr: std_ulogic_vector(addr_length - 1 downto 0);
begin
	count: work.util.counter
		generic map (g => g, N => addr_length)
		port map (
			clk      =>  clk,
			rst      =>  rst,
			ce       =>  ce,
			cr       =>  cr,
			dout     =>  addr,
			load_i   =>  load,
			load_we  =>  load_we);

	ram: work.util.single_port_block_ram
		generic map (
			g           => g,
			addr_length => addr_length,
			data_length => data_length,
			file_name   => file_name,
			file_type   => file_type)
		port map (
			clk  => clk,
			addr => addr,
			dwe  => '0',
			dre  => '1',
			din  => (others => '0'),
			dout => dout);

end architecture;

------------------------- Data Source -----------------------------------------------

------------------------- uCPU ------------------------------------------------------
-- @brief An incredible simple microcontroller
-- @license MIT
-- @author Richard James Howe
-- @copyright Richard James Howe (2017)
--
-- Based on:
-- https://stackoverflow.com/questions/20955863/vhdl-microprocessor-microcontroller
--
--  INSTRUCTION    CYCLES 87 6543210 OPERATION
--  ADD WITH CARRY   2    00 ADDRESS A = A   + MEM[ADDRESS]
--  NOR              2    01 ADDRESS A = A NOR MEM[ADDRESS]
--  STORE            1    10 ADDRESS MEM[ADDRESS] = A
--  JCC              1    11 ADDRESS IF(CARRY) { PC = ADDRESS, CLEAR CARRY }
--
-- It would be interesting to make a customizable CPU in which the
-- instructions could be customized based upon what. Another interesting
-- possibility is making a simple assembler purely in VHDL, which should
-- be possible, but difficult. A single port version would require another
-- state to fetch the operand and another register, or more states.
--
-- @todo Test in hardware, document, make assembler, and a project that
-- just contains an instantiation of this core, Select CPU behavior with
-- generics (instructions, branch conditions...)
--

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ucpu is
	generic (
		asynchronous_reset:  boolean := true;  -- use asynchronous reset if true, synchronous if false
		delay:               time    := 0 ns; -- simulation only

		width:               positive range 8 to 32 := 8);
	port (
		clk, rst: in  std_ulogic;

		pc:       out std_ulogic_vector(width - 3 downto 0);
		op:       in  std_ulogic_vector(width - 1 downto 0);

		adr:      out std_ulogic_vector(width - 3 downto 0);
		di:       in  std_ulogic_vector(width - 1 downto 0);
		re, we:   out std_ulogic;
		do:       out std_ulogic_vector(width - 1 downto 0));
end entity;

architecture rtl of ucpu is
	signal a_c,   a_n:       unsigned(di'high + 1 downto di'low) := (others => '0'); -- accumulator
	signal pc_c,  pc_n:      unsigned(pc'range)                  := (others => '0');
	signal alu:              std_ulogic_vector(1 downto 0)       := (others => '0');
	signal state_c, state_n: std_ulogic                          := '0'; -- FETCH/Single cycle instruction or EXECUTE
begin
	pc          <= std_ulogic_vector(pc_n) after delay;
	do          <= std_ulogic_vector(a_c(do'range)) after delay;
	alu         <= op(op'high downto op'high - 1) after delay;
	adr         <= op(adr'range) after delay;
	we          <= '1' when alu = "10" else '0' after delay; -- STORE
	re          <= alu(1) nor state_c after delay;           -- FETCH for ADD and NOR
	state_n     <= alu(1) nor state_c after delay;           -- FETCH not taken or FETCH done
	pc_n        <= unsigned(op(pc_n'range)) when (alu = "11"    and a_c(a_c'high) = '0') else -- Jump when carry set
			pc_c                    when (state_c = '0' and alu(1) = '0')        else -- FETCH
			(pc_c + 1) after delay; -- EXECUTE

	process(clk, rst)
	begin
		if rst = '1' and asynchronous_reset then
			a_c     <= (others => '0') after delay;
			pc_c    <= (others => '0') after delay;
			state_c <= '0' after delay;
		elsif rising_edge(clk) then
			if rst = '1' and not asynchronous_reset then
				a_c     <= (others => '0') after delay;
				pc_c    <= (others => '0') after delay;
				state_c <= '0' after delay;
			else
				a_c     <= a_n after delay;
				pc_c    <= pc_n after delay;
				state_c <= state_n after delay;
			end if;
		end if;
	end process;

	process(op, alu, di, a_c, state_c)
	begin
		a_n     <= a_c after delay;

		if alu = "11" and a_c(a_c'high) = '0' then a_n(a_n'high) <= '0' after delay; end if;

		if state_c = '1' then -- EXECUTE for ADD and NOR
			assert alu(1) = '0' severity failure;
			if alu(0) = '0' then a_n <= '0' & a_c(di'range) + unsigned('0' & di) after delay; end if;
			if alu(0) = '1' then a_n <= a_c nor '0' & unsigned(di) after delay; end if;
		end if;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.util.all;

entity ucpu_tb is
	generic (g: common_generics;
		file_name: string := "ucpu.bin");
end entity;

architecture testing of ucpu_tb is
	constant clock_period:      time     := 1000 ms / g.clock_frequency;

	constant data_length: positive := 8;
	constant addr_length: positive := data_length - 2;

	signal a_addr: std_ulogic_vector(addr_length - 1 downto 0);
	signal a_dout: std_ulogic_vector(data_length - 1 downto 0) := (others => '0');

	signal b_dwe:  std_ulogic;
	signal b_dre:  std_ulogic;
	signal b_addr: std_ulogic_vector(addr_length - 1 downto 0);
	signal b_din:  std_ulogic_vector(data_length - 1 downto 0);
	signal b_dout: std_ulogic_vector(data_length - 1 downto 0) := (others => '0');

	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	bram_0: entity work.dual_port_block_ram
	generic map (g => g,
		addr_length => addr_length,
		data_length => data_length,
		file_name   => file_name,
		file_type   => FILE_BINARY)
	port map (
		a_clk   =>  clk,
		a_dwe   =>  '0',
		a_dre   =>  '1',
		a_addr  =>  a_addr,
		a_din   =>  (others => '0'),
		a_dout  =>  a_dout,

		b_clk   =>  clk,
		b_dwe   =>  b_dwe,
		b_dre   =>  b_dre,
		b_addr  =>  b_addr,
		b_din   =>  b_din,
		b_dout  =>  b_dout);

	ucpu_0: entity work.ucpu
	generic map (delay => g.delay, width => data_length)
	port map (
		clk => clk,
		rst => rst,
		pc  => a_addr,
		op  => a_dout,

		re  => b_dre,
		we  => b_dwe,
		di  => b_dout,
		do  => b_din,
		adr => b_addr);

	stimulus_process: process
	begin
		wait for clock_period * 1000;
		stop <= '1';
		wait;
	end process;

end architecture;

------------------------- uCPU ------------------------------------------------------

------------------------- Restoring Division ----------------------------------------
-- @todo Add remainder to output, rename signals, make a
-- better test bench, add non-restoring division, and describe module
--
-- Computes a/b in N cycles
--
-- https://en.wikipedia.org/wiki/Division_algorithm#Restoring_division
--
--
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity restoring_divider is
	generic (g: common_generics; N: positive);
	port (
		clk:   in  std_ulogic;
		rst:   in  std_ulogic := '0';

		a:     in  std_ulogic_vector(N - 1 downto 0);
		b:     in  std_ulogic_vector(N - 1 downto 0);
		start: in  std_ulogic;
		done:  out std_ulogic;
		c:     out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture rtl of restoring_divider is
	signal a_c, a_n: unsigned(a'range) := (others => '0');
	signal b_c, b_n: unsigned(b'range) := (others => '0');
	signal m_c, m_n: unsigned(b'range) := (others => '0');
	signal o_c, o_n: unsigned(c'range) := (others => '0');
	signal e_c, e_n: std_ulogic         := '0';
	signal count_c, count_n: unsigned(work.util.n_bits(N) downto 0) := (others => '0');
begin
	c <= std_ulogic_vector(o_n);

	process(clk, rst)
		procedure reset is
		begin
			a_c      <=  (others  =>  '0') after g.delay;
			b_c      <=  (others  =>  '0') after g.delay;
			m_c      <=  (others  =>  '0') after g.delay;
			o_c      <=  (others  =>  '0') after g.delay;
			e_c      <=  '0' after g.delay;
			count_c  <=  (others  =>  '0') after g.delay;
		end procedure;
	begin
		if rst = '1' and g.asynchronous_reset then
			reset;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				reset;
			else
				a_c      <=  a_n after g.delay;
				b_c      <=  b_n after g.delay;
				m_c      <=  m_n after g.delay;
				o_c      <=  o_n after g.delay;
				e_c      <=  e_n after g.delay;
				count_c  <=  count_n after g.delay;
			end if;
		end if;
	end process;

	divide: process(a, b, start, a_c, b_c, m_c, e_c, o_c, count_c)
		variable m_v: unsigned(b'range) := (others => '0');
	begin
		done     <=  '0';
		a_n      <=  a_c;
		b_n      <=  b_c;
		m_v      :=  m_c;
		e_n      <=  e_c;
		o_n      <=  o_c;
		count_n  <=  count_c;
		if start = '1' then
			a_n   <= unsigned(a) after g.delay;
			b_n   <= unsigned(b) after g.delay;
			m_v   := (others => '0');
			e_n   <= '1' after g.delay;
			o_n   <= (others => '0') after g.delay;
			count_n <= (others => '0') after g.delay;
		elsif e_c = '1' then
			if count_c(count_c'high) = '1' then
				done  <= '1' after g.delay;
				e_n   <= '0' after g.delay;
				o_n   <= a_c after g.delay;
				count_n <= (others => '0') after g.delay;
			else
				m_v(b'high downto 1) := m_v(b'high - 1 downto 0);
				m_v(0) := a_c(a'high);
				a_n(a'high downto 1) <= a_c(a'high - 1 downto 0) after g.delay;
				m_v := m_v - b_c;
				if m_v(m_v'high) = '1' then
					m_v := m_v + b_c;
					a_n(0) <= '0' after g.delay;
				else
					a_n(0) <= '1' after g.delay;
				end if;
				count_n <= count_c + 1 after g.delay;
			end if;
		else
			count_n <= (others => '0') after g.delay;
		end if;
		m_n <= m_v after g.delay;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.util.common_generics;

entity restoring_divider_tb is
	generic (g: common_generics);
end entity;

architecture testing of restoring_divider_tb is
	constant clock_period: time     := 1000 ms / g.clock_frequency;
	constant N:          positive := 8;

	signal a: std_ulogic_vector(N - 1 downto 0) := (others => '0');
	signal b: std_ulogic_vector(N - 1 downto 0) := (others => '0');
	signal c: std_ulogic_vector(N - 1 downto 0) := (others => '0');
	signal start, done: std_ulogic := '0';

	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.restoring_divider
		generic map (g => g, N => N)
		port map (
			clk   => clk,
			rst   => rst,
			a     => a,
			b     => b,
			start => start,
			done  => done,
			c     => c);

	stimulus_process: process
	begin
		wait for clock_period * 2;

		a <= x"64";
		b <= x"0A";
		start <= '1';
		wait for clock_period * 1;
		start <= '0';
		wait until done = '1';
		--assert c = x"0A" severity failure;

		wait for clock_period * 10;
		b     <= x"05";
		start <= '1';
		wait for clock_period * 1;
		start <= '0';
		wait until done = '1';
		--assert c = x"14" severity failure;

		stop <= '1';
		wait;
	end process;

end architecture;
------------------------- Restoring Divider ---------------------------------------------------

------------------------- Debouncer -----------------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity debounce_us is
	generic (g: common_generics; timer_period_us: natural);
	port (
		clk:   in  std_ulogic;
		di:    in  std_ulogic;
		do:    out std_ulogic := '0');
end entity;

architecture rtl of debounce_us is
	signal ff: std_ulogic_vector(1 downto 0) := (others => '0');
	signal rst, done: std_ulogic             := '0';
begin
	timer: work.util.timer_us
		generic map (g => g, timer_period_us => timer_period_us)
		port map (
			clk => clk,
			rst => rst,
			co  => done);

	process(clk)
	begin
		if rising_edge(clk) then
			ff(0) <= di    after g.delay;
			ff(1) <= ff(0) after g.delay;
			if (ff(0) xor ff(1)) = '1' then
				rst <= '1' after g.delay;
			elsif done = '1' then
				do  <= ff(1) after g.delay;
			else
				rst   <= '0';
			end if;
		end if;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity debounce_us_tb is
	generic (g: common_generics);
end entity;

architecture testing of debounce_us_tb is
	constant clock_period: time := 1000 ms / g.clock_frequency;

	signal di,  do:  std_ulogic := '0';
	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: work.util.debounce_us
		generic map (g => g, timer_period_us => 1)
		port map (clk => clk, di => di, do => do);

	stimulus_process: process
	begin
		wait for clock_period * 2;
		di <= '1';

		wait for 1.5 us;

		stop <= '1';
		wait;
	end process;
end architecture;
------------------------- Debouncer -----------------------------------------------------------

------------------------- Debouncer Block -----------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity debounce_block_us is
	generic (g: common_generics; N: positive; timer_period_us: natural);
	port (
		clk:   in  std_ulogic;
		di:    in  std_ulogic_vector(N - 1 downto 0);
		do:    out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture structural of debounce_block_us is
begin
	debouncer: for i in (N - 1) downto 0 generate
		d_instance: work.util.debounce_us
			generic map (g => g, timer_period_us => timer_period_us)
			port map (clk => clk, di => di(i), do => do(i));
	end generate;
end architecture;

------------------------- Debouncer Block -----------------------------------------------------

------------------------- State Changed -------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity state_changed is
	generic (g: common_generics);
	port (
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		di:  in  std_ulogic;
		do:  out std_ulogic);
end entity;

architecture rtl of state_changed is
	signal state_c, state_n: std_ulogic_vector(1 downto 0) := (others => '0');
begin
	process(clk, rst)
	begin
		if rst = '1' and g.asynchronous_reset then
			state_c <= (others => '0') after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				state_c <= (others => '0') after g.delay;
			else
				state_c <= state_n after g.delay;
			end if;
		end if;
	end process;

	do <= '1' when (state_c(0) xor state_c(1)) = '1' else '0';

	process(di, state_c)
	begin
		state_n(0) <= state_c(1) after g.delay;
		state_n(1) <= di after g.delay;
	end process;
end architecture;

------------------------- Change State --------------------------------------------------------

------------------------- Change State Block --------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.common_generics;

entity state_block_changed is
	generic (g: common_generics; N: positive);
	port (
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		di:  in  std_ulogic_vector(N - 1 downto 0);
		do:  out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture structural of state_block_changed is
begin
	changes: for i in (N - 1) downto 0 generate
		d_instance: work.util.state_changed
			generic map (g => g)
			port map (clk => clk, rst => rst, di => di(i), do => do(i));
	end generate;
end architecture;

------------------------- Change State Block --------------------------------------------------

------------------------- Reset Signal Generator ----------------------------------------------
-- @todo Allow retriggering of the reset, perhaps even add watchdog functionality to it.
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity reset_generator is
	generic (g: common_generics; reset_period_us:  natural := 0);
	port (
		clk: in  std_logic := 'X';
		rst: out std_logic := '0'); -- reset out!
end entity;

architecture behavior of reset_generator is
	constant cycles:  natural := (g.clock_frequency / 1000000) * reset_period_us;
	subtype  counter is unsigned(max(1, n_bits(cycles) - 1) downto 0);
	signal   c_c, c_n: counter := (others => '0');
begin
	process (clk)
	begin
		if rising_edge(clk) then
			c_c <= c_n after g.delay;
		end if;
	end process;

	process (c_c)
	begin
		if c_c = (cycles - 1) then
			c_n <= c_c after g.delay;
			rst <= '0' after g.delay;
		else
			c_n <= c_c + 1 after g.delay;
			rst <= '1' after g.delay;
		end if;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.common_generics;

entity reset_generator_tb is
	generic (g: common_generics);
end entity;

architecture testing of reset_generator_tb is
	constant clock_period:      time     := 1000 ms / g.clock_frequency;
	signal stop, clk, rst: std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => open);

	uut: entity work.reset_generator
		generic map (g => g, reset_period_us => 1)
		port map (clk => clk, rst => rst);

	stimulus_process: process
	begin
		wait for clock_period;
		assert rst = '1' severity failure;
		wait for 1 us;
		assert rst = '0' severity failure;
		stop <= '1';
		wait;
	end process;

end architecture;

------------------------- Reset Signal Generator ----------------------------------------------
------------------------- Bit Count -----------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.n_bits;
use work.util.common_generics;

entity bit_count is
	generic (g: common_generics; N: positive);
	port (
		bits:   in std_ulogic_vector(N - 1 downto 0);
		count: out std_ulogic_vector(n_bits(N) downto 0));
end entity;

architecture behavior of bit_count is
begin
	process (bits)
		constant zero: unsigned(count'high - 1 downto count'low)  := (others => '0');
		variable t: unsigned(count'range) := (others => '0');
	begin
		t := (others => '0');
		for i in bits'low to bits'high loop
			t := t + (zero & bits(i));
		end loop;
		count <= std_ulogic_vector(t) after g.delay;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.n_bits;
use work.util.common_generics;

entity bit_count_tb is
	generic (g: common_generics);
end entity;

architecture testing of bit_count_tb is
	constant clock_period: time     := 1000 ms / g.clock_frequency;
	constant N:            positive := 3;
	signal bits:  std_ulogic_vector(N - 1 downto 0)     := (others => '0');
	signal count: std_ulogic_vector(n_bits(N) downto 0) := (others => '0');
begin
	uut: entity work.bit_count
		generic map (g => g, N => N)
		port map (bits => bits, count => count);

	stimulus_process: process
		procedure test(b: std_ulogic_vector; c: std_ulogic_vector)  is
		begin
			bits <= b;
			wait for clock_period;
			assert count = c severity failure;
		end procedure;
	begin
		test("000", "000");
		test("001", "001");
		test("010", "001");
		test("011", "010");
		test("100", "001");
		test("101", "010");
		test("110", "010");
		test("111", "011");
		wait;
	end process;

end architecture;
------------------------- Bit Count -----------------------------------------------------------
------------------------- Majority Voter ------------------------------------------------------
-- NB. This could be constructed from a more generic 'assert output if bit
-- count greater than N' module. 
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity majority is
	generic (g: common_generics; N: positive; even_wins: boolean := false);
	port (
		bits: in std_ulogic_vector(N - 1 downto 0);
		vote: out std_ulogic;
		tie:  out std_ulogic);
end entity;

architecture behavior of majority is
	signal count: std_ulogic_vector(n_bits(N) downto 0) := (others => '0');
	-- It might be worth handling up to five or so bits in combinatorial
	-- logic, or it might not. 
begin
	majority_1: if N = 1 generate
		vote <= bits(0) after g.delay;
		tie  <= '0' after g.delay;
	end generate;

	majority_2: if N = 2 generate
		ew_2:  if     even_wins generate vote <= bits(0)  or bits(1) after g.delay; end generate;
		enw_2: if not even_wins generate vote <= bits(0) and bits(1) after g.delay; end generate;
		tie  <= bits(0) or bits(1);
	end generate;

	majority_3: if N = 3 generate
		vote <= (bits(0) and bits(1)) or (bits(1) and bits(2)) or (bits(0) and bits(2)) after g.delay;
		tie  <= '0' after g.delay;
	end generate;

	majority_n: if N > 3 generate
		bit_counter: entity work.bit_count
			generic map (g => g, N => N)
			port map (bits => bits, count => count);

		tie <= '1' when (unsigned(count) = N/2) and (N mod 2) = 0 else '0' after g.delay;

		process (count)
		begin
			if even_wins and (N mod 2) = 0 then
				if unsigned(count) >= (N/2) then
					vote <= '1' after g.delay;
				else
					vote <= '0' after g.delay;
				end if;
			else
				if unsigned(count) > (N/2) then
					vote <= '1' after g.delay;
				else
					vote <= '0' after g.delay;
				end if;
			end if;
		end process;
	end generate;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity majority_tb is
	generic (g: common_generics);
end entity;

architecture testing of majority_tb is
	constant clock_period: time := 1000 ms / g.clock_frequency;

	constant N_3:    positive := 3;
	constant N_4_t:  positive := 4;
	constant N_4_m:  positive := 4;

	signal bits_3:   std_ulogic_vector(N_3   - 1 downto 0) := (others => '0');
	signal bits_4_t: std_ulogic_vector(N_4_t - 1 downto 0) := (others => '0');
	signal bits_4_m: std_ulogic_vector(N_4_m - 1 downto 0) := (others => '0');

	signal vote_3,     tie_3: std_ulogic := '0';
	signal vote_4_t, tie_4_t: std_ulogic := '0';
	signal vote_4_m, tie_4_m: std_ulogic := '0';
begin
	uut_3: entity work.majority
		generic map (g => g, N => N_3)
		port map (bits => bits_3, vote => vote_3, tie => tie_3);

	uut_4_t: entity work.majority
		generic map (g => g, N => N_4_t, even_wins => true)
		port map (bits => bits_4_t, vote => vote_4_t, tie => tie_4_t);

	uut_4_m: entity work.majority
		generic map (g => g, N => N_4_m)
		port map (bits => bits_4_m, vote => vote_4_m, tie => tie_4_m);

	stimulus_process: process
		procedure test_3(b: std_ulogic_vector; vote, tie: std_ulogic)  is
		begin
			bits_3 <= b;
			wait for clock_period;
			assert vote_3 = vote and tie_3 = tie severity failure;
		end procedure;

		procedure test_4_t(b: std_ulogic_vector; vote, tie: std_ulogic)  is
		begin
			bits_4_t <= b;
			wait for clock_period;
			assert vote_4_t = vote and tie_4_t = tie severity failure;
		end procedure;

		procedure test_4_m(b: std_ulogic_vector; vote, tie: std_ulogic)  is
		begin
			bits_4_m <= b;
			wait for clock_period;
			assert vote_4_m = vote and tie_4_m = tie severity failure;
		end procedure;
	begin
		test_3("000", '0', '0');
		test_3("001", '0', '0');
		test_3("010", '0', '0');
		test_3("011", '1', '0');
		test_3("100", '0', '0');
		test_3("101", '1', '0');
		test_3("110", '1', '0');
		test_3("111", '1', '0');

		test_4_t("0000", '0', '0');
		test_4_t("0001", '0', '0');
		test_4_t("0010", '0', '0');
		test_4_t("0011", '1', '1');
		test_4_t("0100", '0', '0');
		test_4_t("0101", '1', '1');
		test_4_t("0110", '1', '1');
		test_4_t("0111", '1', '0');
		test_4_t("1000", '0', '0');
		test_4_t("1001", '1', '1');
		test_4_t("1010", '1', '1');
		test_4_t("1011", '1', '0');
		test_4_t("1100", '1', '1');
		test_4_t("1101", '1', '0');
		test_4_t("1110", '1', '0');
		test_4_t("1111", '1', '0');

		test_4_m("0000", '0', '0');
		test_4_m("0001", '0', '0');
		test_4_m("0010", '0', '0');
		test_4_m("0011", '0', '1');
		test_4_m("0100", '0', '0');
		test_4_m("0101", '0', '1');
		test_4_m("0110", '0', '1');
		test_4_m("0111", '1', '0');
		test_4_m("1000", '0', '0');
		test_4_m("1001", '0', '1');
		test_4_m("1010", '0', '1');
		test_4_m("1011", '1', '0');
		test_4_m("1100", '0', '1');
		test_4_m("1101", '1', '0');
		test_4_m("1110", '1', '0');
		test_4_m("1111", '1', '0');

		wait;
	end process;
end architecture;

------------------------- Majority Voter ------------------------------------------------------

------------------------- Delay Line ----------------------------------------------------------
-- 'DEPTH' * clock period delay line. Minimum delay of 0.
--
-- NB. It would be possible to create a delay line that would allow you to delay samples by 
-- varying amounts with a FIFO and a counter, which is sort of line a Run
-- Length Compression Decoder. A sample and a counter would be pushed to the
-- FIFO, the delay line mechanism would pull a sample/counter and hold the value
-- for that amount of time.
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity delay_line is
	generic (g: common_generics; width: positive; depth: natural);
	port (
		clk:  in std_ulogic;
		rst:  in std_ulogic;
		ce:   in std_ulogic := '1';
		di:   in std_ulogic_vector(width - 1 downto 0);
		do:  out std_ulogic_vector(width - 1 downto 0));
end entity;

architecture behavior of delay_line is
	type delay_line_t is array(integer range 0 to depth) of std_ulogic_vector(di'range);
	signal sigs: delay_line_t := (others => (others => '0'));
begin
	sigs(0) <= di;
	delay_line_generate: for i in 0 to depth generate
		rest: if i > 0 generate
			ux: work.util.reg
				generic map (g => g, N => width)
				port map (clk => clk, rst => rst, we => ce, di => sigs(i - 1), do => sigs(i));
		end generate;
	end generate;
	do <= sigs(depth);
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity delay_line_tb is
	generic (g: common_generics);
end entity;

architecture testing of delay_line_tb is
	constant clock_period:  time     := 1000 ms / g.clock_frequency;
	constant depth:       natural    := 2;
	constant width:       positive   := 8;
	signal clk, rst:      std_ulogic := '0';
	signal stop:          std_ulogic := '0';

	signal di, do: std_ulogic_vector(width - 1 downto 0) := (others => '0');
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut: entity work.delay_line
	generic map (g => g, depth => depth, width => width) port map (clk => clk, rst => rst, di => di, do => do, ce => '1');

	stimulus_process: process
	begin
		-- put a bit into the shift register and wait
		-- for it to come out the other size
		wait until rst = '0';
		di <= x"AA";
		wait for clock_period * 1;
		di <= x"55";
		wait for clock_period * 1;
		di <= x"CC";
		wait for clock_period * 1;
		di <= x"DD";
		assert do = x"AA" severity failure;
		wait for clock_period * 1;
		di <= x"00";
		assert do = x"55" severity failure;
		wait for clock_period * 1;
		assert do = x"CC" severity failure;
		wait for clock_period * 1;
		assert do = x"DD" severity failure;
		wait for clock_period * 1;
		assert do = x"00" severity failure;
		stop <= '1';
		wait;
	end process;
end architecture;

------------------------- Delay Line ----------------------------------------------------------
------------------------- Gray CODEC ----------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity gray_encoder is
	generic (g: common_generics; N: positive);
	port (di: in std_ulogic_vector(N - 1 downto 0);
	     do: out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture behavior of gray_encoder is
begin
	gry: for i in N - 1 downto 0 generate
		first: if i = (N - 1) generate
			do(i) <= di(i);
		end generate;

		rest: if i < (N - 1) generate
			do(i) <= di(i + 1) xor di(i);
		end generate;
	end generate;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity gray_decoder is
	generic (g: common_generics; N: positive);
	port (di: in std_ulogic_vector(N - 1 downto 0);
	     do: out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture behavior of gray_decoder is
begin
	gry: for i in N - 1 downto 0 generate
		first: if i = (N - 1) generate
			do(i) <= di(i) after g.delay;
		end generate;

		rest: if i < (N - 1) generate
			do(i) <= parity(di(N - 1 downto i), true) after g.delay;
		end generate;
	end generate;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity gray_tb is
	generic (g: common_generics);
end entity;

architecture testing of gray_tb is
	constant clock_period:  time     := 1000 ms / g.clock_frequency;
	constant n:           positive   := 3;
	signal clk, rst:      std_ulogic := '0';
	signal stop:          std_ulogic := '0';

	signal di, gray, do: std_ulogic_vector(n - 1 downto 0) := (others => '0');
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut_encode: entity work.gray_encoder
	generic map (g => g, n => n) port map (di => di, do => gray);

	uut_decode: entity work.gray_decoder
	generic map (g => g, n => n) port map (di => gray, do => do);

	stimulus_process: process
		procedure test(slv: std_ulogic_vector) is
		begin
			di <= slv;
			wait for clock_period * 1;
			assert di = do severity failure;
			wait for clock_period * 1;
		end procedure;
	begin
		di <= (others => '0');
		wait until rst = '0';
		test("000");
		test("001");
		test("010");
		test("011");
		test("100");
		test("101");
		test("110");
		test("111");
		stop <= '1';
		wait;
	end process;
end architecture;
------------------------- Gray CODEC ----------------------------------------------------------
------------------------- Parity Module -------------------------------------------------------
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity parity_module is
	generic (g: common_generics; N: positive; even: boolean);
	port (di: in std_ulogic_vector(N - 1 downto 0); do: out std_ulogic);
end entity;

architecture behavior of parity_module is
begin
	do <= parity(di, even) after g.delay;
end architecture;
------------------------- Parity Module -------------------------------------------------------
------------------------- Hamming CODEC  ------------------------------------------------------
-- This is a Hamming encoder/decoder, with an extra parity bit. This can be used for error
-- correction and detection across a noisy line.
--
-- See <https://en.wikipedia.org/wiki/Hamming_code> for more information.
--
library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity hamming_7_4_encoder is
	generic (g: common_generics);
	port (
		di:      in std_ulogic_vector(3 downto 0);
		do:     out std_ulogic_vector(6 downto 0);
		parity: out std_ulogic);
end entity;

architecture behavior of hamming_7_4_encoder is
	signal p1, p2, p3: std_ulogic := '0';
begin
	p1 <= di(0) xor di(1) xor di(3) after g.delay;
	p2 <= di(0) xor di(2) xor di(3) after g.delay;
	p3 <= di(1) xor di(2) xor di(3) after g.delay;
	do(0) <= p1    after g.delay;
	do(1) <= p2    after g.delay;
	do(2) <= di(0) after g.delay;
	do(3) <= p3    after g.delay;
	do(4) <= di(1) after g.delay;
	do(5) <= di(2) after g.delay;
	do(6) <= di(3) after g.delay;
	parity <= p1 xor p2 xor p3 xor di(0) xor di(1) xor di(2) xor di(3);
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity hamming_7_4_decoder is
	generic (g: common_generics; secdec: boolean := true);
	port (
		di:      in std_ulogic_vector(6 downto 0);
		parity:  in std_ulogic;
		do:     out std_ulogic_vector(3 downto 0);
		single, double: out std_ulogic);
end entity;

architecture behavior of hamming_7_4_decoder is
	signal s:  std_ulogic_vector(2 downto 0) := (others => '0');
	signal co, ct, dip: std_ulogic_vector(di'high + 1 downto 0)   := (others => '0');
	signal cp: std_ulogic := '0';
	signal unequal: std_ulogic := '0';
begin
	s(2) <= di(3) xor di(4) xor di(5) xor di(6) after g.delay;
	s(1) <= di(1) xor di(2) xor di(5) xor di(6) after g.delay;
	s(0) <= di(0) xor di(2) xor di(4) xor di(6) after g.delay;
	
	do(0) <= co(2) after g.delay;
	do(1) <= co(4) after g.delay;
	do(2) <= co(5) after g.delay;
	do(3) <= co(6) after g.delay;

	cp <= '0' when not secdec else di(0) xor di(1) xor di(2) xor di(3) xor di(4) xor di(5) xor di(6) after g.delay;

	dip(dip'high) <= parity when secdec else '0' after g.delay;
	dip(di'range) <= di after g.delay;

	unequal <= '1' when ct(di'range) /= dip(di'range) else '0' after g.delay;

	process (dip, s)
	begin
		ct <= dip after g.delay;
		ct(to_integer(unsigned(s) - 1)) <= not dip(to_integer(unsigned(s) - 1)) after g.delay;
	end process;

	process (s, dip, parity, ct, cp, unequal)
	begin
		co <= dip;
		single <= '0';
		double <= '0';

		if secdec and parity /= cp then
			if unequal = '1' then
				single <= '1';
				co <= ct;
			end if;
		else
			if unequal = '1' then
				if secdec then
					double <= '1';
				else
					single <= '1';
					co <= ct;
				end if;
			end if;
		end if;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity hamming_7_4_tb is
	generic (g: common_generics);
end entity;

architecture testing of hamming_7_4_tb is
	constant clock_period:  time     := 1000 ms / g.clock_frequency;
	constant n:           positive   := 3;
	signal clk, rst:      std_ulogic := '0';
	signal stop:          std_ulogic := '0';
	signal di, do, do_s: std_ulogic_vector(3 downto 0) := (others => '0');
	signal encoded_tx, encoded_rx, ebit: std_ulogic_vector(6 downto 0) := (others => '0');
	signal parity, single, double: std_ulogic := '0';
	signal parity_s, single_s, double_s: std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut_encode: entity work.hamming_7_4_encoder
	generic map (g => g) port map (di => di, do => encoded_tx, parity => parity);

	lossy_channel: process(encoded_tx)
		variable seed1, seed2: positive;
		variable r: real;
		variable i: integer range 0 to 7;
	begin
		encoded_rx    <= encoded_tx;
		uniform(seed1, seed2, r);
		if r > 0.5 then
			uniform(seed1, seed2, r);
			i := integer(floor(r * 6.99));
			encoded_rx(i) <= not encoded_tx(i);
			uniform(seed1, seed2, r);
		end if;
		if r > 0.5 then
			uniform(seed1, seed2, r);
			i := integer(floor(r * 6.99));
			encoded_rx(i) <= not encoded_tx(i);
		end if;
	end process;

	ebit <= encoded_tx xor encoded_rx;

	uut_decode_secdec: entity work.hamming_7_4_decoder
	generic map (g => g) port map (di => encoded_rx, do => do, parity => parity, single => single, double => double);

	uut_decode_single: entity work.hamming_7_4_decoder
	generic map (g => g, secdec => false) port map (di => encoded_rx, do => do_s, parity => parity_s, single => single_s, double => double_s);

	stimulus_process: process
		procedure test(slv: std_ulogic_vector) is
		begin
			di <= slv;
			wait for clock_period * 2;
			if bit_count_f(ebit) = 2 then
				assert double = '1' severity failure;
				assert single = '0' severity failure;
			elsif bit_count_f(ebit) = 1 then
				assert di = do severity failure;
				assert single = '1' severity failure;
				assert double = '0' severity failure;

				assert di = do_s severity failure;
				assert single_s = '1' severity failure;
			else
				assert di = do severity failure;
				assert double = '0' severity failure;
				assert single = '0' severity failure;

				assert di = do_s severity failure;
				assert single_s = '0' severity failure;
			end if;
			wait for clock_period * 2;
		end procedure;
		variable ii: unsigned(7 downto 0) := (others => '1');
	begin
		di <= (others => '0');
		wait until rst = '0';

		while ii /= x"00" loop
			ii := ii - 1;
			test(std_ulogic_vector(ii(3 downto 0)));
		end loop;
		stop <= '1';
		wait;
	end process;

end architecture;
------------------------- Hamming CODEC  ------------------------------------------------------
------------------------- VGA Controller ------------------------------------------------------
-- VGA Controller
--
-- See: 
--  * <https://en.wikipedia.org/wiki/Video_Graphics_Array>
--  * <http://www.ece.ualberta.ca/~elliott/ee552/studentAppNotes/1998_w/Altera_UP1_Board_Map/vga.html>
--  * <https://www.digikey.com/eewiki/pages/viewpage.action?pageId=15925278>
--
-- This purpose of this VGA controller is to provide the necessary VGA
-- timings for a given resolution (which has to be determined at instantiation
-- time and cannot be configured on the fly). The VGA controller will generate
-- the correct HSYNC and VSYNCH signals needed, as well as the current row and
-- column that is currently being drawn. This can be used to generate an image.
--
-- Example timing for 640 x 480:
--
--  |-----800 pixels / 31.778 us---------|
--  |-----640 Pixels--------|-160 Pixels-|
--  |-----25.422 us---------|--5.75 us---|
--   
--  +-----------------------+------------+   VSYNC
--  |                       |         ^  |     |
--  |                       |         |  |     |
--  |                       |         |  |     |
--  |    Display Period     |   480 Rows |     |
--  |                       |         |  |     |
--  |                       |         |  |     |
--  |                       |         |  |     |
--  |                       |         v  |     |
--  +-----------------------+        --- |     | 
--  |                                 ^  |    _| <- Front porch 0.318 ms (10 rows)
--  |                                 |  |   | 
--  |      Blanking Period       45 Rows |   | <--- VSYNC pulse 0.064 ms (2 rows)
--  |                                 |  |   |_
--  |                                 v  |     | <- Back porch 1.048 ms (33 rows)
--  +------------------------------------+     |
--                                  
--                              ___
--  ___________________________|   |_____ HSYNC
--                            ^  ^  ^
--  0.636 us   Front Porch __/  /  / <-(16 pixels)
--  3.813 us   HSYNC Pulse ____/  /  <-(96 pixels)
--  1.907 us   Back Porch _______/   <-(48 pixels)
--

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.all;

entity vga_controller is
	generic (
		g: common_generics;
		pixel_clock_frequency:  positive := 25_000_000;
		constant cfg: vga_configuration  := vga_640x480); 
	port (
		clk, rst:          in std_ulogic;
		h_sync, v_sync:   out std_ulogic;
		h_blank, v_blank: out std_ulogic;
		column, row:      out integer); -- TODO: Use unsigned value of minimum necessary width
end entity;

architecture behavior of vga_controller is
	constant h_period: integer := cfg.h_pulse + cfg.h_back_porch + cfg.h_pixels + cfg.h_front_porch; -- number of pixel clocks in a row
	constant v_period: integer := cfg.v_pulse + cfg.v_back_porch + cfg.v_pixels + cfg.v_front_porch; -- number of rows in column
	signal h_sync_internal, v_sync_internal: std_ulogic := '0';
begin
	-- The clock does not need to be exactly the correct value
	assert pixel_clock_frequency <= (cfg.clock_frequency + 250_000) 
		and pixel_clock_frequency >= (cfg.clock_frequency - 250_000) severity warning;
	
	h_sync <= h_sync_internal xor cfg.h_polarity;
	v_sync <= v_sync_internal xor cfg.v_polarity;

	process (clk, rst)
		constant h_start: integer := cfg.h_pixels + cfg.h_front_porch;
		constant h_end:   integer := cfg.h_pixels + cfg.h_front_porch + cfg.h_pulse;
		constant v_start: integer := cfg.v_pixels + cfg.v_front_porch;
		constant v_end:   integer := cfg.v_pixels + cfg.v_front_porch + cfg.v_pulse;
		variable h_count: integer range 0 to h_period - 1 := 0;  -- horizontal counter (counts the columns)
		variable v_count: integer range 0 to v_period - 1 := 0;  -- vertical counter (counts the rows)
		procedure reset is
		begin
			h_count         := 0;
			v_count         := 0;
			h_blank         <= '0' after g.delay;
			v_blank         <= '0' after g.delay;
			column          <= 0 after g.delay;
			row             <= 0 after g.delay;
		end procedure;
	begin
		if rst = '1' and g.asynchronous_reset then
			reset;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				reset;
			else
				if h_count < (h_period - 1) then -- pixel count
					h_count := h_count + 1;
				else
					if v_count < (v_period - 1) then -- row count
						v_count := v_count + 1;
					else
						v_count := 0;
					end if;
					h_count := 0;
				end if;

				h_sync_internal <= not logical((h_count < h_start) or (h_count >= h_end)) after g.delay;
				v_sync_internal <= not logical((v_count < v_start) or (v_count >= v_end)) after g.delay;

				column <= cfg.h_pixels - 1;
				row    <= cfg.v_pixels - 1;

				if h_count < cfg.h_pixels then h_blank <= '0'; column <= h_count; else h_blank <= '1'; end if;
				if v_count < cfg.v_pixels then v_blank <= '0'; row    <= v_count; else v_blank <= '1'; end if;
			end if;
		end if;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity vga_tb is
	generic (g: common_generics; pixel_clock_frequency: positive := 25_000_000; simulation_us: time := 20000 us);
end entity;

architecture testing of vga_tb is
	constant pixel_clock_period: time   := 1000 ms / pixel_clock_frequency;
	signal rst, clk:        std_ulogic  := '1';
	signal stop:            boolean     := false;
	signal h_sync, v_sync:  std_ulogic  := 'X';
	signal h_blank, v_blank: std_ulogic := 'X';
	signal column, row:     integer     := 0;
begin
	duration: process begin wait for simulation_us; stop <= true; wait; end process;
	clk_process: process
	begin
		rst <= '1';
		wait for pixel_clock_period * 5;
		rst <= '0';
		while not stop loop
			clk <= '1';
			wait for pixel_clock_period / 2;
			clk <= '0';
			wait for pixel_clock_period / 2;
		end loop;
		wait;
	end process;

	uut: work.util.vga_controller
		generic map (g => g, pixel_clock_frequency => pixel_clock_frequency)
		port map (
			rst     => rst, 
			clk     => clk,
			h_sync  => h_sync, 
			v_sync  => v_sync,
			h_blank => h_blank,
			v_blank => v_blank,
			column  => column, 
			row     => row);
end architecture;
------------------------- VGA Controller ------------------------------------------------------
------------------------- LED Controller ------------------------------------------------------
--| This module implements a 7 segment display (plus decimal point) driver, 
--|  with 4 displays in total:
--|
--|    _____________________ an (selects segment)
--|    |     |     |     |
--|   __    __    __    __
--|  |  |  |  |  |  |  |  |
--|  |__|  |__|  |__|  |__|
--|  |  |  |  |  |  |  |  |
--|  |__|. |__|. |__|. |__|.
--|   |____|_____|_____|____ ka (value to display on segment)
--|
--| Each of the display shares a common anode for all of its LEDs, this can be
--| used to select an individual display

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity led_7_segment_display is
	generic (
		g:                      common_generics;
		use_bcd_not_hex:        boolean  := true;
		refresh_rate_us:        natural  := 1500;
		number_of_led_displays: positive := 4);
	port (
		clk:      in   std_ulogic;
		rst:      in   std_ulogic;

		leds_we:  in   std_ulogic;
		leds:     in   std_ulogic_vector((number_of_led_displays * led_7_segment_character_length) - 1 downto 0);

		-- Physical outputs
		an:       out  std_ulogic_vector(number_of_led_displays - 1 downto 0);  -- anodes, controls on/off
		ka:       out  std_ulogic_vector(7 downto 0)); -- cathodes, data on display
end;

architecture rtl of led_7_segment_display is

	-- This lookup table converts a BCD character into a value
	-- that can be displayed on an 7 segment display. The layout of which
	-- is as follows:
	--
	--       A
	--      ---
	--   F |   | B
	--     |___|
	--   E | G | C
	--     |___| . DP
	--       D
	--
	-- The following encoding is used to convert the input BCD character
	-- into a value that can be put onto the display.
	--
	--  -----------------------------------------
	-- |   | DP| G | F | E | D | C | B | A | Hex |
	-- |BCD| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |Hi Lo|
	--  -----------------------------------------
	-- | 0 |   |   | 1 | 1 | 1 | 1 | 1 | 1 | 3 F |
	-- | 1 |   |   |   |   |   | 1 | 1 |   | 0 6 |
	-- | 2 |   | 1 |   | 1 | 1 |   | 1 | 1 | 5 B |
	-- | 3 |   | 1 |   |   | 1 | 1 | 1 | 1 | 4 F |
	-- | 4 |   | 1 | 1 |   |   | 1 | 1 |   | 6 6 |
	-- | 5 |   | 1 | 1 |   | 1 | 1 |   | 1 | 6 D |
	-- | 6 |   | 1 | 1 | 1 | 1 | 1 |   | 1 | 7 D |
	-- | 7 |   |   |   |   |   | 1 | 1 | 1 | 0 7 |
	-- | 8 |   | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 7 F |
	-- | 9 |   | 1 | 1 |   | 1 | 1 | 1 | 1 | 6 F |
	-- |   |   |   |   |   |   |   |   |   | 0 0 |
	-- | . | 1 |   |   |   |   |   |   |   | 8 0 |
	-- | - |   | 1 |   |   |   |   |   |   | 4 0 |
	--  -----------------------------------------
	-- | A |   | 1 | 1 | 1 |   | 1 | 1 | 1 | 7 7 |
	-- | b |   | 1 | 1 | 1 | 1 | 1 |   |   | 7 C |
	-- | C |   |   | 1 | 1 | 1 |   |   | 1 | 3 9 |
	-- | d |   | 1 |   | 1 | 1 | 1 | 1 |   | 5 E |
	-- | E |   | 1 | 1 | 1 | 1 |   |   | 1 | 7 9 |
	-- | F |   | 1 | 1 | 1 |   |   |   | 1 | 7 1 |
	--  -----------------------------------------
	--
	-- The table is then inverted before it goes to the output.
	--

	function hex_to_7_segment(a: led_7_segment_character) return led_7_segment is
		variable r: std_ulogic_vector(7 downto 0) := x"00";
	begin
		-- NB. You may have thought a case statement would be the best
		-- way of doing this, and you would be right. However, Xilinx ISE
		-- issues annoying 'INFO' statements when you do - thanks Xilinx!
		-- The generated hardware is the same however.
		   if a = "0000" then r := x"3F"; -- 0
		elsif a = "0001" then r := x"06"; -- 1
		elsif a = "0010" then r := x"5B"; -- 2
		elsif a = "0011" then r := x"4F"; -- 3
		elsif a = "0100" then r := x"66"; -- 4
		elsif a = "0101" then r := x"6D"; -- 5
		elsif a = "0110" then r := x"7D"; -- 6
		elsif a = "0111" then r := x"07"; -- 7
		elsif a = "1000" then r := x"7F"; -- 8
		elsif a = "1001" then r := x"6F"; -- 9
		elsif a = "1010" then r := x"77"; -- A
		elsif a = "1011" then r := x"7C"; -- b
		elsif a = "1100" then r := x"39"; -- C
		elsif a = "1101" then r := x"5E"; -- d
		elsif a = "1110" then r := x"79"; -- E
		elsif a = "1111" then r := x"71"; -- F
		end if;
		return r;
	end function;

	function bcd_to_7_segment(a: led_7_segment_character) return led_7_segment is
		variable r: std_ulogic_vector(7 downto 0) := x"00";
	begin
		case a is
			when "0000" => r := x"3F"; -- 0
			when "0001" => r := x"06"; -- 1
			when "0010" => r := x"5B"; -- 2
			when "0011" => r := x"4F"; -- 3
			when "0100" => r := x"66"; -- 4
			when "0101" => r := x"6D"; -- 5
			when "0110" => r := x"7D"; -- 6
			when "0111" => r := x"07"; -- 7
			when "1000" => r := x"7F"; -- 8
			when "1001" => r := x"6F"; -- 9
			when "1010" => r := x"00"; -- Blank
			when "1011" => r := x"80"; -- .
			when "1100" => r := x"40"; -- -
			when "1101" => r := x"00"; -- Unused
			when "1110" => r := x"00"; -- Unused
			when "1111" => r := x"00"; -- Unused
			when others => r := x"00"; -- Unused
		end case;
		return r;
	end function;

	function char_to_7_segment(a: led_7_segment_character) return led_7_segment is
	begin
		if use_bcd_not_hex then
			return invert(bcd_to_7_segment(a));
		else
			return invert(hex_to_7_segment(a));
		end if;
	end function;

	signal leds_o: std_ulogic_vector(leds'range) := (others => '0');

	signal do_shift:  std_ulogic := '0';
	signal shift_reg: std_ulogic_vector(number_of_led_displays - 1 downto 0);

	signal leds_reg_o: std_ulogic_vector(leds'range) := (others => '0');
	signal leds_reg_we_o: std_ulogic := '0';
begin
	an <= invert(shift_reg) after g.delay;

	segment_reg: entity work.reg
		generic map (g => g, N => number_of_led_displays * led_7_segment_character_length)
		port map (
			clk => clk,
			rst => rst,
			we  => leds_we,
			di  => leds,
			do  => leds_reg_o);

	segment_reg_re: entity work.reg
		generic map (g => g, N => 1)
		port map (
			clk   => clk,
			rst   => rst,
			we    => '1',
			di(0) => leds_we,
			do(0) => leds_reg_we_o);

	led_gen: for i in number_of_led_displays - 1 downto 0 generate
		led_i: entity work.reg
			generic map (g => g, N => led_7_segment_character_length)
			port map (
				clk => clk,
				rst => rst,
				we  => leds_reg_we_o,
				di  => leds_reg_o((i*led_7_segment_character_length) + led_7_segment_character_length - 1 downto (i*led_7_segment_character_length)),
				do  => leds_o((i*led_7_segment_character_length) + led_7_segment_character_length - 1 downto (i*led_7_segment_character_length)));
	end generate;

	timer: entity work.timer_us
		generic map (g => g, timer_period_us => refresh_rate_us)
		port map (
			clk             => clk,
			rst             => rst,
			co              => do_shift);

	process(rst, clk, do_shift, shift_reg)
	begin
		if rst = '1' and g.asynchronous_reset then
			shift_reg    <= (others => '0') after g.delay;
			shift_reg(0) <= '1' after g.delay;
		elsif rising_edge(clk) then
			if rst = '1' and not g.asynchronous_reset then
				shift_reg    <= (others => '0') after g.delay;
				shift_reg(0) <= '1' after g.delay;
			else
				if do_shift = '1' then
					shift_reg <= shift_reg(number_of_led_displays - 2 downto 0) & shift_reg(number_of_led_displays - 1) after g.delay;
				else
					shift_reg <= shift_reg after g.delay;
				end if;
			end if;
		end if;
	end process;

	process(leds_o, shift_reg)
	begin
		ka <= (others => '0');
		for i in  number_of_led_displays - 1 downto 0 loop
			if '1' = shift_reg(number_of_led_displays - i - 1) then
				ka <= char_to_7_segment(leds_o(i*led_7_segment_character_length + led_7_segment_character_length - 1 downto (i*led_7_segment_character_length))) after g.delay;
			end if;
		end loop;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity led_7_segment_display_tb is
	generic (g: common_generics);
end entity;

architecture testing of led_7_segment_display_tb is
	constant clock_period:  time     := 1000 ms / g.clock_frequency;
	signal clk, rst:      std_ulogic := '0';
	signal stop:          std_ulogic := '0';

	constant number_of_led_displays: positive := 4;
	signal an: std_ulogic_vector(number_of_led_displays - 1 downto 0);
	signal ka: std_ulogic_vector(7 downto 0);
	signal leds_we: std_ulogic;
	signal leds:    std_ulogic_vector((number_of_led_displays * led_7_segment_character_length) - 1 downto 0);
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	-- We have a very fast refresh rate here, just for testing purposes.
	uut: entity work.led_7_segment_display
		generic map (g => g, refresh_rate_us => 1)
		port map (clk => clk, rst => rst, leds_we => leds_we, leds => leds, an => an, ka => ka);

	stimulus_process: process
	begin
		wait for clock_period * 2;
		leds_we <= '1';
		leds <= x"1234";
		wait for clock_period * 1;
		leds_we <= '0';
		wait for clock_period * 1000;
		stop <= '1';
		wait;
	end process;

end architecture;

------------------------- LED Controller ------------------------------------------------------
------------------------- Sine / Cosine  ------------------------------------------------------
-- Sine / Cosine calculation using multiplication
-- Half-inched from <https://github.com/jamesbowman/sincos>
-- Angles are input as signed Furmans (1 Furman = (1/pow(2, 16) of a circle))
-- 1 Degree is ~182 Furmans. 1 rad is ~10430 Furmans.
-- Result is signed scaled 16-bit integer; -1 = -32767, +1 = 32767
--
-- TODO: Optionally insert registers at various locations throughout the module, 
-- specified with a generic. Also, create a resource shared version.
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity sine is
	generic (g: common_generics; pipeline: boolean := true);
	port (
		clk, rst, xwe: in std_ulogic;
		x:  in  std_ulogic_vector(15 downto 0);
		s:  out std_ulogic_vector(15 downto 0));
end entity;

architecture behavior of sine is
	subtype val is signed(x'range);
	subtype mul is signed((val'high * 2) + 1 downto 0);
	function half_multiply_add(a, b, c: val) return val is
		variable t: mul;
		variable r: val;
	begin
		t := a * b;
		r := t(t'high downto r'high + 1) + c;
		return r;
	end function;
	signal n: signed(2 downto 0);
	signal xn, z, y, sums, sumc, sum1, cc,  t0, t1, t0n, t1n, sa, so: val;
	signal cc32: mul;
	signal xnslv, t0nslv, t1nslv: std_ulogic_vector(x'range);
begin
	pipe_0: if pipeline generate
		reg_in: work.util.reg 
			generic map (g => g, N => x'length) 
			port map (clk => clk, rst => rst, we => xwe, di => x, do => xnslv);
		reg_out: work.util.reg
			generic map (g => g, N => x'length) 
			port map (clk => clk, rst => rst, we => '1', di => std_ulogic_vector(so), do => s);
		xn  <= signed(xnslv);
		t0n <= signed(t0); -- also need to delay n
		t1n <= signed(t1); -- also need to delay n
	end generate;
	no_pipe_0: if not pipeline generate 
		xn <= signed(x); 
		s  <= std_ulogic_vector(so) after g.delay;
		t0n <= t0;
		t1n <= t1;
	end generate;

	y(1 downto 0)  <= (others => '0') after g.delay;
	y(15 downto 2) <= signed(xn(13 downto 0)) after g.delay;
	n    <= signed(xn(15 downto 13)) + "01" after g.delay;
	z    <= half_multiply_add(y, y,        x"0000") after g.delay;
	sumc <= half_multiply_add(z, x"0FBD", -x"4EE9") after g.delay;
	sums <= half_multiply_add(z, x"04F8", -x"2953") after g.delay;
	sum1 <= half_multiply_add(z, sums,     x"6487") after g.delay;
	t0   <= z    when n(1) = '1' else y after g.delay;
	t1   <= sumc when n(1) = '1' else sum1 after g.delay;
	cc32 <= t0n * t1n after g.delay;
	cc   <= cc32(cc32'high - 1 downto cc'high) after g.delay;
	sa   <= cc + x"7FFF" when n(1) = '1' else cc after g.delay;
	so   <= -sa when n(2) = '1' else sa after g.delay;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity cosine is
	generic (g: common_generics; pipeline: boolean := true);
	port (
		clk, rst, xwe: in std_ulogic;
		x:  in  std_ulogic_vector(15 downto 0);
		c:  out std_ulogic_vector(15 downto 0));
end entity;

architecture behavior of cosine is
	signal xn: std_ulogic_vector(c'range);
begin
	xn <= std_ulogic_vector(signed(x) + x"4000");
	calc: entity work.sine 
		generic map(g => g, pipeline => pipeline) port map(clk => clk, rst => rst, xwe => xwe, x => xn, s => c);
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity sine_tb is
	generic (g: common_generics);
end entity;

architecture testing of sine_tb is
	constant clock_period:  time     := 1000 ms / g.clock_frequency;
	signal clk, rst:      std_ulogic := '0';
	signal stop:          std_ulogic := '0';

	constant number_of_led_displays: positive := 4;
	signal x: std_ulogic_vector(15 downto 0);
	signal s, c: std_ulogic_vector(x'range);
begin
	cs: entity work.clock_source_tb
		generic map (g => g, hold_rst => 2)
		port map (stop => stop, clk => clk, rst => rst);

	uut_c: entity work.sine   generic map (g => g) port map (clk => clk, rst => rst, xwe => '1', x => x, s => s);
	uut_s: entity work.cosine generic map (g => g) port map (clk => clk, rst => rst, xwe => '1', x => x, c => c);

	stimulus_process: process
		variable cnt: integer := -32768;
	begin
		x <= std_ulogic_vector(to_signed(cnt, x'length));
		wait for clock_period * 2;
		while cnt < 32768 loop
			x <= std_ulogic_vector(to_signed(cnt, x'length));
			wait for clock_period * 10;
			cnt := cnt + 182;
		end loop;
		stop <= '1';
		wait;
	end process;
end architecture;
------------------------- Sine / Cosine  ------------------------------------------------------

