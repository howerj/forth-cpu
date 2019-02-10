-------------------------------------------------------------------------------
--| @file util.vhd
--| @brief A collection of utilities and simple components. The components
--| should be synthesizable, and the functions can be used within synthesizable
--| components, unless marked with a "_tb" suffix (or is the function n_bits).
--| @author         Richard James Howe
--| @copyright      Copyright 2017 Richard James Howe
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package util is
	component util_tb is
		generic(clock_frequency: positive);
	end component;

	component clock_source_tb is
		generic(clock_frequency: positive; hold_rst: positive := 1);
		port(
			stop:            in     std_ulogic := '0';
			clk:             buffer std_ulogic;
			clk_with_jitter: out    std_ulogic := '0';
			rst:             out    std_ulogic := '0');
	end component;

	component reg
		generic(N: positive);
		port(
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			we:  in  std_ulogic;
			di:  in  std_ulogic_vector(N - 1 downto 0);
			do:  out std_ulogic_vector(N - 1 downto 0));
	end component;

	component shift_register
		generic(N: positive);
		port(
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
		generic(clock_frequency: positive);
	end component;

	component timer_us
		generic(clock_frequency: positive; timer_period_us: natural);
		port(
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			co:  out std_ulogic);
	end component;

	component timer_us_tb
		generic(clock_frequency: positive);
	end component;

	component rising_edge_detector is
	port(
		clk:    in  std_ulogic;
		rst:    in  std_ulogic;
		di:     in  std_ulogic;
       		do:     out std_ulogic);
	end component;

	component rising_edge_detector_tb is
		generic(clock_frequency: positive);
	end component;

	component rising_edge_detectors is
	generic(N: positive);
	port(
		clk:    in  std_ulogic;
		rst:    in  std_ulogic;
		di:     in  std_ulogic_vector(N - 1 downto 0);
       		do:     out std_ulogic_vector(N - 1 downto 0));
	end component;


	-- @note half_adder test bench is folded in to full_adder_tb
	component half_adder is
		port(
			a:     in  std_ulogic;
			b:     in  std_ulogic;
			sum:   out std_ulogic;
			carry: out std_ulogic);
	end component;

	component full_adder is
		port(
			x:     in    std_ulogic;
			y:     in    std_ulogic;
			z:     in    std_ulogic;
			sum:   out   std_ulogic;
			carry: out   std_ulogic);
	end component;

	component full_adder_tb is
		generic(clock_frequency: positive);
	end component;

	component fifo is
		generic (data_width: positive;
			fifo_depth: positive);
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
		generic(clock_frequency: positive);
	end component;

	component counter is
		generic(
			N: positive);
		port(
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
		generic(clock_frequency: positive);
	end component;

	component lfsr is
		generic(constant tap: std_ulogic_vector);
		port
		(
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			ce:  in  std_ulogic := '1';
			we:  in  std_ulogic;
			di:  in  std_ulogic_vector(tap'high + 1 to tap'low);
			do:  out std_ulogic_vector(tap'high + 1 to tap'low));
	end component;

	component lfsr_tb is
		generic(clock_frequency: positive);
	end component;

	component io_pins is
		generic(
			N: positive);
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
	end component;

	component io_pins_tb is
		generic(clock_frequency: positive);
	end component;

	type file_format is (FILE_HEX, FILE_BINARY, FILE_NONE);

	component dual_port_block_ram is
	generic(addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port(
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
	generic(addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port(
		clk:  in  std_ulogic;
		dwe:  in  std_ulogic;
		dre:  in  std_ulogic;
		addr: in  std_ulogic_vector(addr_length - 1 downto 0);
		din:  in  std_ulogic_vector(data_length - 1 downto 0);
		dout: out std_ulogic_vector(data_length - 1 downto 0) := (others => '0'));
	end component;

	component data_source is
		generic(addr_length: positive    := 12;
			data_length: positive    := 16;
			file_name:   string      := "memory.bin";
			file_type:   file_format := FILE_BINARY);
		port(
			clk:     in  std_ulogic;
			rst:     in  std_ulogic;

			ce:      in  std_ulogic := '1';
			cr:      in  std_ulogic;

			load:    in  std_ulogic_vector(addr_length - 1 downto 0) := (others => '0');
			load_we: in  std_ulogic := '0';

			dout:    out std_ulogic_vector(data_length - 1 downto 0));
	end component;

	component ucpu is
		generic(width: positive range 8 to 32 := 8);
		port(
			clk, rst: in  std_ulogic;

			pc:       out std_ulogic_vector(width - 3 downto 0);
			op:       in  std_ulogic_vector(width - 1 downto 0);

			adr:      out std_ulogic_vector(width - 3 downto 0);
			di:       in  std_ulogic_vector(width - 1 downto 0);
			re, we:   out std_ulogic;
			do:       out std_ulogic_vector(width - 1 downto 0));
	end component;

	component ucpu_tb is
		generic(
			clock_frequency: positive;
			file_name: string := "ucpu.bin");
	end component;

	component restoring_divider is
		generic(N: positive);
		port(
			clk:   in  std_ulogic;
			rst:   in  std_ulogic := '0';

			a:     in  std_ulogic_vector(N - 1 downto 0);
			b:     in  std_ulogic_vector(N - 1 downto 0);
			start: in  std_ulogic;
			done:  out std_ulogic;
			c:     out std_ulogic_vector(N - 1 downto 0));
	end component;

	component restoring_divider_tb is
		generic(clock_frequency: positive);
	end component;

	component debounce_us is
		generic(clock_frequency: positive; timer_period_us: natural);
		port(
			clk:   in  std_ulogic;
			di:    in  std_ulogic;
			do:    out std_ulogic);
	end component;

	component debounce_block_us is
		generic(N: positive; clock_frequency: positive; timer_period_us: natural);
		port(
			clk:   in  std_ulogic;
			di:    in  std_ulogic_vector(N - 1 downto 0);
			do:    out std_ulogic_vector(N - 1 downto 0));
	end component;

	component debounce_us_tb is
		generic(clock_frequency: positive);
	end component;

	component state_changed is
		port(
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			di:  in  std_ulogic;
			do:  out std_ulogic);
	end component;

	component state_block_changed is
		generic(N: positive);
		port(
			clk: in  std_ulogic;
			rst: in  std_ulogic;
			di:  in  std_ulogic_vector(N - 1 downto 0);
			do:  out std_ulogic_vector(N - 1 downto 0));
	end component;

	function max(a: natural; b: natural) return natural;
	function min(a: natural; b: natural) return natural;
	function n_bits(x: natural) return natural;
	function n_bits(x: std_ulogic_vector) return natural;
	function reverse (a: in std_ulogic_vector) return std_ulogic_vector;
	function invert(slv:std_ulogic_vector) return std_ulogic_vector;
	function parity(slv:std_ulogic_vector; even: boolean) return std_ulogic;
	function select_bit(indexed, selector: std_ulogic_vector) return std_ulogic;
	function priority(order: std_ulogic_vector; high: boolean) return natural;
	function priority(order: std_ulogic_vector; high: boolean) return std_ulogic_vector;
	function mux(a: std_ulogic_vector; b: std_ulogic_vector; sel: std_ulogic) return std_ulogic_vector;
	function mux(a: std_ulogic; b: std_ulogic; sel: std_ulogic) return std_ulogic;
	function mux(a, b : std_ulogic_vector) return std_ulogic;
	function decode(encoded: std_ulogic_vector) return std_ulogic_vector;
	function hex_char_to_std_ulogic_vector(hc: character) return std_ulogic_vector;
	function to_std_ulogic_vector(s: string) return std_ulogic_vector;

	type ulogic_string is array(integer range <>) of std_ulogic_vector(7 downto 0);
  	function to_std_ulogic_vector(s: string) return ulogic_string;

	--- Not synthesizable ---

	subtype configuration_name is string(1 to 8);

	type configuration_item is record
		name:  configuration_name;
		value: integer;
	end record;

	type configuration_items is array(integer range <>) of configuration_item;

	function search_configuration_tb(find_me: configuration_name; ci: configuration_items) return integer;
	procedure read_configuration_tb(file_name:  string; ci: inout configuration_items);
	procedure write_configuration_tb(file_name: string; ci: configuration_items);

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

	function n_bits(x: natural) return natural is
		variable x1: natural := max(x, 1) - 1;
		variable n:  natural := 1;
	begin
		while x1 > 1 loop
			x1 := x1 / 2;
			n  := n + 1;
		end loop;
		return n;
	end function;

	function n_bits(x: std_ulogic_vector) return natural is
	begin
		return n_bits(x'high);
	end function;

	-- https://stackoverflow.com/questions/13584307
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

	function hex_char_to_std_ulogic_vector(hc: character) return std_ulogic_vector is
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

	--- Not synthesizable ---

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

end;

------------------------- Utility Test Bench ----------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use work.util.all;

entity util_tb is
	generic(clock_frequency: positive);
end entity;

architecture behav of util_tb is
begin
	uut_shiftReg: work.util.shift_register_tb    generic map(clock_frequency => clock_frequency);
	uut_timer_us: work.util.timer_us_tb          generic map(clock_frequency => clock_frequency);
	uut_full_add: work.util.full_adder_tb        generic map(clock_frequency => clock_frequency);
	uut_fifo:     work.util.fifo_tb              generic map(clock_frequency => clock_frequency);
	uut_counter:  work.util.counter_tb           generic map(clock_frequency => clock_frequency);
	uut_lfsr:     work.util.lfsr_tb              generic map(clock_frequency => clock_frequency);
	uut_ucpu:     work.util.ucpu_tb              generic map(clock_frequency => clock_frequency);
	uut_rdivider: work.util.restoring_divider_tb generic map(clock_frequency => clock_frequency);
	uut_debounce: work.util.debounce_us_tb       generic map(clock_frequency => clock_frequency);
	uut_rising_edge_detector: work.util.rising_edge_detector_tb generic map(clock_frequency => clock_frequency);

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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity clock_source_tb is
	generic(clock_frequency: positive; hold_rst: positive := 1);
	port(
		stop:            in     std_ulogic := '0';
		clk:             buffer std_ulogic;
		clk_with_jitter: out    std_ulogic := '0';
		rst:             out    std_ulogic := '0');
end entity;

architecture rtl of clock_source_tb is
	constant clock_period: time      :=  1000 ms / clock_frequency;
	signal jitter_delay:   time      := 0 ns;
	signal jitter_clk:     std_ulogic := '0';
begin
	clk_process: process
		variable seed1, seed2 : positive;
		variable r : real;
	begin
		while stop = '0' loop
			uniform(seed1, seed2, r);
			jitter_delay <= r * 1 ns;

			clk <= '1';
			wait for clock_period / 2;
			clk <= '0';
			wait for clock_period / 2;
		end loop;
		wait;
	end process;

	clk_with_jitter <= transport clk after jitter_delay;

	rst_process: process
	begin
		rst <= '1';
		wait for clock_period * hold_rst;
		rst <= '0';
		wait;
	end process;

end architecture;

------------------------- Generic Register of std_ulogic_vector ----------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reg is
	generic(
		N: positive);
	port
	(
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		we:  in  std_ulogic;
		di:  in  std_ulogic_vector(N - 1 downto 0);
		do:  out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture rtl of reg is
	signal r_c, r_n: std_ulogic_vector(N - 1 downto 0) := (others => '0');
begin
	do <= r_c;

	process(rst, clk)
	begin
		if rst = '1' then
			r_c <= (others => '0');
		elsif rising_edge(clk) then
			r_c <= r_n;
		end if;
	end process;

	process(r_c, di, we)
	begin
		r_n <= r_c;
		if we = '1' then
			r_n <= di;
		end if;
	end process;
end;

------------------------- Generic Register of std_ulogic_vector ----------------------

------------------------- Shift register --------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- https://stackoverflow.com/questions/36342960/optional-ports-in-vhdl
entity shift_register is
	generic(N: positive);
	port
	(
		clk:     in  std_ulogic;
		rst:     in  std_ulogic;
		we:      in  std_ulogic;
		di:      in  std_ulogic;
		do:      out std_ulogic;

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
		if rst = '1' then
			r_c <= (others => '0');
		elsif rising_edge(clk) then
			r_c <= r_n;
		end if;
	end process;

	process(r_c, di, we, load_i, load_we)
	begin
		if load_we = '1' then
			r_n <= load_i;
		else
			r_n <= "0" & r_c(N - 1 downto 1);
			if we = '1' then
				r_n(N-1) <= di;
			end if;
		end if;
	end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity shift_register_tb is
	generic(clock_frequency: positive);
end entity;

architecture behav of shift_register_tb is
	constant N: positive := 8;
	constant clock_period: time :=  1000 ms / clock_frequency;
	signal we: std_ulogic := '0';
	signal di: std_ulogic := '0';
	signal do: std_ulogic := '0';

	signal clk, rst: std_ulogic := '0';
	signal stop: std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.shift_register
	generic map(N => N) port map(clk => clk, rst => rst, we => we, di => di, do => do);

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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.max;
use work.util.n_bits;

entity timer_us is
	generic(
		clock_frequency: positive;
		timer_period_us: positive := 1);
	port(
		clk:  in std_ulogic := 'X';
		rst:  in std_ulogic := 'X';
		co:  out std_ulogic := '0');
end timer_us;

architecture rtl of timer_us is
	constant cycles:   natural := (clock_frequency / 1000000) * timer_period_us;
	subtype  counter is unsigned(max(1, n_bits(cycles) - 1) downto 0);
	signal   c_c, c_n: counter := (others => '0');
begin
	process (clk, rst)
	begin
		if rst = '1' then
			c_c <= (others => '0');
		elsif rising_edge(clk) then
			c_c <= c_n;
		end if;
	end process;

	process (c_c)
	begin
		if c_c = (cycles - 1) then
			c_n <= (others => '0');
			co  <= '1';
		else
			c_n <= c_c + 1;
			co  <= '0';
		end if;
	end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timer_us_tb is
	generic(clock_frequency: positive);
end;

architecture behav of timer_us_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
	signal co: std_ulogic        := 'X';
	signal clk, rst: std_ulogic  := '0';
	signal stop: std_ulogic      := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.timer_us
		generic map(clock_frequency => clock_frequency, timer_period_us => 1)
		port map(clk => clk, rst => rst, co => co);

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
library ieee;
use ieee.std_logic_1164.all;

entity rising_edge_detector is
port(
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
		if rst = '1' then
			sin_0 <= '0';
			sin_1 <= '0';
		elsif rising_edge(clk) then
			sin_0 <= di;
			sin_1 <= sin_0;
		end if;
	end process;
	do <= not sin_1 and sin_0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rising_edge_detector_tb is
	generic(clock_frequency: positive);
end;

architecture behav of rising_edge_detector_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
	signal di:  std_ulogic := '0';
	signal do: std_ulogic := 'X';

	signal clk, rst: std_ulogic := '0';
	signal stop: std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.rising_edge_detector
		port map(clk => clk, rst => rst, di => di, do => do);

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

library ieee;
use ieee.std_logic_1164.all;

entity rising_edge_detectors is
generic(N: positive);
port(
	clk:    in  std_ulogic;
	rst:    in  std_ulogic;
	di:     in  std_ulogic_vector(N - 1 downto 0);
	do:     out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture structural of rising_edge_detectors is
begin
	changes: for i in N - 1 downto 0 generate
		d_instance: work.util.rising_edge_detector
			port map(clk => clk, rst => rst, di => di(i), do => do(i));
	end generate;
end architecture;

------------------------- Rising Edge Detector --------------------------------------

------------------------- Half Adder ------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity half_adder is
	port(
		a:     in  std_ulogic;
		b:     in  std_ulogic;
		sum:   out std_ulogic;
		carry: out std_ulogic);
end entity;

architecture rtl of half_adder is
begin
	sum   <= a xor b;
	carry <= a and b;
end architecture;

------------------------- Half Adder ------------------------------------------------

------------------------- Full Adder ------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity full_adder is
	port(
		x:     in    std_ulogic;
		y:     in    std_ulogic;
		z:     in    std_ulogic;
		sum:   out   std_ulogic;
		carry: out   std_ulogic);
end entity;

architecture rtl of full_adder is
	signal carry1, carry2, sum1: std_ulogic;
begin
	ha1: entity work.half_adder port map(a => x,    b => y, sum => sum1, carry => carry1);
	ha2: entity work.half_adder port map(a => sum1, b => z, sum => sum,  carry => carry2);
	carry <= carry1 or carry2;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity full_adder_tb is
	generic(clock_frequency: positive);
end entity;

architecture behav of full_adder_tb is
	constant clock_period: time  := 1000 ms / clock_frequency;
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
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.full_adder port map(x => x, y => y, z => z, sum => sum, carry => carry);

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

-- Originally from http://www.deathbylogic.com/2013/07/vhdl-standard-fifo/
-- @copyright Public Domain
-- @todo Add more comments about the FIFOs origin, add assertions test
-- synthesis, make this more generic (with empty FIFO and FIFO count signals)
--
-- The code can be used freely and appears to be public domain, comment
-- from author is: "You can use any code posted here freely, there is no copyright."
--
-- @note The FIFO has been modified from the original to bring it in line with
-- this projects coding standards.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo is
	generic(
		data_width: positive;
		fifo_depth: positive);
	port(
		clk:   in  std_ulogic;
		rst:   in  std_ulogic;
		we:    in  std_ulogic;
		di:    in  std_ulogic_vector (data_width - 1 downto 0);
		re:    in  std_ulogic;
		do:    out std_ulogic_vector (data_width - 1 downto 0);
		empty: out std_ulogic := '1';
		full:  out std_ulogic := '0');
end fifo;

architecture behavioral of fifo is
begin

	-- memory pointer process
	fifo_proc: process (clk, rst)
		type fifo_memory is array (0 to fifo_depth - 1) of std_ulogic_vector (data_width - 1 downto 0);
		variable memory: fifo_memory;

		variable head: natural range 0 to fifo_depth - 1;
		variable tail: natural range 0 to fifo_depth - 1;

		variable looped: boolean;
	begin
		if rst = '1' then
			head := 0;
			tail := 0;

			looped := false;

			full  <= '0';
			empty <= '1';
			do    <= (others => '0');
		elsif rising_edge(clk) then
			do    <= (others => '0');
			if re = '1' then
				if looped = true or head /= tail then
					-- update data output
					do <= memory(tail);

					-- update tail pointer as needed
					if tail = fifo_depth - 1 then
						tail   := 0;
						looped := false;
					else
						tail := tail + 1;
					end if;
				end if;
			end if;

			if we = '1' then
				if looped = false or head /= tail then
					-- write data to memory
					memory(head) := di;

					-- increment head pointer as needed
					if head = fifo_depth - 1 then
						head := 0;

						looped := true;
					else
						head := head + 1;
					end if;
				end if;
			end if;

			-- update empty and full flags
			if head = tail then
				if looped then
					full  <= '1';
				else
					empty <= '1';
				end if;
			else
				empty	<= '0';
				full	<= '0';
			end if;
		end if;
	end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo_tb is
	generic(clock_frequency: positive);
end entity;

architecture behavior of fifo_tb is
	constant clock_period: time  := 1000 ms / clock_frequency;
	constant data_width: positive := 8;
	constant fifo_depth: positive := 16;

	signal di:      std_ulogic_vector(data_width - 1 downto 0) := (others => '0');
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
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	stop <= '1' when stop_w = '1' and stop_r = '1' else '0';

	uut: entity work.fifo
		generic map(data_width => data_width, fifo_depth => fifo_depth)
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
	generic(
		N: positive);
	port(
		clk:     in  std_ulogic;
		rst:     in  std_ulogic;
		ce:      in  std_ulogic;
		cr:      in  std_ulogic;
		dout:    out std_ulogic_vector(N - 1 downto 0);

		load_we: in  std_ulogic := '0';
		load_i:  in  std_ulogic_vector(N - 1 downto 0) := (others => '0'));
end entity;

architecture rtl of counter is
	signal c_c, c_n: unsigned(N - 1 downto 0) := (others => '0');
begin
	dout <= std_ulogic_vector(c_c);

	process(clk, rst)
	begin
		if rst = '1' then
			c_c <= (others => '0');
		elsif rising_edge(clk) then
			c_c <= c_n;
		end if;
	end process;

	process(c_c, cr, ce, load_we, load_i)
	begin
		c_n <= c_c;
		if load_we = '1' then
			c_n <= unsigned(load_i);
		else
			if cr = '1' then
				c_n <= (others => '0');
			elsif ce = '1' then
				c_n <= c_c + 1;
			end if;
		end if;
	end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter_tb is
	generic(clock_frequency: positive);
end entity;

architecture behavior of counter_tb is
	constant clock_period: time     := 1000 ms / clock_frequency;
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
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.counter
		generic map(
			N => length)
		port map(
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
-- This component could be improved a lot, and it could also be used to
-- calculate CRCs, which are basically the same computation. Its interface
-- is not the best either, being a free running counter.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity lfsr is
	generic(constant tap: std_ulogic_vector);
	port
	(
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		we:  in  std_ulogic;
		ce:  in  std_ulogic := '1';
		di:  in  std_ulogic_vector(tap'high + 1 downto tap'low);
		do:  out std_ulogic_vector(tap'high + 1 downto tap'low));
end entity;

architecture rtl of lfsr is
	signal r_c, r_n : std_ulogic_vector(di'range) := (others => '0');
begin
	do <= r_c;

	process(rst, clk)
	begin
		if rst = '1' then
			r_c <= (others => '0');
		elsif rising_edge(clk) then
			r_c <= r_n;
		end if;
	end process;

	process(r_c, di, we, ce)
	begin
		if we = '1' then
			r_n <= di;
		elsif ce = '1' then

			r_n(r_n'high) <= r_c(r_c'low);

			for i in tap'high downto tap'low loop
				if tap(i) = '1' then
					r_n(i) <= r_c(r_c'low) xor r_c(i+1);
				else
					r_n(i) <= r_c(i+1);
				end if;
			end loop;
		else
			r_n <= r_c;
		end if;
	end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity lfsr_tb is
	generic(clock_frequency: positive);
end entity;

architecture behavior of lfsr_tb is
	constant clock_period: time     := 1000 ms / clock_frequency;
	signal we: std_ulogic := '0';
	signal do, di: std_ulogic_vector(7 downto 0) := (others => '0');

	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.lfsr
		generic map(tap => "0111001")
		port map(clk => clk,
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity io_pins is
	generic(
		N: positive);
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

	control_r: entity work.reg generic map(N => N) port map(clk => clk, rst => rst, di => control, we => control_we, do => control_o);
	din_r:     entity work.reg generic map(N => N) port map(clk => clk, rst => rst, di => din,     we => din_we,     do => din_o);

	pins_i: for i in control_o'range generate
		dout(i) <= pins(i)  when control_o(i) = '0' else '0';
		pins(i) <= din_o(i) when control_o(i) = '1' else 'Z';
	end generate;

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity io_pins_tb is
	generic(clock_frequency: positive);
end entity;

architecture behavior of io_pins_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
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
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.io_pins
		generic map(N => N)
		port map(
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

library ieee;
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
	generic(addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port(
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
						slv((j*4)-1 downto (j*4)-4) := hex_char_to_std_ulogic_vector(c);
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
				a_dout <= ram(to_integer(unsigned(a_addr)));
			else
				a_dout <= (others => '0');
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
				b_dout <= ram(to_integer(unsigned(b_addr)));
			else
				b_dout <= (others => '0');
			end if;
		end if;
	end process;
end architecture;

--- Single Port Model ---

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.util.all;

entity single_port_block_ram is
	generic(addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port(
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
						slv((j*4)-1 downto (j*4)-4) := hex_char_to_std_ulogic_vector(c);
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
				dout <= ram(to_integer(unsigned(addr)));
			else
				dout <= (others => '0');
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

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.single_port_block_ram;
use work.util.counter;
use work.util.all;

entity data_source is
	generic(addr_length: positive    := 12;
		data_length: positive    := 16;
		file_name:   string      := "memory.bin";
		file_type:   file_format := FILE_BINARY);
	port(
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
		generic map(
			N => addr_length)
		port map(
			clk      =>  clk,
			rst      =>  rst,
			ce       =>  ce,
			cr       =>  cr,
			dout     =>  addr,
			load_i   =>  load,
			load_we  =>  load_we);

	ram: work.util.single_port_block_ram
		generic map(
			addr_length => addr_length,
			data_length => data_length,
			file_name   => file_name,
			file_type   => file_type)
		port map(
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
-- just contains an instantiation of this core.
--

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ucpu is
	generic(width: positive range 8 to 32 := 8);
	port(
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
	signal alu:              std_ulogic_vector(1 downto 0)        := (others => '0');
	signal state_c, state_n: std_ulogic                           := '0'; -- FETCH/Single cycle instruction or EXECUTE
begin
	pc          <= std_ulogic_vector(pc_n);
	do          <= std_ulogic_vector(a_c(do'range));
	alu         <= op(op'high downto op'high - 1);
	adr         <= op(adr'range);
	we          <= '1' when alu = "10" else '0'; -- STORE
	re          <= alu(1) nor state_c;           -- FETCH for ADD and NOR
	state_n     <= alu(1) nor state_c;           -- FETCH not taken or FETCH done
	pc_n        <= unsigned(op(pc_n'range)) when (alu = "11"    and a_c(a_c'high) = '0') else -- Jump when carry set
			pc_c                    when (state_c = '0' and alu(1) = '0')        else -- FETCH
			pc_c + 1; -- EXECUTE

	process(clk, rst)
	begin
		if rst = '1' then
			a_c     <= (others => '0');
			pc_c    <= (others => '0');
			state_c <= '0';
		elsif rising_edge(clk) then
			a_c     <= a_n;
			pc_c    <= pc_n;
			state_c <= state_n;
		end if;
	end process;

	process(op, alu, di, a_c, state_c)
	begin
		a_n     <= a_c;

		if alu = "11" and a_c(a_c'high) = '0' then a_n(a_n'high) <= '0'; end if;

		if state_c = '1' then -- EXECUTE for ADD and NOR
			assert alu(1) = '0' severity failure;
			if alu(0) = '0' then a_n <= '0' & a_c(di'range) + unsigned('0' & di); end if;
			if alu(0) = '1' then a_n <= a_c nor '0' & unsigned(di); end if;
		end if;
	end process;
end architecture;

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.util.all;

entity ucpu_tb is
	generic(
		clock_frequency: positive;
		file_name: string := "ucpu.bin");
end entity;

architecture testing of ucpu_tb is
	constant clk_period:      time     := 1000 ms / clock_frequency;

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
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	bram_0: entity work.dual_port_block_ram
	generic map(
		addr_length => addr_length,
		data_length => data_length,
		file_name   => file_name,
		file_type   => FILE_BINARY)
	port map(
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
	generic map(width => data_length)
	port map(
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
		wait for clk_period * 1000;
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
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity restoring_divider is
	generic(N: positive);
	port(
		clk:   in  std_ulogic;
		rst:   in  std_ulogic := '0';

		a:     in  unsigned(N - 1 downto 0);
		b:     in  unsigned(N - 1 downto 0);
		start: in  std_ulogic;
		done:  out std_ulogic;
		c:     out unsigned(N - 1 downto 0));
end entity;

architecture rtl of restoring_divider is
	signal a_c, a_n: unsigned(a'range) := (others => '0');
	signal b_c, b_n: unsigned(b'range) := (others => '0');
	signal m_c, m_n: unsigned(b'range) := (others => '0');
	signal o_c, o_n: unsigned(c'range) := (others => '0');
	signal e_c, e_n: std_ulogic         := '0';
	signal count_c, count_n: unsigned(work.util.n_bits(N) downto 0) := (others => '0');
begin
	c <= o_n;

	process(clk, rst)
	begin
		if rst = '1' then
			a_c      <=  (others  =>  '0');
			b_c      <=  (others  =>  '0');
			m_c      <=  (others  =>  '0');
			o_c      <=  (others  =>  '0');
			e_c      <=  '0';
			count_c  <=  (others  =>  '0');
		elsif rising_edge(clk) then
			a_c      <=  a_n;
			b_c      <=  b_n;
			m_c      <=  m_n;
			o_c      <=  o_n;
			e_c      <=  e_n;
			count_c  <=  count_n;
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
			a_n   <= a;
			b_n   <= b;
			m_v   := (others => '0');
			e_n   <= '1';
			o_n   <= (others => '0');
			count_n <= (others => '0');
		elsif e_c = '1' then
			if count_c(count_c'high) = '1' then
				done  <= '1';
				e_n   <= '0';
				o_n   <= a_c;
				count_n <= (others => '0');
			else
				m_v(b'high downto 1) := m_v(b'high - 1 downto 0);
				m_v(0) := a_c(a'high);
				a_n(a'high downto 1) <= a_c(a'high - 1 downto 0);
				m_v := m_v - b_c;
				if m_v(m_v'high) = '1' then
					m_v := m_v + b_c;
					a_n(0) <= '0';
				else
					a_n(0) <= '1';
				end if;
				count_n <= count_c + 1;
			end if;
		else
			count_n <= (others => '0');
		end if;
		m_n <= m_v;
	end process;
end architecture;

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity restoring_divider_tb is
	generic(clock_frequency: positive);
end entity;

architecture testing of restoring_divider_tb is
	constant clk_period: time     := 1000 ms / clock_frequency;
	constant N:          positive := 8;

	signal a: unsigned(N - 1 downto 0) := (others => '0');
	signal b: unsigned(N - 1 downto 0) := (others => '0');
	signal c: unsigned(N - 1 downto 0) := (others => '0');
	signal start, done: std_ulogic := '0';

	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.restoring_divider
		generic map(N => N)
		port map(
			clk   => clk,
			rst   => rst,
			a     => a,
			b     => b,
			start => start,
			done  => done,
			c     => c);

	stimulus_process: process
	begin
		wait for clk_period * 2;

		a <= x"64";
		b <= x"0A";
		start <= '1';
		wait for clk_period * 1;
		start <= '0';
		wait until done = '1';
		--assert c = x"0A" severity failure;

		wait for clk_period * 10;
		b     <= x"05";
		start <= '1';
		wait for clk_period * 1;
		start <= '0';
		wait until done = '1';
		--assert c = x"14" severity failure;

		stop <= '1';
		wait;
	end process;

end architecture;
------------------------- Restoring Divider ---------------------------------------------------

------------------------- Debouncer -----------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity debounce_us is
	generic(clock_frequency: positive; timer_period_us: natural);
	port(
		clk:   in  std_ulogic;
		di:    in  std_ulogic;
		do:    out std_ulogic := '0');
end entity;

architecture rtl of debounce_us is
	signal ff: std_ulogic_vector(1 downto 0) := (others => '0');
	signal rst, done: std_ulogic             := '0';
begin
	timer: work.util.timer_us
		generic map(
			clock_frequency => clock_frequency,
			timer_period_us => timer_period_us)
		port map(
			clk => clk,
			rst => rst,
			co  => done);

	process(clk)
	begin
		if rising_edge(clk) then
			ff(0) <= di;
			ff(1) <= ff(0);
			rst   <= '0';
			if (ff(0) xor ff(1)) = '1' then
				rst <= '1';
			elsif done = '1' then
				do  <= ff(1);
			end if;
		end if;
	end process;
end architecture;

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity debounce_us_tb is
	generic(clock_frequency: positive);
end entity;

architecture testing of debounce_us_tb is
	constant clk_period: time     := 1000 ms / clock_frequency;

	signal di,  do:  std_ulogic := '0';
	signal clk, rst: std_ulogic := '0';
	signal stop:     std_ulogic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: work.util.debounce_us
		generic map(clock_frequency => clock_frequency, timer_period_us => 1)
		port map(clk => clk, di => di, do => do);

	stimulus_process: process
	begin
		wait for clk_period * 2;
		di <= '1';

		wait for 1.5 us;

		stop <= '1';
		wait;
	end process;
end architecture;
------------------------- Debouncer -----------------------------------------------------------

------------------------- Debouncer Block -----------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity debounce_block_us is
	generic(N: positive; clock_frequency: positive; timer_period_us: natural);
	port(
		clk:   in  std_ulogic;
		di:    in  std_ulogic_vector(N - 1 downto 0);
		do:    out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture structural of debounce_block_us is
begin
	debouncer: for i in N - 1 downto 0 generate
		d_instance: work.util.debounce_us
			generic map(
				clock_frequency => clock_frequency,
				timer_period_us => timer_period_us)
			port map(clk => clk, di => di(i), do => do(i));
	end generate;
end architecture;

------------------------- Debouncer Block -----------------------------------------------------

------------------------- State Changed -------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity state_changed is
	port(
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
		if rst = '1' then
			state_c <= (others => '0');
		elsif rising_edge(clk) then
			state_c <= state_n;
		end if;
	end process;

	do <= '1' when (state_c(0) xor state_c(1)) = '1' else '0';

	process(di, state_c)
	begin
		state_n(0) <= state_c(1);
		state_n(1) <= di;
	end process;
end architecture;

------------------------- Change State --------------------------------------------------------

------------------------- Change State Block --------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity state_block_changed is
	generic(N: positive);
	port(
		clk: in  std_ulogic;
		rst: in  std_ulogic;
		di:  in  std_ulogic_vector(N - 1 downto 0);
		do:  out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture structural of state_block_changed is
begin
	changes: for i in N - 1 downto 0 generate
		d_instance: work.util.state_changed
			port map(clk => clk, rst => rst, di => di(i), do => do(i));
	end generate;
end architecture;

------------------------- Change State Block --------------------------------------------------
