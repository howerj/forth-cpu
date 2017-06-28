-------------------------------------------------------------------------------
--| @file util.vhd
--| @brief A collection of utilities and simple components. The components
--| should be synthesizable, and the functions can be used within synthesizable
--| components, unless marked with a "_tb" suffix (or is the function n_bits).
--| @author         Richard James Howe
--| @copyright      Copyright 2017 Richard James Howe
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| @todo Add mux, demux (X To N, IN/OUT), debouncer, serial to parallel (and
--| vice versa), pulse generator, small RAM model, population count
--| priority encoder, types, and other generic functions and components.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package util is
	component clock_source_tb is
		generic(clock_frequency: positive; hold_rst: positive := 1);
		port(
			stop:            in  std_logic := '0';
			clk:             buffer std_logic;
			clk_with_jitter: out std_logic := '0';
			rst:             out std_logic := '0');
	end component;

	component reg
		generic(N: positive);
		port(
			clk: in  std_logic;
			rst: in  std_logic;
			we:  in  std_logic;
			di:  in  std_logic_vector(N - 1 downto 0);
			do:  out std_logic_vector(N - 1 downto 0));
	end component;

	component shift_register
		generic(N: positive);
		port(
			clk: in  std_logic;
			rst: in  std_logic;
			we:  in  std_logic;
			di:  in  std_logic;
			do:  out std_logic);
	end component;

	component shift_register_tb
		generic(clock_frequency: positive);
		port(
			clk:  in std_logic;
			rst:  in std_logic;
			stop: in std_logic);
	end component;

	component timer_us
		generic(clock_frequency: positive; timer_period_us: natural);
		port(
			clk: in  std_logic;
			rst: in  std_logic;
			co:  out std_logic);
	end component;

	component timer_us_tb
		generic(clock_frequency: positive);
		port(stop: in std_logic);
	end component;

	component edge is
	port(
		clk:    in  std_logic;
		rst:    in  std_logic;
		sin:    in  std_logic;
       		output: out std_logic);
	end component;

	component edge_tb is
		generic(clock_frequency: positive);
		port(stop: in std_logic);
	end component;

	-- @note half_adder test bench is folded in to full_adder_tb
	component half_adder is
		port(
			a:     in  std_logic;
			b:     in  std_logic;
			sum:   out std_logic;
			carry: out std_logic);
	end component;

	component full_adder is
		port(
			x:     in    std_logic;
			y:     in    std_logic;
			z:     in    std_logic;
			sum:   out   std_logic;
			carry: out   std_logic);
	end component;

	component full_adder_tb is
		generic(clock_frequency: positive);
		port(stop: in std_logic);
	end component;

	component function_tb is
		generic(clock_frequency: positive);
		port(stop: in std_logic);
	end component;

	component fifo is
		generic (data_width: positive := 8;
			fifo_depth: positive  := 16);
		port (
			clk:   in  std_logic;
			rst:   in  std_logic;
			din:   in  std_logic_vector(data_width - 1 downto 0);
			we:    in  std_logic;
			re:    in  std_logic;
			do:    out std_logic_vector(data_width - 1 downto 0);
			full:  out std_logic := '0';
			empty: out std_logic := '1');
	end component;

	component fifo_tb is
		generic(clock_frequency: positive);
		port(stop: in std_logic);
	end component;

	component counter is
		generic(
			length: positive);
		port(
			clk: in std_logic;
			rst: in std_logic;
			ce:  in std_logic;
			cr:  in std_logic;
			dout: out std_logic_vector(length - 1 downto 0));
	end component;

	component counter_tb is
		generic(clock_frequency: positive);
		port(stop: in std_logic);
	end component;

	component lfsr is
		generic(tap: std_logic_vector);
		port
		(
			clk: in  std_logic;
			rst: in  std_logic;
			ce:  in  std_logic := '1';
			we:  in  std_logic;
			di:  in  std_logic_vector(tap'high + 1 to tap'low);
			do:  out std_logic_vector(tap'high + 1 to tap'low));
	end component;

	component lfsr_tb is
		generic(clock_frequency: positive);
		port(stop: in std_logic);
	end component;


	function max(a: natural; b: natural) return natural;
	function min(a: natural; b: natural) return natural;
	function n_bits(x: natural) return natural;
	function n_bits(x: std_logic_vector) return natural;
	function reverse (a: in std_logic_vector) return std_logic_vector;
	function invert(slv:std_logic_vector) return std_logic_vector;
	function parity(slv:std_logic_vector; even: boolean) return std_logic;
	function select_bit(indexed, selector: std_logic_vector) return std_logic;
	function priority(order: std_logic_vector; high: boolean) return natural;
	function priority(order: std_logic_vector; high: boolean) return std_logic_vector;
	function mux(a: std_logic_vector; b: std_logic_vector; sel: std_logic) return std_logic_vector;
	function mux(a: std_logic; b: std_logic; sel: std_logic) return std_logic;
	function mux(a, b : std_logic_vector) return std_logic;
	function decode(encoded: std_logic_vector) return std_logic_vector;

	--- Not synthesizable ---

	subtype configuration_name is string(1 to 8);

	type configuration_item is record
		name:  configuration_name;
		value: integer;
	end record;

	type configuration_items is array(integer range <>) of configuration_item;

	function search_configuration_tb(find_me: configuration_name; ci: configuration_items) return integer;
	procedure read_configuration_tb(file_name: string; ci: inout configuration_items);
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

	function n_bits(x: std_logic_vector) return natural is
	begin
		return n_bits(x'high);
	end function;

	-- https://stackoverflow.com/questions/13584307
	function reverse (a: in std_logic_vector) return std_logic_vector is
		variable result: std_logic_vector(a'range);
		alias aa: std_logic_vector(a'reverse_range) is a;
	begin
		for i in aa'range loop
			result(i) := aa(i);
		end loop;
		return result;
	end;

	function invert(slv: std_logic_vector) return std_logic_vector is
		variable z: std_logic_vector(slv'range);
	begin
		for i in slv'range loop
			z(i) := not(slv(i));
		end loop;
		return z;
	end;

	function parity(slv: std_logic_vector; even: boolean) return std_logic is
		variable z: std_logic := '0';
	begin
		if not even then
			z := '1';
		end if;
		for i in slv'range loop
			z := z xor slv(i);
		end loop;
		return z;
	end;

	function select_bit(indexed, selector: std_logic_vector) return std_logic is
		variable z: std_logic := 'X';
	begin
		assert n_bits(indexed) = selector'high + 1 severity failure;
		for i in indexed'range loop
			if i = to_integer(unsigned(selector)) then
				z := indexed(i);
			end if;
		end loop;
		return z;
	end;

	function priority(order: std_logic_vector; high: boolean) return natural is
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

	function priority(order: std_logic_vector; high: boolean) return std_logic_vector is
		variable length: natural := n_bits(order'length);
	begin
		return std_logic_vector(to_unsigned(priority(order, high), length));
	end;

	function mux(a: std_logic_vector; b: std_logic_vector; sel: std_logic) return std_logic_vector is
		variable m: std_logic_vector(a'range) := (others => 'X');
	begin
		if sel = '0' then m := a; else m := b; end if;
		return m;
	end;

	function mux(a: std_logic; b: std_logic; sel: std_logic) return std_logic is
		variable m: std_logic := 'X';
	begin
		if sel = '0' then m := a; else m := b; end if;
		return m;
	end;

	function mux(a, b : std_logic_vector) return std_logic is
		variable r: std_logic_vector(b'length - 1 downto 0) := (others => 'X');
		variable i: integer;
	begin
		r := b;
		i := to_integer(unsigned(a));
		return r(i);
	end;

	function decode(encoded : std_logic_vector) return std_logic_vector is
		variable r: std_logic_vector((2 ** encoded'length) - 1 downto 0) := (others => '0');
		variable i: natural;
	begin
		i    := to_integer(unsigned(encoded));
		r(i) := '1';
		return r;
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
	end function;

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

------------------------- Function Test Bench ---------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use work.util.all;

entity function_tb is
	generic(clock_frequency: positive);
	port(stop: in std_logic);
end entity;

architecture behav of function_tb is
begin

	stimulus_process: process
	begin
		assert max(5, 4) = 5 severity failure;
		assert work.util.min(5, 4) = 4 severity failure;
		assert n_bits(1) = 1 severity failure;
		assert n_bits(2) = 1 severity failure;
		assert n_bits(7) = 3 severity failure;
		assert n_bits(8) = 3 severity failure;
		assert n_bits(9) = 4 severity failure;
		assert reverse("1") = "1" severity failure;
		assert reverse("0") = "0" severity failure;
		assert reverse("10") = "01" severity failure;
		assert reverse("11") = "11" severity failure;
		assert reverse("0101") = "1010" severity failure;
		assert invert("1") = "0" severity failure;
		assert invert("0") = "1" severity failure;
		assert invert("0101") = "1010" severity failure;
		assert select_bit("01000","01") = '1' severity failure;
		assert parity("0", true) = '0' severity failure;
		assert parity("1", true) = '1' severity failure;
		assert parity("11", true) = '0' severity failure;
		assert parity("1010001", true) = '1' severity failure;
		assert parity("0", false) = '1' severity failure;
		assert parity("1", false) = '0' severity failure;
		assert parity("11", false) = '1' severity failure;
		assert parity("1010001", false) = '0' severity failure;
		assert priority("01001", false) = 1 severity failure;
		assert mux("1010", "0101", '0') = "1010" severity failure;
		assert mux("1010", "0101", '1') = "0101" severity failure;
		assert decode("00") = "0001" severity failure;
		assert decode("01") = "0010" severity failure;
		assert decode("10") = "0100" severity failure;
		assert decode("11") = "1000" severity failure;
		-- n_bits(x: std_logic_vector) return natural;
		-- mux(a, b : std_logic_vector) return std_logic;
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
		stop:            in     std_logic := '0';
		clk:             buffer std_logic;
		clk_with_jitter: out    std_logic := '0';
		rst:             out    std_logic := '0');
end entity;

architecture rtl of clock_source_tb is
	constant clock_period: time      :=  1000 ms / clock_frequency;
	signal jitter_delay:   time      := 0 ns;
	signal jitter_clk:     std_logic := '0';
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

------------------------- Generic Register of std_logic_vector ----------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reg is
	generic(N: positive := 8);
	port
	(
		clk: in  std_logic;
		rst: in  std_logic;
		we:  in  std_logic;
		di:  in  std_logic_vector(N-1 downto 0);
		do:  out std_logic_vector(N-1 downto 0));
end entity;

architecture rtl of reg is
	signal r_c, r_n : std_logic_vector(N-1 downto 0) := (others => '0');
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

------------------------- Generic Register of std_logic_vector ----------------------

------------------------- Shift register --------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- @todo Add optional parallel load and store
-- https://stackoverflow.com/questions/36342960/optional-ports-in-vhdl
entity shift_register is
	generic(N: positive);
	port
	(
		clk: in  std_logic;
		rst: in  std_logic;
		we:  in  std_logic;
		di:  in  std_logic;
		do:  out std_logic);
end entity;

architecture rtl of shift_register is
	signal r_c, r_n : std_logic_vector(N-1 downto 0) := (others => '0');
begin
	do <= r_c(0);

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
		r_n <= "0" & r_c(N-1 downto 1);
		if we = '1' then
			r_n(N-1) <= di;
		end if;
	end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity shift_register_tb is
	generic(clock_frequency: positive);
	port(stop: in std_logic);
end entity;

architecture behav of shift_register_tb is
	constant N: positive := 8;
	constant clock_period: time :=  1000 ms / clock_frequency;
	signal we: std_logic := '0';
	signal di: std_logic := '0';
	signal do: std_logic := '0';

	signal clk, rst: std_logic := '0';
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

		assert stop = '0' report "Test bench not run to completion";
		wait;
	end process;
end;
------------------------- Shift register --------------------------------------------

------------------------- Microsecond Timer -----------------------------------------
--| @todo There is a special case for the microsecond timer, one where we do
--| not have to use a comparator, but instead we can use the top bit of a
--| counter to signal the timer has elapsed. This special case could be
--| selected for with generics. The situation occurs when the cycles variable
--| is a power of two less one.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.max;
use work.util.n_bits;

entity timer_us is
	generic(
		clock_frequency: positive;
		timer_period_us: natural  := 0);
	port(
		clk:  in std_logic := 'X';
		rst:  in std_logic := 'X';
		co:  out std_logic := '0');
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
	port(stop: in std_logic);
end;

architecture behav of timer_us_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
	signal co: std_logic := 'X';
	signal clk, rst: std_logic := '0';
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
		assert stop = '0' report "Test bench not run to completion";
		wait;
	end process;

end;

------------------------- Microsecond Timer -----------------------------------------

------------------------- Edge Detector ---------------------------------------------
--| @todo have generic to decide whether it is on the rising or falling edge
library ieee;
use ieee.std_logic_1164.all;

entity edge is
port (
	clk:    in  std_logic;
	rst:    in  std_logic;
	sin:    in  std_logic;
	output: out std_logic);
end;

architecture rtl of edge is
	signal sin0: std_logic := '0';
	signal sin1: std_logic := '0';
begin
	rising_edge_detector: process(clk, rst)
	begin
		if rst = '1' then
			sin0 <= '0';
			sin1 <= '0';
		elsif rising_edge(clk) then
			sin0 <= sin;
			sin1 <= sin0;
		end if;
	end process;
	output <= not sin1 and sin0;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity edge_tb is
	generic(clock_frequency: positive);
	port(stop: in std_logic);
end;

architecture behav of edge_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
	signal sin:    std_logic := '0';
	signal output: std_logic := 'X';

	signal clk, rst: std_logic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.edge
		port map(clk => clk, rst => rst, sin => sin, output => output);

	stimulus_process: process
	begin
		wait for clock_period * 5;
		assert output = '0' severity failure;
		wait for clock_period;
		sin <= '1';
		wait for clock_period * 0.5;
		assert output = '1' severity failure;
		wait for clock_period * 1.5;
		sin <= '0';
		assert output = '0' severity failure;
		wait for clock_period;
		assert output = '0' severity failure;

		assert stop = '0' report "Test bench not run to completion";
		wait;
	end process;
end architecture;

------------------------- Edge Detector ---------------------------------------------

------------------------- Half Adder ------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity half_adder is
	port(
		a:     in  std_logic;
		b:     in  std_logic;
		sum:   out std_logic;
		carry: out std_logic);
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
		x:     in    std_logic;
		y:     in    std_logic;
		z:     in    std_logic;
		sum:   out   std_logic;
		carry: out   std_logic);
end entity;

architecture rtl of full_adder is
	signal carry1, carry2, sum1: std_logic;
begin
	ha1: entity work.half_adder port map(a => x,    b => y, sum => sum1, carry => carry1);
	ha2: entity work.half_adder port map(a => sum1, b => z, sum => sum,  carry => carry2);
	carry <= carry1 or carry2;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity full_adder_tb is
	generic(clock_frequency: positive);
	port(stop: in std_logic);
end entity;

architecture behav of full_adder_tb is
	constant clock_period: time  := 1000 ms / clock_frequency;
	signal x, y, z:    std_logic := '0';
	signal sum, carry: std_logic := '0';

	type stimulus_data   is array (0 to 7)              of std_logic_vector(2 downto 0);
	type stimulus_result is array (stimulus_data'range) of std_logic_vector(0 to     1);

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

	signal clk, rst: std_logic := '0';
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
					"For: "       & std_logic'image(x) & std_logic'image(y) & std_logic'image(z) &
					" Got: "      & std_logic'image(sum)          & std_logic'image(carry) &
					" Expected: " & std_logic'image(result(i)(0)) & std_logic'image(result(i)(1))
				severity failure;
			wait for clock_period;
		end loop;

		assert stop = '0' report "Test bench not run to completion";
		wait;
	end process;
end architecture;

------------------------- Full Adder ------------------------------------------------

------------------------- FIFO ------------------------------------------------------

-- Originally from http://www.deathbylogic.com/2013/07/vhdl-standard-fifo/
-- @copyright Public Domain
-- @todo Add more comments about the FIFOs origin, add assertions test
-- synthesis.
--
-- The code can be used freely and appears to be public domain, comment
-- from author is: "You can use any code posted here freely, there is no copyright."
-- @note The FIFO has been modified from the original to bring it in line with
-- this projects coding standards.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo is
	generic(
		data_width: positive := 8;
		fifo_depth: positive := 16);
	port(
		clk:   in  std_logic;
		rst:   in  std_logic;
		we:    in  std_logic;
		din:   in  std_logic_vector (data_width - 1 downto 0);
		re:    in  std_logic;
		do:    out std_logic_vector (data_width - 1 downto 0);
		empty: out std_logic := '1';
		full:  out std_logic := '0');
end fifo;

architecture behavioral of fifo is
begin

	-- memory pointer process
	fifo_proc: process (clk, rst)
		type fifo_memory is array (0 to fifo_depth - 1) of std_logic_vector (data_width - 1 downto 0);
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
					memory(head) := din;
					
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
	port(stop: in std_logic);
end entity;

architecture behavior of fifo_tb is
	constant clock_period: time  := 1000 ms / clock_frequency;
	constant data_width: positive := 8;
	constant fifo_depth: positive := 16;

	--inputs
	signal din: std_logic_vector(data_width - 1 downto 0) := (others => '0');
	signal re:  std_logic := '0';
	signal we:  std_logic := '0';
	
	--outputs
	signal do:    std_logic_vector(data_width - 1 downto 0) := (others => '0');
	signal empty: std_logic := '0';
	signal full:  std_logic  := '0';
	
	signal clk, rst: std_logic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.fifo
		generic map(data_width => data_width, fifo_depth => fifo_depth)
		port map (
			clk   => clk,
			rst   => rst,
			din   => din,
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
			din <= std_logic_vector(counter);
			wait for clock_period * 1;
			we <= '1';
			wait for clock_period * 1;
			we <= '0';
		end loop;
		
		wait for clock_period * 20;
		
		for i in 1 to 32 loop
			counter := counter + 1;
			din <= std_logic_vector(counter);
			wait for clock_period * 1;
			we <= '1';
			wait for clock_period * 1;
			we <= '0';
		end loop;
		
		assert stop = '0' report "Test bench not run to completion";
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

		assert stop = '0' report "Test bench not run to completion";
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
		length: positive);
	port(
		clk:  in std_logic;
		rst:  in std_logic;
		ce:   in std_logic;
		cr:   in std_logic;
		dout: out std_logic_vector(length - 1 downto 0));
end entity;

architecture rtl of counter is
	signal c_c, c_n: unsigned(length - 1 downto 0) := (others => '0');
begin
	dout <= std_logic_vector(c_c);

	process(clk, rst)
	begin
		if rst = '1' then
			c_c <= (others => '0');
		elsif rising_edge(clk) then
			c_c <= c_n;
		end if;
	end process;

	process(c_c, cr, ce)
	begin
		c_n <= c_c;

		if cr = '1' then
			c_n <= (others => '0');
		elsif ce = '1' then
			c_n <= c_c + 1;
		end if;
	end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter_tb is
	generic(
		clock_frequency: positive);
	port(
		stop: in std_logic);
end entity;

architecture behavior of counter_tb is
	constant clock_period: time     := 1000 ms / clock_frequency;
	constant length:       positive := 2;

	-- inputs
	signal ce: std_logic := '0';
	signal cr: std_logic := '0';

	-- outputs
	signal dout: std_logic_vector(length - 1 downto 0);

	-- test data
	type stimulus_data   is array (0 to 16)             of std_logic_vector(1 downto 0);
	type stimulus_result is array (stimulus_data'range) of std_logic_vector(0 to     1);

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

	signal clk, rst: std_logic := '0';
begin
	cs: entity work.clock_source_tb
		generic map(clock_frequency => clock_frequency, hold_rst => 2)
		port map(stop => stop, clk => clk, rst => rst);

	uut: entity work.counter
		generic map(
			length => length)
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
					"For: ce("    & std_logic'image(ce) & ") cr(" & std_logic'image(cr) & ") " &
					" Got: "      & integer'image(to_integer(unsigned(dout))) &
					" Expected: " & integer'image(to_integer(unsigned(result(i))))
				severity failure;
		end loop;
		assert stop = '0' report "Test bench not run to completion";
		wait;
	end process;

end architecture;

------------------------- Free running counter --------------------------------------

------------------------- Linear Feedback Shift Register ----------------------------
-- For good sources on LFSR see
-- * https://sites.ualberta.ca/~delliott/ee552/studentAppNotes/1999f/Drivers_Ed/lfsr.html
-- * https://en.wikipedia.org/wiki/Linear-feedback_shift_register
--
--
-- Some optimal taps
--
-- Taps start at the left most std_logic element of tap at '0' and proceed to
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity lfsr is
	generic(tap: std_logic_vector);
	port
	(
		clk: in  std_logic;
		rst: in  std_logic;
		we:  in  std_logic;
		ce:  in  std_logic := '1';
		di:  in  std_logic_vector(tap'high + 1 downto tap'low);
		do:  out std_logic_vector(tap'high + 1 downto tap'low));
end entity;

architecture rtl of lfsr is
	signal r_c, r_n : std_logic_vector(di'range) := (others => '0');
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
	port(stop: in std_logic);
end entity;

architecture behavior of lfsr_tb is
	constant clock_period: time     := 1000 ms / clock_frequency;
	signal we: std_logic := '0';
	signal do, di: std_logic_vector(7 downto 0) := (others => '0');

	signal clk, rst: std_logic := '0';
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
		we <= '1';
		di <= "00000001";
		wait for clock_period;
		we <= '0';
		wait;
	end process;

end architecture;

------------------------- Linear Feedback Shift Register ----------------------------
