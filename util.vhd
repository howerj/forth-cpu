-------------------------------------------------------------------------------
--| @file util.vhd
--| @brief          A collection of utilities and simple components,
--|                 generic, reusable components are listed here, they
--|                 should also be entirely self contained and ready
--|                 to use by themselves.
--| @author         Richard James Howe
--| @copyright      Copyright 2017 Richard James Howe
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| @todo Make these components type generic if possible
--| @todo Add mux, demux (X To N, IN/OUT), debouncer, serial to parallel (and
--| vice versa) and other generic 
--| functions and components
--| @todo A test bench for the functions should be created.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is

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
	generic(clock_frequency: positive; N: positive);
	port(
		clk:  in std_logic;
		rst:  in std_logic;
		stop: in std_logic);
	end component;

	component timer_us 
		generic(clock_frequency: positive; timer_period_us: natural);
		port(
			rst: in  std_logic;
			clk: in  std_logic;
			co:  out std_logic);
	end component;

	component timer_us_tb
		generic(clock_frequency: positive);
		port(
			clk:  in std_logic;
			rst:  in std_logic;
			stop: in std_logic);
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
		port(
			clk:  in std_logic;
			rst:  in std_logic;
			stop: in std_logic);
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
		port(
			clk:  in std_logic;
			rst:  in std_logic;
			stop: in std_logic);
	end component;

	function max(a: natural; b: natural) return natural;
	function min(a: natural; b: natural) return natural;
	function n_bits(x: natural) return natural;
	function n_bits(x: std_logic_vector) return natural;
	function reverse (a: in std_logic_vector) return std_logic_vector;
	function mux(a: std_logic_vector; b: std_logic_vector; sel: std_logic) return std_logic_vector;
	function invert(slv:std_logic_vector) return std_logic_vector;
	function parity(slv:std_logic_vector; even: boolean) return std_logic;
	function select_bit(indexed, selector: std_logic_vector) return std_logic;
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

	function mux(a: std_logic_vector; b: std_logic_vector; sel: std_logic) return std_logic_vector is
		variable m: std_logic_vector(a'range);
	begin
		if sel = '0' then m := a; else m := b; end if;
		return m;
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
		assert n_bits(indexed) = (selector'high + 1);
		for i in indexed'range loop
			if i = to_integer(unsigned(selector)) then
				z := indexed(i);
			end if;
		end loop;
		return z;
	end;
end;

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
end reg;

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
end shift_register;

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
	port(
		clk:  in std_logic;
		rst:  in std_logic;
		stop: in std_logic);
end shift_register_tb;

architecture behav of shift_register_tb is
	constant N: positive := 8;
	constant clock_period: time   :=  1000 ms / clock_frequency;
	signal we: std_logic := '0';
	signal di: std_logic := '0';
	signal do: std_logic := '0';
begin
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

		while stop = '0' loop
			assert do = '0' report "extra bit in shift register";
			wait for clock_period;
		end loop;
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
		rst:  in std_logic := 'X';
		clk:  in std_logic := 'X';
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
	port(
		clk:  in std_logic;
		rst:  in std_logic;
		stop: in std_logic);
end;

architecture behav of timer_us_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
	signal co: std_logic := 'X';
begin
	uut: entity work.timer_us
	generic map(clock_frequency => clock_frequency, timer_period_us => 1) port map(clk => clk, rst => rst, co => co);

	stimulus_process: process
	begin
		wait for 1 us;
		assert co = '0';
		wait for clock_period;
		assert co = '1';
		wait;
	end process;

end;

------------------------- Microsecond Timer -----------------------------------------

------------------------- Edge Detector ---------------------------------------------

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
	rising_edge_detector: process(clk,rst)
	begin
		if rst ='1' then
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
	port(
		clk:  in std_logic;
		rst:  in std_logic;
		stop: in std_logic);
end;

architecture behav of edge_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
	signal sin:    std_logic := '0';
	signal output: std_logic := 'X';
begin
	uut: entity work.edge
	port map(clk => clk, rst => rst, sin => sin, output => output);

	stimulus_process: process
	begin
		wait for clock_period * 5;
		assert output = '0';
		wait for clock_period;
		sin <= '1';
		wait for clock_period * 0.5;
		assert output = '1';
		wait for clock_period * 1.5;
		sin <= '0';
		assert output = '0';
		wait for clock_period;
		assert output = '0';
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
	port(
		clk:  in std_logic;
		rst:  in std_logic;
		stop: in std_logic);
end entity;

architecture behav of full_adder_tb is
	constant clock_period: time := 1000 ms / clock_frequency;
	signal x, y, z:    std_logic := '0';
	signal sum, carry: std_logic := '0';

	type stimulus_data   is array (7 downto 0) of std_logic_vector(2 downto 0);
	type stimulus_result is array (7 downto 0) of std_logic_vector(0 to     1);

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
begin
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
					" Expected: " & std_logic'image(result(i)(0)) & std_logic'image(result(i)(1));
			wait for clock_period;
		end loop;
		wait;
	end process;
end architecture;

------------------------- Full Adder ------------------------------------------------
