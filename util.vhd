-------------------------------------------------------------------------------
--! @file util.vhd
--! @brief          A collection of utilities and simple components,
--!                 generic, reusable components are listed here, they
--!                 should also be entirely self contained and ready
--!                 to use by themselves.
--! @author         Richard James Howe
--! @copyright      Copyright 2017 Richard James Howe
--! @license        MIT
--! @email          howe.r.j.89@gmail.com
--!
--! @todo Make these components type generic
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is

	--- Component definitions ---------------------------------------------------
	component reg
	generic(N: positive := 8);
	port
	(
		clk: in  std_logic;
		rst: in  std_logic;
		we:  in  std_logic;
		di:  in  std_logic_vector(N-1 downto 0);
		do:  out std_logic_vector(N-1 downto 0));
	end component;

	component breg
	port
	(
		clk: in  std_logic;
		rst: in  std_logic;
		we:  in  std_logic;
		di:  in  std_logic;
		do:  out std_logic);
	end component;

	-----------------------------------------------------------------------------

end util;

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

architecture behav of reg is
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
end behav;

------------------------- Generic Register of std_logic -----------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity breg is
	generic(N: positive := 8);
	port
	(
		clk: in  std_logic;
		rst: in  std_logic;
		we:  in  std_logic;
		di:  in  std_logic;
		do:  out std_logic);
end breg;

architecture behav of breg is
	signal r_c, r_n : std_logic := '0';
begin
	do <= r_c;

	process(rst, clk)
	begin
		if rst = '1' then
			r_c <= '0';
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
end behav;


