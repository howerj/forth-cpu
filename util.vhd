-------------------------------------------------------------------------------
--! @file util.vhd
--! @brief          A collection of utilities and simple components,
--!                 generic, reusable components are listed here, they
--!                 should also be entirely self contained and ready
--!                 to use by themselves.
--! @author         Richard James Howe
--! @copyright      Copyright 2014 Richad James Howe
--! @license        LGPL version 3
--! @email          howe.r.j.89@gmail.com
--!
--! Notes:
--! * Two subsections have been written by Javier Valcarce García as part of
--! VGA driver, which are labeled. 
--!
--! @todo split this file up
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is

	--- Component definitions ---------------------------------------------------
	component gptimer is
	generic(gptimerbits: positive := 16);
	port(
		clk:          in std_logic;
		rst:          in std_logic;

		--! Write enables
		ctrin_we:     in std_logic;                     -- write enable

		--! Control register
		ctrin:        in std_logic_vector(gptimerbits - 1 downto 0); 

		-- Timer interrupts
		irq:          out std_logic;                    -- Compare Interrupt
		Q:            out std_logic;                    -- Timer signal
		NQ:           out std_logic                     -- Timer signal inverted
	);
	end component;

	component losr 
	generic (N : integer := 4);
	port 
	(
		reset : in  std_logic;
		clk   : in  std_logic;
		load  : in  std_logic;
		ce    : in  std_logic;
		do    : out std_logic := '0';
		di    : in  std_logic_vector(N-1 downto 0)
	);
	end component;

	component ctrm
	generic (M : integer := 8);
	port (
		reset : in  std_logic;              -- asynchronous reset
		clk   : in  std_logic;
		ce    : in  std_logic;              -- enable counting
		rs    : in  std_logic;              -- synchronous reset
		do    : out integer range (M-1) downto 0 := 0
	);
	end component;
	-----------------------------------------------------------------------------

end util;

-------------------------------------------------------------------------------
--! @file util.vhd
--! @brief Shift register N-bit, asynchronous reset, synchronous load,
--!        (losr.vhd, original file name)
--! and enable
--! @author         Javier Valcarce García
--! @copyright      Copyright 2007 Javier Valcarce García
--! @license        LGPL version 3
--! @email          javier.valcarce@gmail.com
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity losr is
	generic (N : integer := 4);
	port 
	(
		reset : in  std_logic;
		clk   : in  std_logic;
		load  : in  std_logic;
		ce    : in  std_logic;
		do    : out std_logic := '0';
		di    : in  std_logic_vector(N-1 downto 0));
end losr;

architecture behav of losr is
begin

	process(reset, clk)
	variable data : std_logic_vector(N-1 downto 0):= (others => '0');
	begin
	if reset = '1' then
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
end behav;

-------------------------------------------------------------------------------
--! @file ctrm.vhd
--! @brief Counter, asynchronous *and* synchronous reset, up only.
--!        (ctrm.vhd, original filename)
--! @author         Javier Valcarce García
--! @copyright      Copyright 2007 Javier Valcarce García
--! @license        LGPL version 3
--! @email          javier.valcarce@gmail.com
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ctrm is
	generic (M : integer := 8);
	port (
		reset : in  std_logic;              -- asynchronous reset
		clk   : in  std_logic;
		ce    : in  std_logic;              -- enable counting
		rs    : in  std_logic;              -- synchronous reset
		do    : out integer range (M-1) downto 0 := 0
	);
end ctrm;

architecture behav of ctrm is
	signal c : integer range (M-1) downto 0:= 0;
begin
	do <= c;
	process(reset, clk)
	begin
		if reset = '1' then
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
end behav;

