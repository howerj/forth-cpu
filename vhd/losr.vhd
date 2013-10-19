-------------------------------------------------------------------------------
--! @file losr.vhd
--! @brief Shift register N-bit, asynchronous reset, synchronous load 
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
  generic 
  (
    N : integer := 4
  );
  port 
  (
    reset : in  std_logic;
    clk   : in  std_logic;
    load  : in  std_logic;
    ce    : in  std_logic;
    do    : out std_logic := '0';
    di    : in  std_logic_vector(N-1 downto 0)
  );
end losr;

architecture arch of losr is
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
end arch;
