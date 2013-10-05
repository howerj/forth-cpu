-------------------------------------------------------------------------------
--! @file cordic_iteration.vhd
--! @brief CORDIC single iteration
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cordic_iteration is
  generic(
    shiftr:     in integer range 0 to 15
         );
  port(
    tval:       in  signed(15 downto 0); -- ROM value for current interation

    xin:        in  signed(15 downto 0);
    yin:        in  signed(15 downto 0);
    zin:        in  signed(15 downto 0);

    xout:       out signed(15 downto 0);
    yout:       out signed(15 downto 0);
    zout:       out signed(15 downto 0)
  );
end entity;

architecture behav of cordic_iteration is
  signal d:   signed(15 downto 0):= (others => '1'); -- -1
  signal x_n: signed(15 downto 0):= (others => '0');
  signal y_n: signed(15 downto 0):= (others => '0');
  signal z_n: signed(15 downto 0):= (others => '0');
begin
  d <= (others => '0') when zin(15) = '0' else (others => '1');

  x_n <= xin - (((yin srl shiftr) xor d) - d);
  y_n <= yin + (((xin srl shiftr) xor d) - d);
  z_n <= zin - (((tval srl shiftr) xor d) - d);

  xout <= x_n;
  yout <= y_n;
  zout <= z_n;
end architecture;
