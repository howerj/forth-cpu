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
  signal d:    signed(15 downto 0):= (others => '0'); 
  signal x_n:  signed(15 downto 0):= (others => '0');
  signal y_n:  signed(15 downto 0):= (others => '0');
  signal z_n:  signed(15 downto 0):= (others => '0');

  function arithmetic_shift_right(X: signed; Y: integer) return signed is
    variable tmp: signed(15 downto 0):=X;
  begin
    case Y is
      when 0 => -- do nothing
      when 1 =>  tmp := tmp(15) & tmp(15 downto 1);
      when 2 =>  tmp := tmp(15) & tmp(15) & tmp(15 downto 2); 
      when 3 =>  tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 3); 
      when 4 =>  tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 4); 
      when 5 =>  tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 5); 
      when 6 =>  tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 6); 
      when 7 =>  tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 7); 
      when 8 =>  tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 8); 
      when 9 =>  tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 9); 
      when 10 => tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 10); 
      when 11 => tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 11); 
      when 12 => tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 12); 
      when 13 => tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 13); 
      when 14 => tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 14); 
      when 15 => tmp := tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15) & tmp(15 downto 15); 
      when others =>
    end case;
    return tmp;
  end function;

begin
  d <= (others => '0') when zin(15) = '0' else (others => '1');

--  x_n <= xin - (((yin srl shiftr) xor d) - d);
--  y_n <= yin + (((xin srl shiftr) xor d) - d);
--  z_n <= zin - (((tval) xor d) - d);

  x_n <= xin - ((arithmetic_shift_right(yin,shiftr) xor d) - d);
  y_n <= yin + ((arithmetic_shift_right(xin,shiftr) xor d) - d);
  z_n <= zin - ((tval xor d) - d);

  xout <= x_n;
  yout <= y_n;
  zout <= z_n;
end architecture;
