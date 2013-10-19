------------------------------------------------------------------------------- 
--! @file abmm_multiplexer.vhd
--! @brief Arbitrary bit manipulation module, multiplexer
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity abmm_multiplexer is
  port(
    s:      in std_logic;                          -- Special function select
    c_bit:  in std_logic;                          -- Current bit

    input:  in std_logic_vector(15 downto 0);      -- Input vector
    addr:   in std_logic_vector(3 downto 0);       -- Bit address to select

    
    output: out std_logic                         -- output bit
      );
end;

architecture rtl of abmm_multiplexer is
begin
  process(s,c_bit,input,addr)
  begin
    if s = '0' then
      output <= input(to_integer(unsigned(addr)));
    elsif s = '1' then
      case addr is
        when "0000" => output <= '0';
        when "0001" => output <= '1';
        when "0010" => output <= not(c_bit);
        when others => output <= '0';
      end case;
    end if;
  end process;
end architecture;
