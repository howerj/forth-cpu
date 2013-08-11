-- Arbitrary bit manipulation module, multiplexer
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com

-- TODO, use generators?

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
      case addr is
        when "0000" => output <= input(0); 
        when "0001" => output <= input(1); 
        when "0010" => output <= input(2); 
        when "0011" => output <= input(3); 
        when "0100" => output <= input(4); 
        when "0101" => output <= input(5); 
        when "0110" => output <= input(6); 
        when "0111" => output <= input(7); 
        when "1000" => output <= input(8); 
        when "1001" => output <= input(9); 
        when "1010" => output <= input(10); 
        when "1011" => output <= input(11); 
        when "1100" => output <= input(12); 
        when "1101" => output <= input(13); 
        when "1110" => output <= input(14); 
        when "1111" => output <= input(15); 
        when others => output <= '0';
      end case;
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
