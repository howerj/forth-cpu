library ieee,work,std;
use ieee.std_logic_1164.all; 

entity edgeDff is
  port(
    clk:  in std_logic;
    d:    in std_logic;
    clr:  in std_logic;
    en:   in std_logic;
    q:    out std_logic
  );
end entity;
  
architecture rtl of edgeDff is
begin
  state_change: process(clk, clr) is
  begin
    if clr = '1' then
      q <= '0';
    elsif rising_edge(clk) and en = '1' then
      q <= d;
    end if;
  end process state_change;
end architecture;
