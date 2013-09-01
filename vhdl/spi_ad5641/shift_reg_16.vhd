-- Example from http://www.cse.wustl.edu/~jbf/cse362.d/cse362.vhdl.Generate.pdf
-- Needs modification

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity shift_reg_16 is
  port(
    clk, rst, enable: in std_logic;

    sin:      in std_logic_vector(15 downto 0);

    sout, done:   out std_logic
      );
end entity;

architecture rtl of shift_reg_16 is
  constant count_max: integer := 15;
  subtype counter_range is integer range 0 to count_max;
  signal counter_int: counter_range;
begin

  counter_process: process(clk, rst)
  begin
    if rst = '1' then
      counter_int <= 15;
    elsif rising_edge(clk) then
      if enable = '1' then
        if counter_int = 0 then
          counter_int <= 15;
        else
          counter_int <= counter_int - 1;
        end if;
      end if;
    end if;
  end process;

  sout <= sin(counter_int);
  done <= '1' when counter_int = 0 else '0';

end architecture;
