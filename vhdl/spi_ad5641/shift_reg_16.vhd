-- Example from http://www.cse.wustl.edu/~jbf/cse362.d/cse362.vhdl.Generate.pdf
-- Needs modification

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity shift_reg_16 is
  port(
    clk, rst, clr: in std_logic;

    sin_we:       in std_logic;
    sin_reg:      in std_logic_vector(15 downto 0);

    sout, done:   out std_logic
      );
end entity;

architecture rtl of shift_reg_16 is
  signal parallel_data: std_logic_vector(15 downto 0);
  signal sin_reg_c, sin_reg_n: std_logic_vector(15 downto 0);

  signal enable: std_logic;
  signal done_c, done_n: std_logic;

  signal count: std_logic_vector(15 downto 0);
  constant count_max: integer := 15;
  subtype counter_range is integer range 0 to count_max;
  signal counter_int: counter_range;
begin

  counter_process: process(clk, rst)
  begin
    if rst = '0' then
      counter_int <= 0;
    elsif rising_edge(clk) then
      if (counter_int = count_max) or (enable = '0') then
        counter_int <= 0;
      else
        counter_int <= counter_int + 1;
      end if;
    end if;
  end process;

  count <= std_logic_vector(to_unsigned(counter_int,count_max));

  clkReg: process(clk,rst,enable)
  begin
    if rst = '0' then
      sin_reg_c <= (others => '0');
      done_c <= '0';
    elsif rising_edge(clk) then
      sin_reg_c <= sin_reg_n;
      done_c <= done_n;
    end if;
  end process;

  mainProcess: process(
    sin_reg_c, sin_we, sin_reg,
    done_c
  )
  begin
    sin_reg_n <= sin_reg_c;
    done_n <= done_c;

    if done_c = '0' then
      enable <= '1';
    else
      enable <= '0';
    end if;

    if count = X"F" then
      done_n <= '1';
    end if;

    if sin_we = '1' and done_c = '1' then
      sin_reg_n <= sin_reg;
      done_n <= '0';
    end if;
  end process;


  shift_reg: for i in 1 to 16 generate
  begin
    dff_main: if (i >= 1)  and (i < 16) generate
    begin
      dff: entity work.edgeDff
         port map(clk,parallel_data(i-1),clr,enable,parallel_data(i));
    end generate;

    dff_end: if i = 16 generate
    begin
      dff: entity work.edgeDff
        port map(clk,parallel_data(i-1),clr,enable,sout);
    end generate;
  end generate shift_reg;
end architecture;
