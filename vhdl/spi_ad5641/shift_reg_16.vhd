-- Example from http://www.cse.wustl.edu/~jbf/cse362.d/cse362.vhdl.Generate.pdf
-- Needs modification

library ieee,work,std;
use ieee.std_logic_1164.all; 

entity shift_reg_16 is
  port(
    sin, clk, en, clr: in std_logic;
    sout: out std_logic
      );
end entity;

architecture rtl of shift_reg_16 is
  signal parallel_data: std_logic_vector(1 to 16);

begin
  shift_reg: for i in 1 to 16 generate
  begin

    dff_left: if i = 1 generate 
    begin
      dff: entity work.edgeDff
        port map(clk,sin,clr,en,parallel_data(i));
    end generate;

    dff_others: if (i>1) and (i<16) generate
      dff: entity work.edgeDff
        port map(clk,parallel_data(i-1),clr,en,parallel_data(i));
    end generate;

    dff_right: if i = 32 generate
      dff: entity work.edgeDff
        port map(clk,parallel_data(i-1),clr,en,sout);
    end generate;
  end generate shift_reg;

end architecture;
