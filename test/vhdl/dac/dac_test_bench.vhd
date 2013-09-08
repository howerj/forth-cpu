-- SPI AD5641 interface.

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity dac_test_bench is
end entity;

architecture simulation of dac_test_bench is
    constant    clk_freq:     positive                        :=  1000000000;
    constant    clk_period:   time                            :=  1000 ms / clk_freq;

    signal      wait_flag:    std_logic                       :=  '0';
    signal      tb_clk:       std_logic                       :=  '0';
    signal      tb_rst:       std_logic                       :=  '1';
begin

--  dac_uut: entity work.dac
--  port map(
--    clk       => tb_clk,
--    rst       => tb_rst,
--          );

	clk_process: process
	begin
    while wait_flag = '0' loop
      tb_clk	<=	'1';
      wait for clk_period/2;
      tb_clk	<=	'0';
      wait for clk_period/2;
    end loop;
    wait;
	end process;

	stimulus_process: process
	begin

    wait for clk_period * 16;
    wait_flag   <=  '1';
    wait;
  end process;

end architecture;
