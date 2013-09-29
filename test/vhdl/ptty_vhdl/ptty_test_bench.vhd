-- PTTY testbench

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity ptty_test_bench is
end entity;

architecture simulation of ptty_test_bench is
  constant clk_freq:     positive                         :=  100000000;
  constant clk_period:   time                             :=  1000 ms / clk_freq;
  constant baud_rate:    positive                         :=  115200;

  signal tb_done:         std_logic                       :=  '0';
  signal tb_clk:          std_logic                       :=  '0';
  signal tb_rst:          std_logic                       :=  '1';
  signal tb_rx:           std_logic                       :=  '0';
  signal tb_tx:           std_logic                       :=  '1';
begin
  
  uart_loopback: entity work.loopback
  port map(
    CLOCK => tb_clk,
    RESET => tb_rst,
    RX    => tb_tx,
    TX    => tb_rx
          );

  ptty_uut: entity work.ptty
  generic map(
    clock_frequency => clk_freq,
    baud_rate       => baud_rate
  ) 
  port map(
    clk           => tb_clk,
    rst           => tb_rst,
    rx            => tb_rx,
    tx            => tb_tx,
    done          => tb_done
          );

	clk_process: process
	begin
    tb_rst <= '1';
      tb_clk	<=	'1';
      wait for clk_period/2;
      tb_clk	<=	'0';
      wait for clk_period/2;
    tb_rst <= '0';
    wait for clk_period/2;
    while tb_done = '0' loop
      tb_clk	<=	'1';
      wait for clk_period/2;
      tb_clk	<=	'0';
      wait for clk_period/2;
    end loop;
    wait;
	end process;

end architecture;
