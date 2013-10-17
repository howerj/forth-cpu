-------------------------------------------------------------------------------
--! @file queue_test_bench.vhd
--! @brief queue.vhd test bench
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity queue_test_bench is
end entity;

architecture simulation of queue_test_bench is
  constant clk_freq:      positive                        :=   1000000000;
  constant tb_bufxsignals:positive                        :=   4;
  constant tb_queuelen:   positive                        :=   4;
  constant clk_period:    time                            :=   1000 ms / clk_freq;

  signal   wait_flag:     std_logic                       :=  '0';
  signal   tb_clk:        std_logic                       :=  '0';
  signal   tb_rst:        std_logic                       :=  '1';

  signal   tb_unbuf:      std_logic_vector(tb_bufxsignals downto 0):= (others => '0');
  signal   tb_buf:        std_logic_vector(tb_bufxsignals downto 0):= (others => '0');
  signal   tb_flush:      std_logic                       :=  'X';
  signal   tb_full:       std_logic                       :=  '0';
begin

  queue_uut: entity work.queue
  generic map(
    bufxsignals => tb_bufxsignals
  )
  port map(
    clk         => tb_clk,
    rst         => tb_rst,
    flush       => tb_flush,
    full        => tb_full,
    unbuf       => tb_unbuf,
    buf         => tb_buf
          );

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
    tb_rst        <= '1';
    wait for clk_period;
    tb_rst        <= '0';
    wait for clk_period * 63;
    wait_flag <= '1';
    wait;
  end process;

end architecture;
