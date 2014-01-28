-------------------------------------------------------------------------------
--! @file gptimer_test_bench.vhd
--! @brief gptimer.vhd test bench
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity gptimer_test_bench is
end entity;

architecture simulation of gptimer_test_bench is
  constant clk_freq:     positive  :=  1000000000;
  constant clk_period:   time      :=  1000 ms / clk_freq;

  signal   wait_flag:    std_logic :=  '0';
  signal   tb_clk:       std_logic :=  '0';
  signal   tb_rst:       std_logic :=  '1';

  signal   tb_ctrin_we:  std_logic :=  '0';
  signal   tb_compin_we: std_logic :=  '0';

  signal   tb_ctrin:     std_logic_vector(15 downto 0) := (others =>'0'); 
  signal   tb_compin:    std_logic_vector(15 downto 0) := (others =>'0'); 

  signal   tb_irq:       std_logic := 'X';
  signal   tb_q:         std_logic := 'X';                     
  signal   tb_nq:        std_logic := 'X';                     
begin

  gpt_timer_uut: entity work.gptimer
  port map(
    clk       => tb_clk,
    rst       => tb_rst,
    
    ctrin_we  => tb_ctrin_we,
    ctrin     => tb_ctrin,

    irq       => tb_irq,
    q         => tb_q,
    nq        => tb_nq
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
    tb_rst    <= '1';
    wait for clk_period;
    tb_rst    <= '0';

    tb_ctrin_we <= '1';
    tb_ctrin <= X"A008";
    wait for clk_period;
    tb_ctrin_we <= '0';
    wait for clk_period * 24;

    tb_ctrin_we <= '1';
    tb_ctrin <= X"E000";
    wait for clk_period;
    tb_ctrin_we <= '0';
    wait for clk_period * 8;


    tb_ctrin_we <= '1';
    tb_ctrin <= X"E001";
    wait for clk_period;
    tb_ctrin_we <= '0';
    wait for clk_period * 8;


    wait_flag <= '1';
    wait;
  end process;

end architecture;
