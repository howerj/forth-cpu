-------------------------------------------------------------------------------
--! @file ledseg_tb.vhd
--! @brief led test bench
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ledseg_tb is
end ledseg_tb;

architecture testing of ledseg_tb is
  constant clk_freq: positive :=  1000000000;
  constant clk_period: time   :=  1000 ms / clk_freq;
  constant number_of_segments: positive :=  4;

  signal  wait_flag: std_logic :=  '0';

  signal clk_tb:  std_logic;
  signal rst_tb:  std_logic;

  signal led_0_1_tb:     std_logic_vector(15 downto 0); 
  signal led_2_3_tb:     std_logic_vector(15 downto 0);
  signal led_0_1_we_tb:  std_logic;
  signal led_2_3_we_tb:  std_logic;
  signal an_tb:          std_logic_vector(3 downto 0);
  signal ka_tb:          std_logic_vector(7 downto 0);

  signal  tb_clk100MHz:   std_logic :=  '0';
  signal  tb_rst:         std_logic;
begin
---- Units under test ----------------------------------------------------------

  ledseg_uut: entity work.ledseg
  generic map(
    number_of_segments => number_of_segments,        -- not used at the moment
    clk_freq => clk_freq  -- --""--
  )
  port map(
    clk => clk_tb,
    rst => rst_tb,
    led_0_1 => led_0_1_tb,
    led_2_3 => led_2_3_tb,
    led_0_1_we => led_0_1_we_tb,
    led_2_3_we => led_2_3_we_tb,
    an => an_tb,
    ka => ka_tb
  );


------ Simulation only processes ----------------------------------------------
  clk_process: process
  begin
    while wait_flag = '0' loop
      clk_tb  <=  '1';
      wait for clk_period/2;
      clk_tb  <=  '0';
      wait for clk_period/2;
    end loop;
    wait;
  end process;

  stimulus_process: process
  begin
    tb_rst <= '1';
    wait for clk_period * 2;
    tb_rst <= '0';
    wait for clk_period * 255;
    wait_flag   <=  '1';
    wait;
  end process;
------ END ---------------------------------------------------------------------
end architecture;

