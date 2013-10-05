--------------------------------------------------------------------------------
--! @file dac_test_bench.vhd
--! @brief SPI AD5641 DAC interface test bench
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity dac_test_bench is
end entity;

architecture simulation of dac_test_bench is
  constant clk_freq:     positive                        :=  1000000000;
  constant clk_period:   time                            :=  1000 ms / clk_freq;

  signal wait_flag:       std_logic                       :=  '0';
  signal tb_clk:          std_logic                       :=  '0';
  signal tb_clk25MHz:     std_logic                       :=  '0';
  signal tb_rst:          std_logic                       :=  '1';

  signal tb_ctr_r_we:     std_logic := '0';              -- ctr_r write enable
  signal tb_comp1_r_we:   std_logic := '0';              -- comp1_r write enable
  signal tb_load1_r_we:   std_logic := '0';              -- load1_r write enable
  signal tb_load_s_we:    std_logic := '0';              -- load1_r write enable
  signal tb_direct_r_we:  std_logic := '0';              -- direct_r write enable
  signal tb_ram_s_we:     std_logic := '0';              -- ram_s write enable
  signal tb_ctr_r:        std_logic_vector(15 downto 0) := (others => '0'); -- Control register
  signal tb_comp1_r:      std_logic_vector(12 downto 0) := (others => '0'); -- Compare value one
  signal tb_load1_r:      std_logic_vector(12 downto 0) := (others => '0'); -- Compare value two
  signal tb_load_s:       std_logic_vector(12 downto 0) := (others => '0'); -- Compare value two
  signal tb_direct_r:     std_logic_vector(15 downto 0) := (others => '0'); -- Load DAV value directly
  signal tb_ram_s_addr:   std_logic_vector(12 downto 0) := (others => '0'); -- DAC RAM Address
  signal tb_ram_s_data_i: std_logic_vector(15 downto 0) := (others => '0'); -- DAC RAM Data (Input)
  signal tb_ram_s_data_o: std_logic_vector(15 downto 0) := (others => 'X'); -- DAC RAM Data (Output)
  signal tb_irq_comp1:    std_logic;                     -- Compare one Interrupt

  signal tb_cs, tb_oclk, tb_odata, tb_done: std_logic := 'X';   -- SPI, output only

begin

  dac_uut: entity work.dac
  port map(
    clk           => tb_clk,
    clk25MHz      => tb_clk25MHz,
    rst           => tb_rst,

    ctr_r_we      => tb_ctr_r_we,
    comp1_r_we    => tb_comp1_r_we,
    load1_r_we    => tb_load1_r_we,
    load_s_we     => tb_load_s_we,
    direct_r_we   => tb_direct_r_we,
    ram_s_we      => tb_ram_s_we,

    ctr_r         => tb_ctr_r,
    comp1_r       => tb_comp1_r,
    load1_r       => tb_load1_r,
    load_s        => tb_load_s,
    direct_r      => tb_direct_r,
    ram_s_addr    => tb_ram_s_addr,
    ram_s_data_i  => tb_ram_s_data_i,
    ram_s_data_o  => tb_ram_s_data_o,

    irq_comp1     => tb_irq_comp1,

    cs            => tb_cs ,
    oclk          => tb_oclk,
    odata         => tb_odata,
    done          => tb_done
          );


	clk25MHz_process: process
	begin
    while wait_flag = '0' loop
      tb_clk25MHz	<=	'1';
      wait for (clk_period*4)/2;
      tb_clk25MHz	<=	'0';
      wait for (clk_period*4)/2;
    end loop;
    wait;
	end process;

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
    tb_rst <= '1';
    wait for clk_period * 1;
    tb_rst <= '0';
    tb_ctr_r_we <= '1';
    tb_ctr_r    <= X"8000";
    wait for clk_period * 1;
    tb_ctr_r_we <= '0';

    wait for clk_period * 9000; 
--    wait for clk_period * 9000 * 16 * 16;
    wait_flag   <=  '1';
    wait;
  end process;

end architecture;
