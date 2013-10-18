-------------------------------------------------------------------------------
--! @file spi_test_bench.vhd
--! @brief SPI test bench only for AD5641 interface.
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity spi_test_bench is
end entity;

architecture simulation of spi_test_bench is
    constant    clk_freq:     positive                        :=  1000000000;
    constant    clk_period:   time                            :=  1000 ms / clk_freq;

    signal      wait_flag:    std_logic                       :=  '0';
    signal      tb_clk:       std_logic                       :=  '0';
    signal      tb_rst:       std_logic                       :=  '1';
    signal      tb_idata_we:  std_logic                       :=  '0';
    signal      tb_idata:     std_logic_vector(15 downto 0)   := (others => '0');
    signal      tb_cs:        std_logic                       :=  'X';
    signal      tb_oclk:      std_logic                       :=  'X';
    signal      tb_odata:     std_logic                       :=  'X';
    signal      tb_done:      std_logic                       :=  'X';
begin

  spi_ad5641_uut: entity work.spi_ad5641
  port map(
    clk       => tb_clk,
    rst       => tb_rst,
    idata_we  => tb_idata_we,
    idata     => tb_idata,
    cs        => tb_cs,
    oclk      => tb_oclk,
    odata     => tb_odata,
    done      => tb_done
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
    wait for clk_period * 2;
    tb_rst <= '0';
    tb_idata <= X"8005";

    tb_idata_we <= '1';
    wait for clk_period * 1;
    tb_idata_we <= '0';

    wait until tb_done = '1';
    wait for clk_period * 4;

    tb_idata_we <= '1';
    wait for clk_period * 1;
    tb_idata_we <= '0';

    wait until tb_done = '1';
    tb_idata_we <= '1';
    wait for clk_period * 1;
    tb_idata_we <= '0';

    wait until tb_done = '1';
    wait for clk_period * 16;
    wait_flag   <=  '1';
    wait;
  end process;

end architecture;
