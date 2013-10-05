-------------------------------------------------------------------------------
--! @file shift_reg_test_bench.vhd
--! @brief Shift Register test bench only
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity shift_reg_test_bench is
end entity;

architecture simulation of shift_reg_test_bench is
    constant    clk_freq:     positive                        :=  1000000000;
    constant    clk_period:   time                            :=  1000 ms / clk_freq;

    signal      wait_flag:    std_logic                       :=  '0';
    signal      tb_clk:       std_logic                       :=  '0';
    signal      tb_rst:       std_logic                       :=  '1';
    signal      tb_enable:    std_logic                       :=  '0';

    signal      tb_sin:       std_logic_vector(15 downto 0)   := (others => '0');
    signal      tb_sout:      std_logic                       := '0';
    signal      tb_done:      std_logic                       := '0';
begin

  shift_reg_test_bench_uut: entity work.shift_reg_16
  port map(
    clk     => tb_clk,
    rst     => tb_rst,
    enable  => tb_enable,
    sin     => tb_sin,
    sout    => tb_sout,
    done    => tb_done
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
    tb_sin  <= X"8001";
    wait for clk_period * 2;
    tb_rst <= '0';
    tb_enable <= '1';
    wait for clk_period * 256;
    wait_flag   <=  '1';
    wait;
  end process;

end architecture;
