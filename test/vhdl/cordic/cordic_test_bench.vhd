-------------------------------------------------------------------------------
--! @file cordic_test_bench.vhd
--! @brief CORDIC test bench only.
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity cordic_test_bench is
end entity;

architecture simulation of cordic_test_bench is
  constant clk_freq:     positive                         :=  1000000000;
  constant clk_period:   time                             :=  1000 ms / clk_freq;

  signal wait_flag:       std_logic                       :=  '0';
  signal tb_clk:          std_logic                       :=  '0';
  signal tb_rst:          std_logic                       :=  '1';

  signal tb_sin_in:       signed(15 downto 0)             := (others => '0');
  signal tb_cos_in:       signed(15 downto 0)             := X"26DD";
  signal tb_ang_in:       signed(15 downto 0)             := (others => '0');

  signal tb_sin_out:      signed(15 downto 0);
  signal tb_cos_out:      signed(15 downto 0);
  signal tb_ang_out:      signed(15 downto 0);
begin

  cordic_uut: entity work.cordic
  port map(
    clk     => tb_clk,
    rst     => tb_rst,

    sin_in  => tb_sin_in,
    cos_in  => tb_cos_in,
    ang_in  => tb_ang_in,

    sin_out => tb_sin_out,
    cos_out => tb_cos_out,
    ang_out => tb_ang_out
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
      tb_ang_in <= X"0000"; wait for clk_period;
      tb_ang_in <= X"0202"; wait for clk_period;
      tb_ang_in <= X"0405"; wait for clk_period;
      tb_ang_in <= X"0608"; wait for clk_period;
      tb_ang_in <= X"080A"; wait for clk_period;
      tb_ang_in <= X"0A0D"; wait for clk_period;
      tb_ang_in <= X"0C10"; wait for clk_period;
      tb_ang_in <= X"0E13"; wait for clk_period;
      tb_ang_in <= X"1015"; wait for clk_period;
      tb_ang_in <= X"1218"; wait for clk_period;
      tb_ang_in <= X"141B"; wait for clk_period;
      tb_ang_in <= X"161D"; wait for clk_period;
      tb_ang_in <= X"1820"; wait for clk_period;
      tb_ang_in <= X"1A23"; wait for clk_period;
      tb_ang_in <= X"1C26"; wait for clk_period;
      tb_ang_in <= X"1E28"; wait for clk_period;
      tb_ang_in <= X"202B"; wait for clk_period;
      tb_ang_in <= X"222E"; wait for clk_period;
      tb_ang_in <= X"2430"; wait for clk_period;
      tb_ang_in <= X"2633"; wait for clk_period;
      tb_ang_in <= X"2836"; wait for clk_period;
      tb_ang_in <= X"2A39"; wait for clk_period;
      tb_ang_in <= X"2C3B"; wait for clk_period;
      tb_ang_in <= X"2E3E"; wait for clk_period;
      tb_ang_in <= X"3041"; wait for clk_period;
      tb_ang_in <= X"3243"; wait for clk_period;
      tb_ang_in <= X"3446"; wait for clk_period;
      tb_ang_in <= X"3649"; wait for clk_period;
      tb_ang_in <= X"384C"; wait for clk_period;
      tb_ang_in <= X"3A4E"; wait for clk_period;
      tb_ang_in <= X"3C51"; wait for clk_period;
      tb_ang_in <= X"3E54"; wait for clk_period;
      tb_ang_in <= X"4056"; wait for clk_period;
      tb_ang_in <= X"4259"; wait for clk_period;
      tb_ang_in <= X"445C"; wait for clk_period;
      tb_ang_in <= X"465F"; wait for clk_period;
      tb_ang_in <= X"4861"; wait for clk_period;
      tb_ang_in <= X"4A64"; wait for clk_period;
      tb_ang_in <= X"4C67"; wait for clk_period;
      tb_ang_in <= X"4E6A"; wait for clk_period;
      tb_ang_in <= X"506C"; wait for clk_period;
      tb_ang_in <= X"526F"; wait for clk_period;
      tb_ang_in <= X"5472"; wait for clk_period;
      tb_ang_in <= X"5674"; wait for clk_period;
      tb_ang_in <= X"5877"; wait for clk_period;
      tb_ang_in <= X"5A7A"; wait for clk_period;
      tb_ang_in <= X"5C7D"; wait for clk_period;
      tb_ang_in <= X"5E7F"; wait for clk_period;
      tb_ang_in <= X"6082"; wait for clk_period;
      tb_ang_in <= X"6285"; wait for clk_period;


    wait_flag   <=  '1';
    wait;
  end process;

end architecture;

