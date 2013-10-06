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
use ieee.math_real.all;
use std.textio.all;

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

  --! Stringify a signed fixed point 2.14 format number
  function str_fix_s2_14(x: signed(15 downto 0)) return string is
   variable scaling_factor: real := 16384.000000;
  begin
      return real'image(real(to_integer(x))/scaling_factor);
  end function;

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
        --! angle.binary: a list of angles in signed 2.14 fixed point format
        file     in_file:         text is in "angle.binary";
        variable input_line:      line;
        variable tmp_var:         bit_vector(15 downto 0);
	begin
      wait for clk_period;
      while not endfile(in_file) loop
        readline(in_file,input_line);
        read(input_line,tmp_var);
        tb_ang_in <= signed(to_stdlogicvector(tmp_var));
        --! report angle input, sine output, cosine output
        report "ang " & str_fix_s2_14(signed(to_stdlogicvector(tmp_var))) & " sin " & str_fix_s2_14(tb_sin_out) & " cos " & str_fix_s2_14(tb_cos_out);
        wait for clk_period;
      end loop;

    wait_flag   <=  '1';
    wait;
  end process;

end architecture;

