-- Richard James Howe
-- Simple testbench for the dual port RAM to be used.
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.rj.89@googlemail.com
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity mem_h2_tb is
end mem_h2_tb;

architecture behavior of mem_h2_tb is
    constant    clk_freq:       positive                        :=  1000000000;
    constant    clk_period:     time                            :=  1000 ms / clk_freq;

    signal      wait_flag:      std_logic                       :=  '0';
    signal      tb_clk:         std_logic                       :=  '0';

    signal      tb_a_dwe:         std_logic:= '0';
    signal      tb_a_addr:        std_logic_vector(15 downto 0)   := (others => '0');
    signal      tb_a_din:         std_logic_vector(15 downto 0)   := (others => '0');
    signal      tb_a_dout:        std_logic_vector(15 downto 0);

    signal      tb_b_dwe:         std_logic:= '0';
    signal      tb_b_addr:        std_logic_vector(15 downto 0)   := (others => '0');
    signal      tb_b_din:         std_logic_vector(15 downto 0)   := (others => '0');
    signal      tb_b_dout:        std_logic_vector(15 downto 0);
begin
    mem_h2_uut: entity work.mem_h2
    port map(
        a_clk => tb_clk,

        a_dwe => tb_a_dwe,
        a_addr => tb_a_addr,
        a_din => tb_a_din,
        a_dout => tb_a_dout,

        b_clk => tb_clk,
        b_dwe => tb_b_dwe,
        b_addr => tb_b_addr,
        b_din => tb_b_din,
        b_dout => tb_b_dout
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
        tb_b_dwe  <= '0';
        tb_a_dwe  <= '0';
        tb_a_addr <= X"0000";
        tb_b_addr <= X"0001";
        tb_b_din  <= X"0000";
        wait for clk_period * 10;
        tb_a_dwe  <='1';
        tb_a_din  <=X"5A5A";
        tb_a_addr <=X"000E";
        wait for clk_period * 1;
        tb_a_dwe  <='0';
        tb_a_addr <=X"000F";
        wait for clk_period * 1;
        tb_a_addr <=X"000E";
        wait for clk_period * 6;

        wait_flag   <=  '1';
        wait;
    end process;
end architecture;
