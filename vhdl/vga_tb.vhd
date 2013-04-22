library ieee;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity vga_tb is
end vga_tb;

architecture behavior of vga_tb is
    constant    clk_freq:       positive                        :=  500000000;
    constant    clk_period:     time                            :=  1000 ms / clk_freq;

    signal      wait_flag:      std_logic                       :=  '0';
    signal      tb_clk:         std_logic                       :=  '0';
    signal      tb_rst:         std_logic                       :=  '0';

    signal    tb_R        :  std_logic;
    signal    tb_G        :  std_logic;
    signal    tb_B        :  std_logic;
    signal    tb_hsync    :  std_logic;
    signal    tb_vsync    :  std_logic;


begin
    vga_test_uut: entity work.vga80x40_test
    port map(
            reset => tb_rst,
            clk50MHz => tb_clk,
            R => tb_R,
            G => tb_G,
            B => tb_B,
            hsync => tb_hsync,
            vsync => tb_vsync
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
        wait for clk_period * 100000;
        wait_flag   <=  '1';
        wait;
    end process;


end architecture;
