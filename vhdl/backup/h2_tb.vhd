-- Simple test bench for the processor core.
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com

library ieee;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity h2_tb is
end h2_tb;

architecture behavior of h2_tb is
    constant    clk_freq:       positive                        :=  1000000000;
    constant    clk_period:     time                            :=  1000 ms / clk_freq;

    signal      wait_flag:      std_logic                       :=  '0';
    signal      tb_clk:         std_logic                       :=  '0';
    signal      tb_rst:         std_logic;

    -- IO interface.
    signal      tb_io_wr:         std_logic;
    signal      tb_io_din:        std_logic_vector(15 downto 0)    :=  (others => '0');
    signal      tb_io_dout:       std_logic_vector(15 downto 0);
    signal      tb_io_daddr:      std_logic_vector(15 downto 0);

    -- RAM interface.
    signal      tb_pco:         std_logic_vector(15 downto 0);
    signal      tb_insn:        std_logic_vector(15 downto 0)   :=  (0 => '1', others => '0');
    signal      tb_dwe:         std_logic;
    signal      tb_din:         std_logic_vector(15 downto 0)   :=  (others => '0');
    signal      tb_dout:        std_logic_vector(15 downto 0);
    signal      tb_daddr:       std_logic_vector(15 downto 0);

begin
    h2_uut: entity work.h2
    port map(
        clk     =>  tb_clk,
        rst     =>  tb_rst,
        io_wr   =>  tb_io_wr,
        io_din  =>  tb_io_din,
        io_dout =>  tb_io_dout,
        io_daddr=>  tb_io_daddr,
        pco     =>  tb_pco,
        insn    =>  tb_insn,
        dwe     =>  tb_dwe,
        din     =>  tb_din,
        dout    =>  tb_dout,
        daddr   =>  tb_daddr
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
        tb_rst <= '1';
        wait for clk_period * 10;
        tb_rst <= '0';
        wait for clk_period * 10;
        tb_insn <=  "1000000000000010";
        wait for clk_period * 2;
        tb_insn <=  "0110001000000010";
        wait for clk_period * 1;


--        tb_rst <= '1';
--        wait for clk_period * 10;
--        tb_rst <= '0';
--        wait for clk_period * 10;
--        tb_insn <=  "0110000000000000";
--        wait for clk_period * 1;
--        tb_insn <=  "1000000000000111";
--        wait for clk_period * 15;
--        tb_insn <=  "0110001000000010";
--        wait for clk_period * 60;



        wait_flag   <=  '1';
        wait;
    end process;


end architecture;

