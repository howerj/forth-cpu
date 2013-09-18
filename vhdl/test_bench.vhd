-- Richard James Howe
--
-- Main test bench. This is a simple test bench to
-- test the entire system, I/O is controlled here,
-- the program that is loaded into the CPU is in
-- the RAM module "mem_h2.vhd".
--
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_bench is
end test_bench;

architecture behavior of test_bench is
    constant    clk_freq:        positive                     :=  1000000000;
    constant    clk_period:      time                         :=  1000 ms / clk_freq;

    signal      wait_flag:       std_logic                    :=  '0';
    signal      tb_sim_irq:      std_logic                    :=  '0';
    signal      tb_sim_irc:      std_logic_vector(3 downto 0) := (others => '0');
    signal      tb_clk:          std_logic                    :=  '0';
    signal      tb_rst:          std_logic;

    signal      tb_btnu:         std_logic                    :=            '0';  -- button up
    signal      tb_btnd:         std_logic                    :=            '0';  -- button down
    signal      tb_btnc:         std_logic                    :=            '0';  -- button centre
    signal      tb_btnl:         std_logic                    :=            '0';  -- button left
    signal      tb_btnr:         std_logic                    :=            '0';  -- button right
    signal      tb_sw:           std_logic_vector(7 downto 0) := (others => '0'); -- switches
    signal      tb_an:           std_logic_vector(3 downto 0) := (others => '0'); -- anodes   7 segment display
    signal      tb_ka:           std_logic_vector(7 downto 0) := (others => '0'); -- kathodes 7 segment display
    signal      tb_ld:           std_logic_vector(7 downto 0) := (others => '0'); -- leds
    signal      tb_rx:           std_logic                    :=            '0';  -- uart rx 
    signal      tb_tx:           std_logic                    :=            '0';  -- uart tx
    signal      tb_red:          std_logic_vector(2 downto 0) := (others => '0');
    signal      tb_green:        std_logic_vector(2 downto 0) := (others => '0');
    signal      tb_blue:         std_logic_vector(1 downto 0) := (others => '0');
    signal      tb_hsync:        std_logic                    :=            '0';
    signal      tb_vsync:        std_logic                    :=            '0';

begin
---- Units under test ----------------------------------------------------------

    top_level_uut: entity work.top_level
    port map(
      -- Remove me{{
        sim_irq => tb_sim_irq,
        sim_irc => tb_sim_irc,
      -- }}
        clk => tb_clk,
        btnu => tb_btnu,
        btnd => tb_btnd,
        btnc => tb_btnc,
        btnl => tb_btnl,
        btnr => tb_btnr,
        sw => tb_sw,
        an => tb_an,
        ka => tb_ka,
        ld => tb_ld,
        rx => tb_rx,
        tx => tb_tx,
        red => tb_red,
        green => tb_green,
        blue => tb_blue,
        hsync => tb_hsync,
        vsync => tb_vsync
            );

------ Simulation only processes ----------------------------------------------
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

    -- I/O settings go here.
	stimulus_process: process
	begin
        tb_sw <= X"A5";
        tb_rst <= '1';
        wait for clk_period * 2;
        tb_rst <= '0';
        wait for clk_period * 256;
        tb_sim_irq <= '1';
        wait for clk_period * 1;
        tb_sim_irq <= '0';
        wait for clk_period * 256;
        wait_flag   <=  '1';
        wait;
    end process;
------ END ---------------------------------------------------------------------
end architecture;

