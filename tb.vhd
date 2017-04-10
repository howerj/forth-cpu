-------------------------------------------------------------------------------
--! @file tb.vhd
--! @brief Main test bench.
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        MIT
--! @email          howe.r.j.89@gmail.com
--!
--! @todo Optionally read expected inputs and commands from a file;
--! Expected results could be read in from a file along with the signal
--! they are supposed to check, although this should be done optionally,
--!
-------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb is
end tb;

architecture testing of tb is
	constant clk_freq: positive :=  1000000000;
	constant number_of_interrupts: positive := 8;
	constant uart_baud_rate:       positive := 115200;
	constant number_of_iterations: positive := 256;

	constant clk_period: time   :=  1000 ms / clk_freq;

	signal  wait_flag:       std_logic                    :=  '0';
	signal  debug_irq:       std_logic                    :=  '0';
	signal  debug_irc:       std_logic_vector(3 downto 0) := (others => '0');
	signal  debug_pc:        std_logic_vector(12 downto 0);
	signal  debug_insn:      std_logic_vector(15 downto 0);
	signal  debug_mem_dwe:   std_logic := '0';
	signal  debug_mem_din:   std_logic_vector(15 downto 0);
	signal  debug_mem_dout:  std_logic_vector(15 downto 0);
	signal  debug_mem_daddr: std_logic_vector(12 downto 0);

	signal  clk:   std_logic :=  '0';
	signal  rst:   std_logic;

--  signal  cpu_wait: std_logic := '0'; -- CPU wait flag

	-- Basic I/O
	signal  btnu:  std_logic := '0';  -- button up
	signal  btnd:  std_logic := '0';  -- button down
	signal  btnc:  std_logic := '0';  -- button centre
	signal  btnl:  std_logic := '0';  -- button left
	signal  btnr:  std_logic := '0';  -- button right
	signal  sw:    std_logic_vector(7 downto 0) := (others => '0'); -- switches
	signal  an:    std_logic_vector(3 downto 0) := (others => '0'); -- anodes   7 segment display
	signal  ka:    std_logic_vector(7 downto 0) := (others => '0'); -- kathodes 7 segment display
	signal  ld:    std_logic_vector(7 downto 0) := (others => '0'); -- leds

	-- UART
	signal  rx:    std_logic := '0';  -- uart rx
	signal  tx:    std_logic := '0';  -- uart tx

	-- VGA
	signal  red:   std_logic_vector(2 downto 0) := (others => '0');
	signal  green: std_logic_vector(2 downto 0) := (others => '0');
	signal  blue:  std_logic_vector(1 downto 0) := (others => '0');
	signal  hsync: std_logic := '0';
	signal  vsync: std_logic := '0';

	-- HID
	signal  ps2_keyboard_data: std_logic := '0';
	signal  ps2_keyboard_clk:  std_logic := '0';
--  signal  ps2_mouse_data:    std_logic := '0';
--  signal  ps2_mouse_clk:     std_logic := '0';
--  signal  pic_gpio:          std_logic_vector(1 downto 0):= (others => 'X');
begin
---- Units under test ----------------------------------------------------------

	uut: entity work.top
	generic map(
		clock_frequency => clk_freq,
		number_of_interrupts => number_of_interrupts,
		uart_baud_rate => uart_baud_rate)
	port map(
		debug_irq       => debug_irq,
		debug_irc       => debug_irc,
		debug_pc        => debug_pc,
		debug_insn      => debug_insn,
		debug_mem_dwe   => debug_mem_dwe,
		debug_mem_din   => debug_mem_din,
		debug_mem_dout  => debug_mem_dout,
		debug_mem_daddr => debug_mem_daddr,
		clk => clk,
		btnu => btnu,
		btnd => btnd,
		btnc => btnc,
		btnl => btnl,
		btnr => btnr,
		sw => sw,
		an => an,
		ka => ka,
		ld => ld,
		rx => rx,
		tx => tx,
		red => red,

		green => green,
		blue  => blue,
		hsync => hsync,
		vsync => vsync,

		ps2_keyboard_data => ps2_keyboard_data,
		ps2_keyboard_clk  => ps2_keyboard_clk
--		ps2_mouse_data    => ps2_mouse_data,
--		ps2_mouse_clk     => ps2_mouse_clk,
--		pic_gpio          => pic_gpio
	        );

------ Simulation only processes ----------------------------------------------
	clk_process: process
	begin
		while wait_flag = '0' loop
			clk <= '1';
		wait for clk_period/2;
			clk <= '0';
		wait for clk_period / 2;
		end loop;
		wait;
	end process;

	-- I/O settings go here.
	stimulus_process: process
		variable rt: boolean;

		function reportln(pc, insn: std_logic_vector) return boolean is
		begin
			report
				"pc("   & integer'image(to_integer(unsigned(pc)))    &") " &
				"insn(" & integer'image(to_integer(unsigned(insn)))  & ") ";
			return true;
		end reportln;

	begin
		rst <= '1';
		wait for clk_period * 2;
		rst <= '0';
		for i in 0 to number_of_iterations loop
			rt := reportln(debug_pc, debug_insn);
		wait for clk_period * 1;
	end loop;

	wait_flag   <=  '1';
	wait;
	end process;
------ END ---------------------------------------------------------------------
end architecture;

