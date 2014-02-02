-------------------------------------------------------------------------------
--! @file test_bench.vhd
--! @brief Main test bench. This is a simple test bench to
--! test the entire system, I/O is controlled here,
--! the program that is loaded into the CPU is in
--! the RAM module "mem_h2.vhd".
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--! This test bench should be split into two versions eventually:
--! 
--! * Automatic test bench generation:
--! 
--!   - Create a fast virtual machine in C.
--!   - Run a constrained random set of instructions on that processor.
--!   - Save the random input and the virtual machines state
--!   - Feed the state and the random input into a VHDL test bench.
--!   - Compare the results with assertions, they should be equal.
--! 
--! * PTTY testbench:
--! 
--!   - A Pseudo Terminal driven test bench that the user can type
--!   into, and the H2 Core will process the results fed in over a
--!   simulated UART
--! 
-------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity test_bench is
end test_bench;

architecture testing of test_bench is
    constant    clk_freq:        positive                     :=  1000000000;
    constant    clk_period:      time                         :=  1000 ms / clk_freq;

    signal  wait_flag:          std_logic                    :=  '0';
    signal  tb_debug_irq:       std_logic                    :=  '0';
    signal  tb_debug_irc:       std_logic_vector(3 downto 0) := (others => '0');
    signal  tb_debug_pc:        std_logic_vector(12 downto 0);
    signal  tb_debug_insn:      std_logic_vector(15 downto 0);
    signal  tb_debug_mem_dwe:   std_logic:= '0';   
    signal  tb_debug_mem_din:   std_logic_vector(15 downto 0);
    signal  tb_debug_mem_dout:  std_logic_vector(15 downto 0);
    signal  tb_debug_mem_daddr: std_logic_vector(12 downto 0);

    signal  tb_clk:          std_logic                    :=  '0';
    signal  tb_rst:          std_logic;

    signal  tb_btnu:         std_logic                    :=            '0';  -- button up
    signal  tb_btnd:         std_logic                    :=            '0';  -- button down
    signal  tb_btnc:         std_logic                    :=            '0';  -- button centre
    signal  tb_btnl:         std_logic                    :=            '0';  -- button left
    signal  tb_btnr:         std_logic                    :=            '0';  -- button right
    signal  tb_sw:           std_logic_vector(7 downto 0) := (others => '0'); -- switches
    signal  tb_an:           std_logic_vector(3 downto 0) := (others => '0'); -- anodes   7 segment display
    signal  tb_ka:           std_logic_vector(7 downto 0) := (others => '0'); -- kathodes 7 segment display
    signal  tb_ld:           std_logic_vector(7 downto 0) := (others => '0'); -- leds
    signal  tb_rx:           std_logic                    :=            '0';  -- uart rx 
    signal  tb_tx:           std_logic                    :=            '0';  -- uart tx
    signal  tb_red:          std_logic_vector(2 downto 0) := (others => '0');
    signal  tb_green:        std_logic_vector(2 downto 0) := (others => '0');
    signal  tb_blue:         std_logic_vector(1 downto 0) := (others => '0');
    signal  tb_hsync:        std_logic                    :=            '0';
    signal  tb_vsync:        std_logic                    :=            '0';
    signal  tb_ps2_keyboard_data: std_logic               :=            '0'; 
    signal  tb_ps2_keyboard_clk:  std_logic               :=            '0'; 
    signal  tb_ps2_mouse_data:    std_logic               :=            '0'; 
    signal  tb_ps2_mouse_clk:     std_logic               :=            '0'; 
    signal  tb_pic_gpio:         std_logic_vector(1 downto 0):= (others => 'X');
begin
---- Units under test ----------------------------------------------------------

    top_level_uut: entity work.top_level
    port map(
      -- Remove me{{
        debug_irq       => tb_debug_irq,
        debug_irc       => tb_debug_irc,
        debug_pc        => tb_debug_pc,
        debug_insn      => tb_debug_insn,
        debug_mem_dwe   => tb_debug_mem_dwe,
        debug_mem_din   => tb_debug_mem_din,
        debug_mem_dout  => tb_debug_mem_dout,
        debug_mem_daddr => tb_debug_mem_daddr,
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
        vsync => tb_vsync,
        ps2_keyboard_data => tb_ps2_keyboard_data,
        ps2_keyboard_clk => tb_ps2_keyboard_clk
--        ps2_mouse_data => tb_ps2_mouse_data,
--        ps2_mouse_clk => tb_ps2_mouse_clk,
--        pic_gpio => tb_pic_gpio
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
    variable rt: boolean;
    --! Eventually the asserts will go in this function as well!
    --! Just prints out formatted data, it is a shame a "wait"
    --! cannot be put in here with an integer passed to it to
    --! say for how many clock cycles to wait for.
    --! I would like to get rid of the return type, it is not
    --! needed.
    function reportln(pc, insn: std_logic_vector) return boolean is
    begin
        report "pc("   & integer'image(to_integer(unsigned(pc)))    & ") "
             & "insn(" & integer'image(to_integer(unsigned(insn)))  & ") "
        ;
      return true;
    end reportln;
	begin
        tb_sw <= X"A5";
        tb_rst <= '1';
        wait for clk_period * 2;
        tb_rst <= '0';
        for i in 0 to 255 loop
          rt:=reportln(tb_debug_pc, tb_debug_insn);
          wait for clk_period * 1;
        end loop;
        tb_debug_irq <= '1';
        tb_debug_irc <= X"4";
        wait for clk_period * 1;
        tb_debug_irc <= X"0";
        tb_debug_irq <= '0';
        wait for clk_period * 64;
        tb_debug_irq <= '1';
        tb_debug_irc <= X"8";
        wait for clk_period * 1;
        tb_debug_irc <= X"4";
        wait for clk_period * 1;
        tb_debug_irq <= '0';
        wait for clk_period * 64;
        wait_flag   <=  '1';
        wait;
    end process;
------ END ---------------------------------------------------------------------
end architecture;

