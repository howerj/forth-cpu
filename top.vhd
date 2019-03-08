---------------------------------------------------------------------------------
--| @file top.vhd
--| @brief This file is the top level of the project.
--|  It presents an interface between the CPU,
--|  RAM, and all the I/O modules.
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
---------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.core_pkg.all;
use work.vga_pkg.all;
use work.kbd_pkg.ps2_kbd_top;
use work.util.all;
use work.uart_pkg.all;

entity top is
	generic(
		g: common_generics         := default_settings;
		reset_period_us:  natural  := 1;
		uart_baud:        positive := 115200;
		uart_fifo_depth:  positive := 8);
	port
	(
		-- synthesis translate_off
		debug:    out cpu_debug_interface;
		-- synthesis translate_on

		clk:      in  std_ulogic                    := 'X';  -- clock
		-- Buttons
		btnu:     in  std_ulogic                    := 'X';  -- button up
		btnd:     in  std_ulogic                    := 'X';  -- button down
		btnc:     in  std_ulogic                    := 'X';  -- button centre
		btnl:     in  std_ulogic                    := 'X';  -- button left
		btnr:     in  std_ulogic                    := 'X';  -- button right
		-- Switches
		sw:       in  std_ulogic_vector(7 downto 0) := (others => 'X'); -- switches
		-- Simple LED outputs
		an:       out std_ulogic_vector(3 downto 0) := (others => '0'); -- anodes   7 segment display
		ka:       out std_ulogic_vector(7 downto 0) := (others => '0'); -- cathodes 7 segment display

		ld:       out std_ulogic_vector(7 downto 0) := (others => '0'); -- leds

		-- UART
		rx:       in  std_ulogic                    := 'X';  -- uart rx
		tx:       out std_ulogic                    := '0';  -- uart tx

		-- VGA
		o_vga:    out vga_physical_interface;

		-- PS/2 Interface
		ps2_keyboard_data:  in std_ulogic           := '0';
		ps2_keyboard_clk:   in std_ulogic           := '0';

		-- Memory Interface
		ram_cs:    out   std_ulogic := '1';
		mem_oe:    out   std_ulogic := '0'; -- negative logic
		mem_wr:    out   std_ulogic := '0'; -- negative logic
		mem_adv:   out   std_ulogic := '0'; -- negative logic
		mem_wait:  out   std_ulogic := '0'; -- positive logic!
		flash_cs:  out   std_ulogic := '0';
		flash_rp:  out   std_ulogic := '1';
		mem_addr:   out   std_ulogic_vector(26 downto 1) := (others => '0');
		mem_data:    inout std_logic_vector(15 downto 0)  := (others => 'Z'));
end;

architecture behav of top is
	constant timer_length:           positive := 16;
	constant number_of_interrupts:   positive := 8;
	constant number_of_led_displays: positive := 4;
	constant timer_period_us:        positive := 20000;

	-- Signals
	signal rst:      std_ulogic := '0';
	-- CPU H2 IO interface signals.
	signal cpu_wait: std_ulogic := '0';
	signal io_wr:    std_ulogic := '0';
	signal io_re:    std_ulogic := '0';
	signal io_din:   std_ulogic_vector(15 downto 0) := (others => '0');
	signal io_dout:  std_ulogic_vector(15 downto 0) := (others => '0');
	signal io_daddr: std_ulogic_vector(15 downto 0) := (others => '0');

	-- CPU H2 Interrupts
	signal cpu_irc:         std_ulogic_vector(number_of_interrupts - 1 downto 0) := (others => '0');
	signal cpu_irc_mask_we: std_ulogic := '0';

	signal clk25MHz: std_ulogic:= '0';
	signal clk50MHz: std_ulogic:= '0';

	attribute buffer_type: string;
	attribute buffer_type of clk50MHz: signal is "BUFG";
	attribute buffer_type of clk25MHz: signal is "BUFG";

	-- Basic IO register

	---- LEDs/Switches
	signal ld_we:             std_ulogic := '0';

	---- VGA
	signal vga_data:          std_ulogic_vector(7 downto 0) := (others => '0');
	signal vga_data_we:       std_ulogic                    := '0';
	signal vga_data_busy:     std_ulogic                    := '0';

	---- UART
	signal rx_data:           std_ulogic_vector(7 downto 0) := (others => '0');
	signal rx_fifo_empty:     std_ulogic := '0';
	signal rx_fifo_full:      std_ulogic := '0';
	signal rx_data_re:        std_ulogic := '0';

	signal tx_fifo_full:      std_ulogic := '0';
	signal tx_fifo_empty:     std_ulogic := '0';
	signal tx_data_we:        std_ulogic := '0';

	signal uart_clock_tx_we:  std_ulogic := '0';
	signal uart_clock_rx_we:  std_ulogic := '0';
	signal uart_control_we:   std_ulogic := '0';
	---- Timer
	signal timer_control_we:  std_ulogic := '0';
	signal timer_counter_o:   std_ulogic_vector(timer_length - 4 downto 0) := (others =>'0');
	signal timer_irq:         std_ulogic;

	---- PS/2
	signal kbd_char_buf_new:  std_ulogic := '0';
	signal kbd_char_buf:      std_ulogic_vector(6 downto 0) := (others => '0'); -- ASCII char
	signal kbd_char_re:       std_ulogic := '0';

	---- 8 Segment Display
	signal leds_reg_we:       std_ulogic := '0';

	---- Buttons
	signal btnu_d:            std_ulogic := '0'; -- button up
	signal btnd_d:            std_ulogic := '0'; -- button down
	signal btnc_d:            std_ulogic := '0'; -- button centre
	signal btnl_d:            std_ulogic := '0'; -- button left
	signal btnr_d:            std_ulogic := '0'; -- button right
	signal button_changed:    std_ulogic := '0'; -- Any of the buttons have changed state

	-- Switches
	signal sw_d:              std_ulogic_vector(sw'range) := (others => '0');

	-- Memory
	signal mem_addr_26_17_we: std_ulogic := '0';
	signal mem_addr_16_1_we:  std_ulogic := '0';
	signal mem_data_i_we:     std_ulogic := '0';
	signal mem_data_o:        std_ulogic_vector(15 downto 0) := (others => '0');
	signal mem_control_we:    std_ulogic := '0';
begin
-------------------------------------------------------------------------------
-- The Main components
-------------------------------------------------------------------------------

	cpu_wait   <= btnc_d; -- temporary testing measure only!

	system_reset: work.util.reset_generator
	generic map (g => g, reset_period_us => reset_period_us)
	port map (
		clk        => clk,
		rst        => rst);

	cpu_irc(0) <= btnu_d; -- configurable CPU reset (can mask this)
	cpu_irc(1) <= not rx_fifo_empty;
	cpu_irc(2) <= rx_fifo_full;
	cpu_irc(3) <= not tx_fifo_empty;
	cpu_irc(4) <= tx_fifo_full;
	cpu_irc(5) <= kbd_char_buf_new;
	cpu_irc(6) <= timer_irq;
	cpu_irc(7) <= button_changed;

	core_0: entity work.core
	generic map (g => g, number_of_interrupts => number_of_interrupts)
	port map (
-- synthesis translate_off
		debug            => debug,
-- synthesis translate_on
		clk              => clk,
		rst              => rst,
		stop             => cpu_wait,
		io_wr            => io_wr,
		io_re            => io_re,
		io_din           => io_din,
		io_dout          => io_dout,
		io_daddr         => io_daddr,
		cpu_irc          => cpu_irc,
		cpu_irc_mask     => io_dout(number_of_interrupts - 1 downto 0),
		cpu_irc_mask_we  => cpu_irc_mask_we);

	-------------------------------------------------------------------------------
	-- IO
	-------------------------------------------------------------------------------

	-- NOTE: A Wishbone Interface on each of the components would simplify the
	-- system overall. However, each peripheral would need an interface
	-- specifying. This module could be made to be much smaller.
	-- See: <https://en.wikipedia.org/wiki/Wishbone_(computer_bus)>
	-- And: <http://cdn.opencores.org/downloads/wbspec_b4.pdf>

	-- Xilinx Application Note:
	-- It seems like it buffers the clock correctly here, so no need to
	-- use a DCM. However, see:
	-- http://electronics.stackexchange.com/questions/112534/using-digital-clock-manager-with-verilog-to-generate-25mhz-clock-from-32mhz-inte
	---- Clock divider /2.
	clk50MHz <= '0' when rst = '1' else not clk50MHz when rising_edge(clk);

	---- Clock divider /2. Pixel clock is 25MHz
	clk25MHz <= '0' when rst = '1' else not clk25MHz when rising_edge(clk50MHz);

	-- It possible for CPU to issue both signals at the same time, but it should
	-- not happen with a standard instruction.
	assert not(io_wr = '1' and io_re = '1') report "IO Read/Write issued at same time" severity error;

	vga_data <= io_dout(vga_data'range);

	-- TODO: Raise trap (interrupt with bus error) on invalid memory access.
	io_write: block
		signal selector: std_ulogic_vector(3 downto 0) := (others => '0');
		signal is_write: boolean := false;
	begin
		selector          <= io_daddr(4 downto 1);
		is_write          <= true when io_wr = '1' else false;

		tx_data_we        <= io_dout(13) when is_write and selector = x"0" else '0';
		rx_data_re        <= io_dout(10) when is_write and selector = x"0" else '0';

		vga_data_we       <= io_dout(13) when is_write and selector = x"1" else '0';
		kbd_char_re       <= io_dout(10) when is_write and selector = x"1" else '0';

		timer_control_we  <= '1'         when is_write and selector = x"2" else '0';
		ld_we             <= '1'         when is_write and selector = x"3" else '0';
		mem_data_i_we     <= '1'         when is_write and selector = x"4" else '0';

		mem_addr_26_17_we <= '1'         when is_write and selector = x"5" else '0';
		mem_control_we    <= '1'         when is_write and selector = x"5" else '0';

		mem_addr_16_1_we  <= '1'         when is_write and selector = x"6" else '0';

		leds_reg_we       <= '1'         when is_write and selector = x"7" else '0';
		cpu_irc_mask_we   <= '1'         when is_write and selector = x"8" else '0';

		uart_clock_tx_we  <= '1'         when is_write and selector = x"9" else '0';
		uart_clock_rx_we  <= '1'         when is_write and selector = x"A" else '0';
		uart_control_we   <= '1'         when is_write and selector = x"B" else '0';
	end block;

	io_read: process(
		io_wr, io_re, io_daddr,
		sw_d, btnu_d, btnd_d, btnl_d, btnr_d, btnc_d,
		kbd_char_buf_new, kbd_char_buf,

		rx_data,
		rx_fifo_empty,
		rx_fifo_full,

		tx_fifo_full,
		tx_fifo_empty,

		timer_counter_o,

		vga_data_busy,

		mem_data_o)
	begin
		io_din <= (others => '0');

		-- The signal io_re is not needed as none of the reads have
		-- any side effects

		case io_daddr(3 downto 1) is
		when "000" => -- buttons, plus direct access to UART bit.
			io_din(7 downto 0) <= rx_data;
			io_din(8)          <= rx_fifo_empty;
			io_din(9)          <= rx_fifo_full;
			io_din(11)         <= tx_fifo_empty;
			io_din(12)         <= tx_fifo_full;
		when "001" => -- VT100 status and Keyboard
			io_din(6 downto 0) <= kbd_char_buf;
			io_din(8)          <= not kbd_char_buf_new;
			io_din(9)          <= kbd_char_buf_new;
			io_din(11)         <= not vga_data_busy;
			io_din(12)         <= vga_data_busy;
		when "010" => -- Timer in
			io_din(timer_counter_o'range) <= timer_counter_o;
		when "011" => -- Switches and buttons
			io_din <= "000" & btnu_d & btnd_d & btnl_d & btnr_d & btnc_d & sw_d;
		when "100" =>
			io_din             <= mem_data_o;
		when others => io_din <= (others => '0');
		end case;
	end process;

	--- UART ----------------------------------------------------------
	uart_fifo_0: work.uart_pkg.uart_top
		generic map (g => g, baud => uart_baud, use_fifo => false)
		port map (
			clk => clk, 
			rst => rst,

			tx            => tx,
			tx_fifo_full  => tx_fifo_full,
			tx_fifo_empty => tx_fifo_empty,
			tx_fifo_we    => tx_data_we,
			tx_fifo_data  => io_dout(7 downto 0),

			rx            => rx,
			rx_fifo_re    => rx_data_re,
			rx_fifo_data  => rx_data,
			rx_fifo_full  => rx_fifo_full,
			rx_fifo_empty => rx_fifo_empty,

			reg             => io_dout,
			clock_reg_tx_we => uart_clock_tx_we,
			clock_reg_rx_we => uart_clock_rx_we,
			control_reg_we  => uart_control_we);
	--- UART ----------------------------------------------------------

	--- LED Output ----------------------------------------------------
	led_output_reg_0: entity work.reg
		generic map (g => g, N => ld'length)
		port map (
			clk => clk,
			rst => rst,
			we  => ld_we,
			di  => io_dout(ld'range),
			do  => ld);
	--- LED Output ----------------------------------------------------

	--- Timer ---------------------------------------------------------
	timer_0: entity work.timer
	generic map (g => g, timer_length => timer_length)
	port map (
		clk       => clk,
		rst       => rst,
		we        => timer_control_we,
		control_i => io_dout,
		counter_o => timer_counter_o,
		irq       => timer_irq);
	--- Timer ---------------------------------------------------------

	--- VGA -----------------------------------------------------------
	vga_selector: block
		constant use_vt100: boolean := true;
	begin
		gen_vt100_0: if use_vt100 generate
		vt100_0: work.vga_pkg.vt100
			generic map (g => g)
			port map (
				clk         =>  clk,
				clk25MHz    =>  clk25MHz,
				rst         =>  rst,
				we          =>  vga_data_we,
				char        =>  vga_data,
				busy        =>  vga_data_busy,
				o_vga       =>  o_vga);
		end generate;
	
		-- Test code
		-- NOTE: Timing is not the best, VGA monitor loses synchronization
		-- every so often with this module.
		vga_gen_c1: if not use_vt100 generate
		vga_c1: block 
			signal row, column: integer := 0;
			signal h_blank, v_blank, draw: std_ulogic := '0';
		begin
			draw <= not h_blank and not v_blank;
			vga_c: work.util.vga_controller
			generic map (
				g => g,
				pixel_clock_frequency => 25_000_000,
				cfg => work.util.vga_640x480)
			port map (
				clk    => clk25MHz,
				rst    => rst,
				row    => row,
				column => column,
				h_blank => h_blank,
				v_blank => v_blank,
				h_sync => o_vga.hsync,
				v_sync => o_vga.vsync);
			o_vga.red   <= "111" when draw = '1' else "000";
			o_vga.green <= "111" when (draw = '1' and row < 100 and column < 100) else "000";
			o_vga.blue  <= "11";
		end block;
		end generate;
	end block;
	--- VGA -----------------------------------------------------------

	--- Keyboard ------------------------------------------------------
	keyboard_0: work.kbd_pkg.keyboard
	generic map (g => g, ps2_debounce_counter_size => 8)
	port map (
		clk              => clk,
		rst              => rst,

		ps2_clk          => ps2_keyboard_clk,
		ps2_data         => ps2_keyboard_data,

		kbd_char_re      => kbd_char_re,
		kbd_char_buf_new => kbd_char_buf_new,
		kbd_char_buf     => kbd_char_buf);
	--- Keyboard ------------------------------------------------------

	--- LED 8 Segment display -----------------------------------------
	ledseg_0: entity work.led_7_segment_display
	generic map (
		g                      => g,
		number_of_led_displays => number_of_led_displays,
		use_bcd_not_hex        => false)
	port map (
		clk        => clk,
		rst        => rst,

		leds_we    => leds_reg_we,
		leds       => io_dout,

		an         => an,
		ka         => ka);
	--- LED 8 Segment display -----------------------------------------

	--- Buttons -------------------------------------------------------
	button_debouncer: work.util.debounce_block_us
	generic map (g => g, N => 5, timer_period_us    => timer_period_us)
	port map (
		clk   => clk,
		di(0) => btnu,
		di(1) => btnd,
		di(2) => btnc,
		di(3) => btnl,
		di(4) => btnr,
		do(0) => btnu_d,
		do(1) => btnd_d,
		do(2) => btnc_d,
		do(3) => btnl_d,
		do(4) => btnr_d);

	dpad_changed: block
		signal changed_signals:     std_ulogic_vector(4 downto 0) := (others => '0');
		signal any_changed_signals: std_ulogic := '0';
	begin
		state_changed: work.util.state_block_changed
		generic map (g => g, N => changed_signals'length)
		port map (
			clk   => clk,
			rst   => rst,
			di(0) => btnu_d,
			di(1) => btnd_d,
			di(2) => btnc_d,
			di(3) => btnl_d,
			di(4) => btnr_d,
			do    => changed_signals);

		any_changed_signals <= '1' when changed_signals /= "00000" else '0';

		state_changed_reg: work.util.reg
		generic map (g => g, N => 1)
		port map (
			clk   => clk,
			rst   => rst,
			di(0) => any_changed_signals,
			we    => '1',
			do(0) => button_changed);
	end block;

	--- Buttons -------------------------------------------------------

	--- Switches ------------------------------------------------------
	sw_debouncer: work.util.debounce_block_us
		generic map (g => g, N => sw'length, timer_period_us => timer_period_us)
		port map (clk => clk, di => sw, do => sw_d);
	--- Switches ------------------------------------------------------

	--- Memory Interface ----------------------------------------------
	ram_interface_0: entity work.ram_interface
	generic map (g => g)
	port map (
		clk               =>  clk,
		rst               =>  rst,
		mem_addr_16_1     =>  io_dout(io_dout'high downto 1),
		mem_addr_16_1_we  =>  mem_addr_16_1_we,
		mem_addr_26_17    =>  io_dout(9 downto 0),
		mem_addr_26_17_we =>  mem_addr_26_17_we,
		mem_control_i     =>  io_dout(15 downto 10),
		mem_control_we    =>  mem_control_we,
		mem_data_i        =>  io_dout,
		mem_data_i_we     =>  mem_data_i_we,
		mem_data_o        =>  mem_data_o,
		ram_cs            =>  ram_cs,
		mem_oe            =>  mem_oe,
		mem_wr            =>  mem_wr,
		mem_adv           =>  mem_adv,
		mem_wait          =>  mem_wait,
		flash_cs          =>  flash_cs,
		flash_rp          =>  flash_rp,
		mem_addr          =>  mem_addr,
		mem_data          =>  mem_data);
	--- Memory Interface ----------------------------------------------

-------------------------------------------------------------------------------
end architecture;

