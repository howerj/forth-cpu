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
use work.led_pkg.all;
use work.kbd_pkg.ps2_kbd_top;
use work.uart_pkg.uart_core;

entity top is
	generic(
		clock_frequency:      positive := 100_000_000;
		uart_baud_rate:       positive := 115200;
		uart_fifo_depth:      positive := 8);
	port
	(
-- synthesis translate_off
		debug:    out cpu_debug_interface;
-- synthesis translate_on

		clk:      in  std_logic                    := 'X';  -- clock
		-- Buttons
		btnu:     in  std_logic                    := 'X';  -- button up
		btnd:     in  std_logic                    := 'X';  -- button down
		btnc:     in  std_logic                    := 'X';  -- button centre
		btnl:     in  std_logic                    := 'X';  -- button left
		btnr:     in  std_logic                    := 'X';  -- button right
		-- Switches
		sw:       in  std_logic_vector(7 downto 0) := (others => 'X'); -- switches
		-- Simple LED outputs
		an:       out std_logic_vector(3 downto 0) := (others => '0'); -- anodes   7 segment display
		ka:       out std_logic_vector(7 downto 0) := (others => '0'); -- cathodes 7 segment display

		ld:       out std_logic_vector(7 downto 0) := (others => '0'); -- leds

		-- UART
		rx:       in  std_logic                    := 'X';  -- uart rx
		tx:       out std_logic                    := '0';  -- uart tx

		-- VGA
		o_vga:    out vga_physical_interface;

		-- PS/2 Interface
		ps2_keyboard_data:  in std_logic           := '0';
		ps2_keyboard_clk:   in std_logic           := '0';

		-- Memory Interface
		RamCS:    out   std_logic := '1';
		--QuadSpiFlashCS: out   std_logic := '1';
		--MemClk:   out   std_logic := '0'; -- negative logic

		MemOE:    out   std_logic := '0'; -- negative logic
		MemWR:    out   std_logic := '0'; -- negative logic
		MemAdv:   out   std_logic := '0'; -- negative logic
		MemWait:  out   std_logic := '0'; -- positive!

		FlashCS:  out   std_logic := '0';
		FlashRp:  out   std_logic := '1';
		MemAdr:   out   std_logic_vector(26 downto 1) := (others => '0');
		MemDB:    inout std_logic_vector(15 downto 0) := (others => 'Z'));
end;

architecture behav of top is
	constant timer_length:           positive := 16;
	constant number_of_interrupts:   positive := 8;
	constant number_of_led_displays: positive := 4;
	constant timer_period_us:        positive := 20000;

	-- Signals
	signal rst: std_logic := '0';
	-- CPU H2 IO interface signals.
	signal cpu_wait: std_logic := '0';
	signal io_wr:    std_logic := '0';
	signal io_re:    std_logic := '0';
	signal io_din:   std_logic_vector(15 downto 0):= (others => '0');
	signal io_dout:  std_logic_vector(15 downto 0):= (others => '0');
	signal io_daddr: std_logic_vector(15 downto 0):= (others => '0');

	-- CPU H2 Interrupts
	signal cpu_irq:         std_logic := '0';
	signal cpu_irc:         std_logic_vector(number_of_interrupts - 1 downto 0) := (others => '0');
	signal cpu_irc_mask:    std_logic_vector(number_of_interrupts - 1 downto 0) := (others => '1');
	signal cpu_irc_mask_we: std_logic := '0';

	signal clk25MHz: std_logic:= '0';
	signal clk50MHz: std_logic:= '0';

	attribute buffer_type: string;
	attribute buffer_type of clk50MHz: signal is "BUFG";
	attribute buffer_type of clk25MHz: signal is "BUFG";

	-- Basic IO register

	---- LEDs/Switches
	signal ld_we:             std_logic := '0';

	---- VGA
	signal vga_data:          std_logic_vector(7 downto 0) := (others => '0');
	signal vga_data_we:       std_logic                    := '0';
	signal vga_data_busy:     std_logic                    := '0';

	---- UART
	signal rx_data:           std_logic_vector(7 downto 0) := (others => '0');
	signal rx_data_n:         std_logic_vector(7 downto 0) := (others => '0');
	signal rx_fifo_empty:     std_logic := '0';
	signal rx_fifo_full:      std_logic := '0';
	signal rx_data_re:        std_logic := '0';
	signal rx_data_re_n:      std_logic := '0';

	signal tx_data:           std_logic_vector(7 downto 0) := (others => '0');
	signal tx_fifo_full:      std_logic := '0';
	signal tx_fifo_empty:     std_logic := '0';
	signal tx_data_we:        std_logic := '0';

	---- Timer
	signal timer_control_we:  std_logic := '0';
	signal timer_control_i:   std_logic_vector(timer_length - 1 downto 0) := (others =>'0');
	signal timer_counter_o:   std_logic_vector(timer_length - 4 downto 0) := (others =>'0');
	signal timer_irq:         std_logic;

	---- PS/2
	signal kbd_char_buf_new:  std_logic := '0';
	signal kbd_char_buf:      std_logic_vector(6 downto 0) := (others => '0'); -- ASCII char
	signal kbd_char_re:       std_logic := '0';

	---- 8 Segment Display

	signal leds_reg:          std_logic_vector(15 downto 0) := (others => '0');
	signal leds_reg_we:       std_logic := '0';

	---- Buttons
	signal btnu_d:            std_logic := '0';  -- button up
	signal btnd_d:            std_logic := '0';  -- button down
	signal btnc_d:            std_logic := '0';  -- button centre
	signal btnl_d:            std_logic := '0';  -- button left
	signal btnr_d:            std_logic := '0';  -- button right

	-- Switches
	signal sw_d:              std_logic_vector(sw'range) := (others => '0');

	-- Memory
	signal mem_addr_26_17:    std_logic_vector(26 downto 17) := (others => '0');
	signal mem_addr_26_17_we: std_logic := '0';

	signal mem_addr_16_1:     std_logic_vector(16 downto 1) := (others => '0');
	signal mem_addr_16_1_we:  std_logic := '0';

	signal mem_data_i:        std_logic_vector(15 downto 0) := (others => '0');
	signal mem_data_i_we:     std_logic := '0';
	signal mem_data_buf_i:    std_logic_vector(15 downto 0) := (others => '0');
	signal mem_data_o:        std_logic_vector(15 downto 0) := (others => '0');

	signal mem_control_i:     std_logic_vector(5 downto 0)  := (others => '0');
	signal mem_control_o:     std_logic_vector(5 downto 0)  := (others => '0');
	signal mem_control_we:    std_logic := '0';

	signal mem_we:            std_logic := '0';
	signal mem_oe:            std_logic := '0';
begin
-------------------------------------------------------------------------------
-- The Main components
-------------------------------------------------------------------------------

------- Output assignments (Not in a process) ---------------------------------

	-- @warning These are both temporary measures for testing only!
	rst        <= btnu_d;
	cpu_wait   <= btnc_d;

	irq_block: block
		signal rx_fifo_not_empty: std_logic := '0';
		signal tx_fifo_not_empty: std_logic := '0';
	begin
		rx_fifo_not_empty <= not rx_fifo_empty;
		tx_fifo_not_empty <= not rx_fifo_empty;

		cpu_irc(0) <= '0';
		cpu_irc(1) <= rx_fifo_not_empty;
		cpu_irc(2) <= rx_fifo_full;
		cpu_irc(3) <= tx_fifo_not_empty;
		cpu_irc(4) <= tx_fifo_full;
		cpu_irc(5) <= kbd_char_buf_new;
		cpu_irc(6) <= timer_irq;
		cpu_irc(7) <= btnl_d; -- @todo replace with button change state

		cpu_irq    <= '1' when
				timer_irq         = '1' or
				rx_fifo_not_empty = '1' or
				rx_fifo_full      = '1' or
				tx_fifo_not_empty = '1' or
				tx_fifo_full      = '1' or
				kbd_char_buf_new  = '1' or
				btnl_d            = '1'
				else '0';
	end block;

	core_0: entity work.core
	generic map(number_of_interrupts => number_of_interrupts)
	port map(
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
		cpu_irq          => cpu_irq,
		cpu_irc          => cpu_irc,
		cpu_irc_mask     => cpu_irc_mask,
		cpu_irc_mask_we  => cpu_irc_mask_we);

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------
	-- Xilinx Application Note:
	-- It seems like it buffers the clock correctly here, so no need to
	-- use a DCM. However, see:
	-- http://electronics.stackexchange.com/questions/112534/using-digital-clock-manager-with-verilog-to-generate-25mhz-clock-from-32mhz-inte
	---- Clock divider /2.
	clk50MHz <= '0' when rst = '1' else not clk50MHz when rising_edge(clk);

	---- Clock divider /2. Pixel clock is 25MHz
	clk25MHz <= '0' when rst = '1' else not clk25MHz when rising_edge(clk50MHz);

	assert not(io_wr = '1' and io_re = '1') report "IO Read/Write issued at same time" severity error;
	assert not(io_wr = '1' or io_re = '1') or not  io_daddr(0) = '1' report "Unaligned register access" severity error;

	cpu_irc_mask      <= io_dout(number_of_interrupts - 1 downto 0);
	timer_control_i   <= io_dout;
	vga_data          <= io_dout(vga_data'range);
	leds_reg          <= io_dout;
	tx_data           <= io_dout(tx_data'range);
	mem_addr_16_1     <= io_dout;
	mem_addr_26_17    <= io_dout(9 downto 0);
	mem_control_i     <= io_dout(15 downto 10);
	mem_data_i        <= io_dout;

	io_write: block
		signal selector: std_logic_vector(3 downto 0) := (others => '0');
		signal is_write: boolean := false;
	begin
		selector           <= io_daddr(4 downto 1);
		is_write           <= true when io_wr = '1' else false;

		tx_data_we         <= io_dout(13) when is_write and selector = x"0" else '0';
		rx_data_re         <= io_dout(10) when is_write and selector = x"0" else '0';

		vga_data_we        <= io_dout(13) when is_write and selector = x"1" else '0';
		kbd_char_re        <= io_dout(10) when is_write and selector = x"1" else '0';

		timer_control_we   <= '1'         when is_write and selector = x"2" else '0';
		ld_we              <= '1'         when is_write and selector = x"3" else '0';
		leds_reg_we        <= '1'         when is_write and selector = x"4" else '0';
		cpu_irc_mask_we    <= '1'         when is_write and selector = x"5" else '0';

		mem_addr_26_17_we  <= '1'         when is_write and selector = x"6" else '0';
		mem_control_we     <= '1'         when is_write and selector = x"6" else '0';

		mem_addr_16_1_we   <= '1'         when is_write and selector = x"7" else '0';
		mem_data_i_we      <= '1'         when is_write and selector = x"8" else '0';
	end block;

	io_read: process(
		io_wr, io_re, io_daddr,
		sw_d, rx, btnu_d, btnd_d, btnl_d, btnr_d, btnc_d,
		kbd_char_buf_new, kbd_char_buf,

		rx_data_n,
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
			io_din(7 downto 0) <= rx_data_n;
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
			io_din <= "00" & rx & btnu_d & btnd_d & btnl_d & btnr_d & btnc_d & sw_d;
		when "100" =>
			io_din             <= mem_data_o;
		when others => io_din <= (others => '0');
		end case;
	end process;

	--- UART ----------------------------------------------------------
	uart_rx_data_reg_we_0: work.util.reg
		generic map(
			N      => 1)
		port map(
			clk    => clk,
			rst    => rst,
			we     => '1',
			di(0)  => rx_data_re,
			do(0)  => rx_data_re_n);

	uart_rx_data_reg_0: work.util.reg
		generic map(
			N => rx_data_n'high + 1)
		port map(
			clk => clk,
			rst => rst,
			we  => rx_data_re_n,
			di  => rx_data,
			do  => rx_data_n);

	uart_0: work.uart_pkg.uart_top
		generic map(
			baud_rate       => uart_baud_rate,
			clock_frequency => clock_frequency,
			fifo_depth      => uart_fifo_depth)
		port map(
			clk             =>  clk,
			rst             =>  rst,
			rx_data         =>  rx_data,
			rx_fifo_empty   =>  rx_fifo_empty,
			rx_fifo_full    =>  rx_fifo_full,
			rx_data_re      =>  rx_data_re,
			tx_data         =>  tx_data,
			tx_fifo_full    =>  tx_fifo_full,
			tx_fifo_empty   =>  tx_fifo_empty,
			tx_data_we      =>  tx_data_we,
			tx              =>  tx,
			rx              =>  rx);
	--- UART ----------------------------------------------------------

	--- LED Output ----------------------------------------------------
	led_output_reg_0: entity work.reg
		generic map(N => ld'high + 1)
		port map(
			clk => clk,
			rst => rst,
			we  => ld_we,
			di  => io_dout(ld'range),
			do  => ld);
	--- LED Output ----------------------------------------------------

	--- Timer ---------------------------------------------------------
	timer0_0: entity work.timer
	generic map(timer_length => timer_length)
	port map(
		clk       => clk,
		rst       => rst,
		we        => timer_control_we,
		control_i => timer_control_i,
		counter_o => timer_counter_o,
		irq       => timer_irq,
		q         => open,
		nq        => open);
	--- Timer ---------------------------------------------------------

	--- VGA -----------------------------------------------------------
	vt100_0: work.vga_pkg.vt100
	port map(
		clk         =>  clk,
		clk25MHz    =>  clk25MHz,
		rst         =>  rst,
		we          =>  vga_data_we,
		char        =>  vga_data,
		busy        =>  vga_data_busy,
		o_vga       =>  o_vga);
	--- VGA -----------------------------------------------------------

	--- Keyboard ------------------------------------------------------
	keyboard_0: work.kbd_pkg.keyboard
	generic map(
		clock_frequency           => clock_frequency,
		ps2_debounce_counter_size => 8)
	port map(
		clk              => clk,
		rst              => rst,

		ps2_clk          => ps2_keyboard_clk,
		ps2_data         => ps2_keyboard_data,

		kbd_char_re      => kbd_char_re,
		kbd_char_buf_new => kbd_char_buf_new,
		kbd_char_buf     => kbd_char_buf);
	--- Keyboard ------------------------------------------------------

	--- LED 8 Segment display -----------------------------------------
	ledseg_0: entity work.led_8_segment_display
	generic map(
		number_of_led_displays => number_of_led_displays,
		clock_frequency        => clock_frequency,
		use_bcd_not_hex        => false)
	port map(
		clk        => clk,
		rst        => rst,

		leds_we    => leds_reg_we,
		leds       => leds_reg,

		an         => an,
		ka         => ka);
	--- LED 8 Segment display -----------------------------------------

	--- Buttons -------------------------------------------------------
	button_debouncer: work.util.debounce_block_us
	generic map(
		N               => 5,
		clock_frequency => clock_frequency,
		timer_period_us => timer_period_us)
	port map(
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
	--- Buttons -------------------------------------------------------

	--- Switches ------------------------------------------------------
	sw_debouncer: work.util.debounce_block_us
		generic map(
			N               => sw'high+1,
			clock_frequency => clock_frequency,
			timer_period_us => timer_period_us)
		port map(clk => clk, di => sw, do => sw_d);
	--- Switches ------------------------------------------------------

	--- Memory Interface ----------------------------------------------
	-- @todo Move to memory interface module
	-- @todo The memory interface should be WORD aligned, that is, the
	-- lowest bit of the address should be dropped
	mem_addr_16_1_reg: entity work.reg
		generic map(N => 16)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_addr_16_1_we,
			di  => mem_addr_16_1,
			do  => MemAdr(16 downto 1));

	mem_addr_26_17_reg: entity work.reg
		generic map(N => 10)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_addr_26_17_we,
			di  => mem_addr_26_17,
			do  => MemAdr(26 downto 17));

	mem_control_reg: entity work.reg
		generic map(N => 6)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_control_we,
			di  => mem_control_i,
			do  => mem_control_o);

	mem_data_i_reg: entity work.reg
		generic map(N => 16)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_data_i_we,
			di  => mem_data_i,
			do  => mem_data_buf_i);

	FlashCS    <= '0' when mem_control_o(5 downto 4) /= "00" and mem_control_o(0) = '1' else '1';
	RamCS      <= '0' when mem_control_o(5 downto 4) /= "00" and mem_control_o(1) = '1' else '1';
	MemWait    <= mem_control_o(2);
	FlashRp    <= '0' when mem_control_o(3) = '1' else '1';
	MemAdv     <= '0' when mem_oe = '1' or mem_we = '1' else '1';
	mem_oe     <= '1' when mem_control_o(5 downto 4) = "01"  else '0';
	mem_we     <= '1' when mem_control_o(5 downto 4) = "10"  else '0';

	MemOE      <= not mem_oe;
	MemWR      <= not mem_we;

	mem_data_o <= MemDB when mem_oe = '1' else (others => '0');
	MemDB      <= mem_data_buf_i when mem_we = '1' else (others => 'Z');

	--- Memory Interface ----------------------------------------------

-------------------------------------------------------------------------------
end architecture;

