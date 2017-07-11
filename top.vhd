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
use work.cpu_pkg.all;
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
		-- @todo Turn button interface into record.
		btnu:     in  std_logic                    := 'X';  -- button up
		btnd:     in  std_logic                    := 'X';  -- button down
		btnc:     in  std_logic                    := 'X';  -- button centre
		btnl:     in  std_logic                    := 'X';  -- button left
		btnr:     in  std_logic                    := 'X';  -- button right
		-- Switches
		sw:       in  std_logic_vector(7 downto 0) := (others => 'X'); -- switches
		-- Simple LED outputs
		an:       out std_logic_vector(3 downto 0) := (others => '0'); -- anodes   7 segment display
		ka:       out std_logic_vector(7 downto 0) := (others => '0'); -- kathodes 7 segment display

		ld:       out std_logic_vector(7 downto 0) := (others => '0'); -- leds

		-- UART
		rx:       in  std_logic                    := 'X';  -- uart rx
		tx:       out std_logic                    := '0';  -- uart tx

		-- VGA
		o_vga:    out vga_physical_interface;

		-- PWM from timer
		gpt0_q:   out std_logic                    := '0';
		gpt0_nq:  out std_logic                    := '0';
		-- PS/2 Interface
		ps2_keyboard_data:  in std_logic           := '0';
		ps2_keyboard_clk:   in std_logic           := '0');
end;

architecture behav of top is
	constant timer_length:         positive := 16;
	constant number_of_interrupts: positive := 8;

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
	
	signal ld_c, ld_n: std_logic_vector(7 downto 0):=  (others => '0');

	---- VGA
	signal vga_control:    vga_control_registers_interface    := vga_control_registers_initialize;
	signal vga_control_we: vga_control_registers_we_interface := vga_control_registers_we_initialize;

	signal vga_dout:    std_logic_vector(15 downto 0) := (others => '0');

	---- UART
	signal rx_data:        std_logic_vector(7 downto 0) := (others => '0');
	signal rx_data_n:      std_logic_vector(7 downto 0) := (others => '0');
	signal rx_fifo_empty:  std_logic := '0';
	signal rx_fifo_full:   std_logic := '0';
	signal rx_data_re:     std_logic := '0';
	signal rx_data_re_n:   std_logic := '0';

	signal tx_data:        std_logic_vector(7 downto 0) := (others => '0');
	signal tx_fifo_full:   std_logic := '0';
	signal tx_fifo_empty:  std_logic := '0';
	signal tx_data_we:     std_logic := '0';


	---- Timer
	signal timer_control_we: std_logic := '0';
	signal timer_control_i:  std_logic_vector(timer_length - 1 downto 0) := (others =>'0');
	signal timer_control_o:  std_logic_vector(timer_length - 1 downto 0) := (others =>'0');
	signal timer_counter_o:  std_logic_vector(timer_length - 4 downto 0) := (others =>'0');
	signal timer_irq:        std_logic;
	signal timer_q:          std_logic;
	signal timer_nq:         std_logic;

	---- PS/2
	-- @todo Replace with FIFO
	signal kbd_new:      std_logic := '0';  -- new ASCII char available
	signal kbd_new_edge: std_logic := '0';
	signal kbd_char:     std_logic_vector(6 downto 0); -- ASCII char
	signal kbd_new_c,  kbd_new_n:  std_logic := '0';
	signal kbd_char_c, kbd_char_n: std_logic_vector(6 downto 0) := (others => '0'); -- ASCII char

	---- 8 Segment Display


	signal leds_reg:    std_logic_vector(15 downto 0) := (others => '0');
	signal leds_reg_we: std_logic := '0';

	---- Buttons

	signal btnu_d: std_logic := '0';  -- button up
	signal btnd_d: std_logic := '0';  -- button down
	signal btnc_d: std_logic := '0';  -- button centre
	signal btnl_d: std_logic := '0';  -- button left
	signal btnr_d: std_logic := '0';  -- button right

	signal sw_d:   std_logic_vector(sw'range) := (others => '0');

	-- LFSR
--	constant lfsr_tap: std_logic_vector(14 downto 0) := "100000000001011";
--	signal lfsr_o:     std_logic_vector(lfsr_tap'high + 1 downto lfsr_tap'low);
--	signal lfsr_i:     std_logic_vector(lfsr_o'range);
--	signal lfsr_i_we:  std_logic := '0';
begin
-------------------------------------------------------------------------------
-- The Main components
-------------------------------------------------------------------------------

------- Output assignments (Not in a process) ---------------------------------

	gpt0_q  <= timer_q;
	gpt0_nq <= timer_nq;

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
		cpu_irc(5) <= kbd_new;
		cpu_irc(6) <= timer_irq;
		cpu_irc(7) <= btnl_d; -- @todo replace with button change state

		cpu_irq    <= '1' when
				timer_irq         = '1' or
				rx_fifo_not_empty = '1' or
				rx_fifo_full      = '1' or
				tx_fifo_not_empty = '1' or
				tx_fifo_full      = '1' or
				kbd_new           = '1' or
				btnl_d            = '1'
				else '0';
	end block;

	cpu_0: entity work.cpu
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
	---- End note.

	io_nextState: process(clk, rst)
	begin
		if rst = '1' then
			-- LEDs/Switches
			ld_c       <= (others => '0');
			-- PS/2
			kbd_char_c <= (others => '0');
			kbd_new_c  <= '0';
		elsif rising_edge(clk) then
			-- LEDs/Switches
			ld_c        <=  ld_n;
			-- PS/2
			kbd_char_c  <= kbd_char_n;
			kbd_new_c   <= kbd_new_n;
		end if;
	end process;

	io_select: process(
		io_wr, io_re, io_dout, io_daddr,
		ld_c,
		sw_d, rx, btnu_d, btnd_d, btnl_d, btnr_d, btnc_d,
		kbd_char, kbd_new_c, kbd_char_c,
		kbd_new_edge,

		vga_dout,

		rx_data_n,
		rx_fifo_empty,
		rx_fifo_full,

		tx_fifo_full,
		tx_fifo_empty,

		-- lfsr_o, 

		timer_control_o,
		timer_counter_o)
	begin
		ld <= ld_c;
		ld_n <= ld_c;

		tx_data_we <= '0';
		rx_data_re <= '0';

		vga_control_we <= vga_control_registers_we_initialize;

		timer_control_we <= '0';

		cpu_irc_mask_we <= '0';

		leds_reg_we <= '0';

		if kbd_new_edge = '1' then
			kbd_new_n  <= '1';
			kbd_char_n <= kbd_char;
		else
			kbd_new_n  <= kbd_new_c;
			kbd_char_n <= kbd_char_c;
		end if;

		io_din <= (others => '0');

		cpu_irc_mask     <= io_dout(number_of_interrupts - 1 downto 0);
		timer_control_i  <= io_dout;
		vga_control.crx  <= io_dout(6 downto 0);
		vga_control.cry  <= io_dout(13 downto 8);
		vga_control.ctl  <= io_dout(vga_control.ctl'range);
		leds_reg         <= io_dout;
		tx_data          <= io_dout(tx_data'range);
		-- lfsr_i           <= io_dout;
		-- lfsr_i_we        <= '1';

		-- @note a 32 bit ALU could be added as an I/O port for
		-- multiplication and addition at least. This should speed
		-- up the CPU.
		if io_re = '1' and io_daddr(15) = '0' then
			-- Get input.
			case io_daddr(2 downto 0) is
			when "000" => -- buttons, plus direct access to UART bit.
				io_din(7 downto 0) <= rx_data_n;
				io_din(8)          <= rx_fifo_empty;
				io_din(9)          <= rx_fifo_full;
				io_din(11)         <= tx_fifo_empty;
				io_din(12)         <= tx_fifo_full;

			when "001" => -- Switches and buttons
				io_din <= "00" & rx & btnu_d & btnd_d & btnl_d & btnr_d & btnc_d & sw_d;

			when "010" => -- VGA, Read VGA text buffer.
				-- @todo remove this register
				io_din <= timer_control_o;

			when "011" => -- Timer in
				io_din(timer_counter_o'range) <= timer_counter_o;

			when "100" => -- VGA dout
				io_din <= vga_dout;

			when "101" => -- PS/2 Keyboard, Check for new char
				io_din(6 downto 0) <= kbd_char_c;
				io_din(8)          <= kbd_new_c;
				kbd_new_n          <= '0';
			-- when "110" =>
			--	io_din             <= lfsr_o;

			when others => io_din <= (others => '0');
			end case;
		elsif io_wr = '1' and io_daddr(15) = '0' then
			-- Write output.
			case io_daddr(2 downto 0) is
			when "000" => -- UART
				tx_data_we <= io_dout(13);
				rx_data_re <= io_dout(10);

			when "001" => -- LEDs, next to switches.
				ld_n <= io_dout(7 downto 0);

			when "010" => -- General purpose timer
				timer_control_we <= '1';

			-- @todo Turn VGA display into 64x16 text display, then have the
			-- cursor track the VGA address register
			when "011" => -- VGA, cursor registers.
				vga_control_we.crx <= '1';
				vga_control_we.cry <= '1';
			when "100" => -- VGA, control register.
				vga_control_we.ctl <= '1';
			when "101" => -- LEDs
				leds_reg_we <= '1';
			when "110" => -- CPU Mask
				cpu_irc_mask_we <= '1';
			-- when "111" => 
			--	lfsr_i <= io_dout;
			--	lfsr_i_we <= '1';
			when others =>
			end case;
		end if;

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

	--- Timer ---------------------------------------------------------
	timer0_0: entity work.timer
	generic map(timer_length => timer_length)
	port map(
		clk       => clk,
		rst       => rst,
		we        => timer_control_we,
		control_i => timer_control_i,
		control_o => timer_control_o,
		counter_o => timer_counter_o,
		irq       => timer_irq,
		Q         => timer_q,
		NQ        => timer_nq);
	--- Timer ---------------------------------------------------------


	--- VGA -----------------------------------------------------------

	-- @todo The interface for reading from the VGA needs sorting
	-- it is currently unusable
	vga: block 
		signal vga_din_we_d: std_logic := '0';
		signal vga_we_ram:   std_logic := '0';
		signal vga_addr_we:  std_logic := '0';
		signal vga_din_we:   std_logic := '0';
		signal vga_addr:     std_logic_vector(12 downto 0) := (others => '0');
		signal vga_din:      std_logic_vector(15 downto 0) := (others => '0');
	begin 
		-- vga_din_we   <= '1' when io_wr = '1' and io_re = '0' and io_daddr(15) = '1' else '0';
		vga_din_we   <= '1' when io_wr = '1' and io_daddr(15) = '1' else '0';
		vga_addr_we  <= vga_din_we;
		vga_din_we_d <= vga_din_we;
		vga_addr     <= io_daddr(12 downto 0);
		vga_din      <= io_dout;

		delay: process(clk, rst)
		begin
			if rst = '1' then
				vga_we_ram <= '0';
			elsif rising_edge(clk) then
				vga_we_ram <= vga_din_we_d;
			end if;
		end process;

		vga_0: entity work.vga_top
		port map(
			clk        => clk,
			clk25MHz   => clk25MHz,
			rst        => rst,

			i_vga_control    => vga_control,
			i_vga_control_we => vga_control_we,

			vga_we_ram  => vga_we_ram,
			vga_addr_we => vga_addr_we,
			vga_din_we  => vga_din_we,
			vga_dout    => vga_dout,
			vga_din     => vga_din,
			vga_addr    => vga_addr,

			o_vga      => o_vga);
	end block;

	--- VGA -----------------------------------------------------------

	--- PS/2 ----------------------------------------------------------

	-- Process a kbd_new into a single edge for the rest of the
	-- system
	ps2_edge_new_character_0: entity work.edge
	port map(
		clk    => clk,
		rst    => rst,
		sin    => kbd_new,
		output => kbd_new_edge);

	ps2_0: work.kbd_pkg.ps2_kbd_top
	generic map(
		clock_frequency => clock_frequency,
		ps2_debounce_counter_size => 8)
	port map(
		clk        => clk,
		ps2_clk    => ps2_keyboard_clk,
		ps2_data   => ps2_keyboard_data,
		ascii_new  => kbd_new,
		ascii_code => kbd_char);
	--- PS/2 ----------------------------------------------------------

	--- LED 8 Segment display -----------------------------------------
	segments: block
		constant number_of_led_displays: positive := 4;
		signal leds: led_8_segment_displays_interface(number_of_led_displays - 1 downto 0) := (others => led_8_segment_display_default);

		signal leds_reg_o: std_logic_vector(15 downto 0) := (others => '0');
		signal leds_reg_we_o: std_logic := '0';
	begin
		segment_reg: entity work.reg
			generic map(N => 16)
			port map(
				clk => clk,
				rst => rst,
				we  => leds_reg_we,
				di  => leds_reg,
				do  => leds_reg_o);

		segment_reg_re: entity work.reg
			generic map(N => 1)
			port map(
				clk   => clk,
				rst   => rst,
				we    => '1',
				di(0) => leds_reg_we,
				do(0) => leds_reg_we_o);

		leds(0).display <= leds_reg_o(15 downto 12);
		leds(0).we      <= leds_reg_we_o;
		leds(1).display <= leds_reg_o(11 downto 8);
		leds(1).we      <= leds_reg_we_o;
		leds(2).display <= leds_reg_o(7 downto 4);
		leds(2).we      <= leds_reg_we_o;
		leds(3).display <= leds_reg_o(3 downto 0);
		leds(3).we      <= leds_reg_we_o;

		ledseg_0: entity work.led_8_segment_display
		generic map(
			number_of_led_displays => number_of_led_displays,
			clock_frequency        => clock_frequency,
			use_bcd_not_hex        => false)
		port map(
			clk        => clk,
			rst        => rst,

			leds       => leds,

			an         => an,
			ka         => ka);
	end block;
	--- LED 8 Segment display -----------------------------------------

	--- Buttons -------------------------------------------------------

	btnu_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnu, result => btnu_d);
	btnd_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnd, result => btnd_d);
	btnc_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnc, result => btnc_d);
	btnl_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnl, result => btnl_d);
	btnr_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnr, result => btnr_d);

	--- Buttons -------------------------------------------------------

	--- Switches ------------------------------------------------------

	sw_debouncer: for i in sw'range generate
		sw_d_instance: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => sw(i), result => sw_d(i));
	end generate;

	--- Switches ------------------------------------------------------

	--- LFSR ----------------------------------------------------------

	-- lfsr_0: entity work.lfsr generic map(tap => lfsr_tap) port map(clk => clk, rst => rst, ce => '1', di => lfsr_i, we => lfsr_i_we, do => lfsr_o);

	--- LFSR ----------------------------------------------------------
-------------------------------------------------------------------------------
end architecture;


