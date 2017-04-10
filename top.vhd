---------------------------------------------------------------------------------
--! @file top.vhd
--! @brief This file is the top level of the project.
--!  It presents an interface between the CPU,
--!  RAM, and all the I/O modules.
--!
--! @author     Richard James Howe.
--! @copyright  Copyright 2013 Richard James Howe.
--! @license    MIT
--! @email      howe.r.j.89@gmail.com
---------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	generic(
		clock_frequency:      positive := 100000000;
		number_of_interrupts: positive := 8;
		uart_baud_rate:       positive := 115200
	);
	port
	(
-- synthesis translate_off
		-- These signals are only used for the test bench
		debug_irq:        in  std_logic;
		debug_irc:        in  std_logic_vector(3 downto 0);

		debug_pc:         out std_logic_vector(12 downto 0);
		debug_insn:       out std_logic_vector(15 downto 0);
		debug_mem_dwe:    out std_logic := '0';
		debug_mem_din:    out std_logic_vector(15 downto 0);
		debug_mem_dout:   out std_logic_vector(15 downto 0);
		debug_mem_daddr:  out std_logic_vector(12 downto 0);
-- synthesis translate_on

		clk:      in  std_logic                    :=      'X';  -- clock
--              rst:      in  std_logic                    :=      'X';
--		cpu_wait: in  std_logic                    :=      'X';  -- CPU wait
		-- Buttons
		btnu:     in  std_logic                    :=      'X';  -- button up
		btnd:     in  std_logic                    :=      'X';  -- button down
		btnc:     in  std_logic                    :=      'X';  -- button centre
		btnl:     in  std_logic                    :=      'X';  -- button left
		btnr:     in  std_logic                    :=      'X';  -- button right
		-- Switches
		sw:       in  std_logic_vector(7 downto 0) :=      (others => 'X'); -- switches
		-- Simple LED outputs
		an:       out std_logic_vector(3 downto 0) :=      (others => '0'); -- anodes   7 segment display
		ka:       out std_logic_vector(7 downto 0) :=      (others => '0'); -- kathodes 7 segment display

		ld:       out std_logic_vector(7 downto 0) :=      (others => '0'); -- leds
		-- UART
		rx:       in  std_logic                    :=      'X';  -- uart rx
		tx:       out std_logic                    :=      '0';  -- uart tx
		-- VGA
		red:      out std_logic_vector(2 downto 0) :=      (others => '0');
		green:    out std_logic_vector(2 downto 0) :=      (others => '0');
		blue:     out std_logic_vector(1 downto 0) :=      (others => '0');
		hsync:    out std_logic                    :=      '0';
		vsync:    out std_logic                    :=      '0';
		-- PWM from timer
		gpt0_q:   out std_logic                    :=      '0';
		gpt0_nq:  out std_logic                    :=      '0';
		-- PS/2 Interface
		ps2_keyboard_data:  in std_logic           :=      '0';
		ps2_keyboard_clk:   in std_logic           :=      '0'

	);
end;

architecture behav of top is
	-- Signals
	signal rst: std_logic := '0';
	-- CPU H2 IO interface signals.
	signal cpu_wait:     std_logic := '0';
	signal cpu_io_wr:    std_logic := '0';
	signal cpu_io_re:    std_logic := '0';
	signal cpu_io_din:   std_logic_vector(15 downto 0):= (others => '0');
	signal cpu_io_dout:  std_logic_vector(15 downto 0):= (others => '0');
	signal cpu_io_daddr: std_logic_vector(15 downto 0):= (others => '0');
	-- CPU H2 Interrupts
	signal cpu_irq: std_logic := '0';
	signal cpu_irc: std_logic_vector(number_of_interrupts - 1 downto 0):= (others => '0');

	signal clk25MHz: std_logic:= '0';
	signal clk50MHz: std_logic:= '0';

	attribute buffer_type : string;
	attribute buffer_type of clk50MHz : signal is "BUFG";
	attribute buffer_type of clk25MHz : signal is "BUFG";

	-- Basic IO register
	---- LEDs/Switches

	signal ld_c,ld_n: std_logic_vector(7 downto 0):=  (others => '0');

	---- VGA
	signal crx_we: std_logic :=  '0';
	signal cry_we: std_logic :=  '0';
	signal ctl_we: std_logic :=  '0';

	signal crx: std_logic_vector(6 downto 0):=  (others => '0');
	signal cry: std_logic_vector(5 downto 0):=  (others => '0');
	signal ctl: std_logic_vector(7 downto 0):=  (others => '0');
	signal vga_we_ram: std_logic :=  '0';
	signal vga_a_we:   std_logic :=  '0';
	signal vga_d_we:   std_logic :=  '0';
	signal vga_addr:   std_logic_vector(12 downto 0):= (others => '0');
	signal vga_dout:   std_logic_vector(15 downto 0) := (others => '0');
	signal vga_din:    std_logic_vector(15 downto 0) := (others => '0');

	---- UART
	-- @todo move this to its own module
	signal uart_din_c, uart_din_n:   std_logic_vector(7 downto 0) := (others => '0');
	signal ack_din_c, ack_din_n:     std_logic:= '0';
	signal uart_dout_c, uart_dout_n: std_logic_vector(7 downto 0):= (others => '0');
	signal stb_dout_c, stb_dout_n:   std_logic:= '0';
	signal uart_din, uart_dout:      std_logic_vector(7 downto 0):= (others => '0');
	signal stb_din, stb_dout:        std_logic:= '0';
	signal ack_din, ack_dout:        std_logic:= '0';
	signal tx_uart, rx_uart,rx_sync: std_logic:= '0';

	---- Timer
	signal gpt0_ctr_r_we:     std_logic := '0';
	signal gpt0_ctr_r:        std_logic_vector(15 downto 0) := (others =>'0');
	signal gpt0_irq_comp1:    std_logic;
	signal gpt0_q_internal:   std_logic;
	signal gpt0_nq_internal:  std_logic;

	---- PS/2
	signal kbd_new:      std_logic := '0';  -- new ASCII char available
	signal kbd_new_edge: std_logic := '0';
	signal kbd_char:     std_logic_vector(6 downto 0); -- ASCII char
	signal kbd_new_c, kbd_new_n: std_logic := '0';
	signal kbd_char_c, kbd_char_n:  std_logic_vector(6 downto 0) := (others => '0'); -- ASCII char

	---- 8 Segment Display
	signal led_0:    std_logic_vector(7 downto 0) := (others => '0'); 
	signal led_1:    std_logic_vector(7 downto 0) := (others => '0'); 
	signal led_2:    std_logic_vector(7 downto 0) := (others => '0'); 
	signal led_3:    std_logic_vector(7 downto 0) := (others => '0'); 
	signal led_0_we: std_logic := '0';
	signal led_1_we: std_logic := '0';
	signal led_2_we: std_logic := '0';
	signal led_3_we: std_logic := '0';

	---- Buttons
	signal btnu_d: std_logic := '0';  -- button up
	signal btnd_d: std_logic := '0';  -- button down
	signal btnc_d: std_logic := '0';  -- button centre
	signal btnl_d: std_logic := '0';  -- button left
	signal btnr_d: std_logic := '0';  -- button right

begin
-------------------------------------------------------------------------------
-- The Main components
-------------------------------------------------------------------------------

------- Output assignments (Not in a process) ---------------------------------
	rst        <= '0';
	cpu_irc(0) <= '0';
	cpu_irc(1) <= gpt0_irq_comp1;
	cpu_irc(2) <= ack_din;
	cpu_irc(3) <= stb_dout;
	-- @todo These interrupts should come from debounced button inputs
	-- until something more useful is found for them. Whether they
	-- act as interrupts or not should be controlled by a register
	-- set by the user.
	cpu_irc(number_of_interrupts -1 downto 4) <= (others => '0');
	cpu_wait   <= '0';
	cpu_irq    <= '1' when gpt0_irq_comp1 = '1' or ack_din = '1' or stb_dout = '1' else '0';

	cpu_0: entity work.cpu
	generic map(number_of_interrupts => number_of_interrupts)
	port map(
-- synthesis translate_off
	debug_pc        => debug_pc,
	debug_insn      => debug_insn,
	debug_mem_dwe   => debug_mem_dwe,
	debug_mem_din   => debug_mem_din,
	debug_mem_dout  => debug_mem_dout,
	debug_mem_daddr => debug_mem_daddr,
-- synthesis translate_on

	clk => clk,
	rst => rst,

	cpu_wait  => cpu_wait,
	cpu_wr    => cpu_io_wr,
	cpu_re    => cpu_io_re,
	cpu_din   => cpu_io_din,
	cpu_dout  => cpu_io_dout,
	cpu_daddr => cpu_io_daddr,

	cpu_irq   => cpu_irq,
	cpu_irc   => cpu_irc);

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
			ld_c        <=  (others => '0');
			-- UART
			uart_din_c  <=  (others => '0');
			ack_din_c   <=  '0';
			stb_dout_c  <=  '0';

			-- PS/2
			kbd_char_c <= (others => '0');
			kbd_new_c  <= '0';
		elsif rising_edge(clk) then
			-- LEDs/Switches
			ld_c        <=  ld_n;
			-- UART
			uart_din_c  <=  uart_din_n;
			ack_din_c   <=  ack_din_n;
			uart_dout_c <=  uart_dout_n;
			stb_dout_c  <=  stb_dout_n;
			-- PS/2
			kbd_char_c  <= kbd_char_n;
			kbd_new_c   <= kbd_new_n;
		end if;
	end process;

	io_select: process(
		cpu_io_wr, cpu_io_re, cpu_io_dout, cpu_io_daddr,
		ld_c,
		sw, rx, btnu_d, btnd_d, btnl_d, btnr_d, btnc_d,
		uart_din_c, ack_din_c,
		uart_dout_c,
		uart_dout, stb_dout, ack_din,
		stb_dout, stb_dout_c, vga_dout,
		kbd_char, kbd_new_c, kbd_char_c,
		kbd_new_edge,

		gpt0_ctr_r_we)
	begin
		-- Outputs
		ld <= ld_c;

		uart_din <= uart_din_c;
		stb_din  <= '0';
		ack_dout <= '0';

		-- LEDs
		led_0 <= (others => '0');
		led_1 <= (others => '0');
		led_2 <= (others => '0');
		led_3 <= (others => '0');

		led_0_we <= '0';
		led_1_we <= '0';
		led_2_we <= '0';
		led_3_we <= '0';

		-- Register defaults
		ld_n <= ld_c;

		-- VGA
		crx_we <= '0';
		cry_we <= '0';
		ctl_we <= '0';
		crx <= (others => '0');
		cry <= (others => '0');
		ctl <= (others => '0');

		vga_we_ram <= '0';
		vga_a_we   <= '0';
		vga_d_we   <= '0';
		vga_din    <= (others => '0');
		vga_addr   <= (others => '0');

		uart_din_n  <=  uart_din_c;

		-- General Purpose Timer
		gpt0_ctr_r_we <= '0';
		gpt0_ctr_r <= (others => '0');

		if kbd_new_edge = '1' then
			kbd_new_n  <= '1';
			kbd_char_n <= kbd_char;
		else
			kbd_new_n  <= kbd_new_c;
			kbd_char_n <= kbd_char_c;
		end if;

		if ack_din = '1' then
			ack_din_n <= '1';
		else
			ack_din_n <= ack_din_c;
		end if;

		if stb_dout = '1' then
			stb_dout_n  <= '1';
			uart_dout_n <= uart_dout;
			ack_dout    <= '1';
		else
			uart_dout_n <=  uart_dout_c;
			stb_dout_n  <= stb_dout_c;
		end if;

		cpu_io_din <= (others => '0');

		if cpu_io_wr = '1' then
			-- Write output.
			case cpu_io_daddr(4 downto 0) is
			when "00000" => -- Not used!
			when "00001" => -- LEDs, next to switches.
				ld_n <= cpu_io_dout(7 downto 0);
			when "00010" => -- VGA, cursor registers.
				crx_we <= '1';
				cry_we <= '1';
				crx <= cpu_io_dout(6 downto 0);
				cry <= cpu_io_dout(13 downto 8);
			when "00011" => -- VGA, control register.
				ctl_we <= '1';
				ctl <= cpu_io_dout(7 downto 0);
			when "00100" => -- VGA update address register.
				vga_a_we <= '1';
				vga_addr <= cpu_io_dout(12 downto 0);
			when "00101" => -- VGA, update register.
				vga_d_we <= '1';
				vga_din <= cpu_io_dout(15 downto 0);
			when "00110" => -- VGA write RAM write
				vga_we_ram <= '1';
			when "00111" => -- UART write output.
				uart_din_n <= cpu_io_dout(7 downto 0);
			when "01000" => -- UART strobe input.
				stb_din <= '1';
			when "01001" => -- UART acknowledge output.
				ack_dout <= '1';
			when "01010" => -- General purpose timer
				gpt0_ctr_r_we <= '1';
				gpt0_ctr_r    <= cpu_io_dout(15 downto 0);
			when "01011" => -- LED 8 Segment display
				led_0    <= cpu_io_dout(7 downto 0);
				led_0_we <= '1';
			when "01100" => -- LED 8 Segment display
				led_1    <= cpu_io_dout(7 downto 0);
				led_1_we <= '1';
			when "01101" =>
				led_2    <= cpu_io_dout(7 downto 0);
				led_2_we <= '1';
			when "01110" =>
				led_3    <= cpu_io_dout(7 downto 0);
				led_3_we <= '1';
			when "01111" =>
			when "10000" =>
			when others =>
			end case;
		elsif cpu_io_re = '1' then
			-- Get input.
			case cpu_io_daddr(3 downto 0) is
			when "0000" => -- Switches, plus direct access to UART bit.
				cpu_io_din <= "0000000000" & rx & btnu_d & btnd_d & btnl_d & btnr_d & btnc_d;
			when "0001" =>
				cpu_io_din <= X"00" & sw;
			when "0010" => -- VGA, Read VGA text buffer.
				cpu_io_din <= vga_dout;
			when "0011" => -- UART get input.
				cpu_io_din <= X"00" & uart_dout_c;
			when "0100" => -- UART acknowledged input.
				cpu_io_din <= (0 => ack_din_c, others => '0');
				ack_din_n <= '0';
			when "0101" => -- UART strobe output (write output).
				cpu_io_din <= (0 => stb_dout_c, others => '0');
				stb_dout_n <= '0';
			when "0110" =>  -- PS/2 Keyboard, Check for new char
				cpu_io_din <= (0 => kbd_new_c, others => '0');
			when "0111" =>  -- PS/2 ASCII In and ACK
				cpu_io_din <= "000000000" &  kbd_char_c;
				kbd_new_n <= '0';
			when "1000" => cpu_io_din <= (others => '0');
			when others => cpu_io_din <= (others => '0');
			end case;
		end if;
	end process;

	--- UART ----------------------------------------------------------
	--! @todo integrate this into the UART module
	uart_deglitch_0: process (clk)
	begin
	if rising_edge(clk) then
		rx_sync <= rx;
		rx_uart <= rx_sync;
		tx <= tx_uart;
	end if;
	end process;

	uart_0: entity work.uart
	generic map(
		baud_rate       => uart_baud_rate,
		clock_frequency => clock_frequency)
	port map(
		clock               => clk,
		reset               => rst,
		data_stream_in      => uart_din,
		data_stream_in_stb  => stb_din,
		data_stream_in_ack  => ack_din,
		data_stream_out     => uart_dout,
		data_stream_out_stb => stb_dout,
		data_stream_out_ack => ack_dout,
		rx                  => rx_uart,
		tx                  => tx_uart);
	--- UART ----------------------------------------------------------

	--- Timer ---------------------------------------------------------
	gpt0_q  <= gpt0_q_internal;
	gpt0_nq <= gpt0_nq_internal;
	timer0_0: entity work.timer
	generic map(timer_length => 16)
	port map(
		clk       => clk,
		rst       => rst,
		we        => gpt0_ctr_r_we,
		control   => gpt0_ctr_r,
		irq       => gpt0_irq_comp1,
		Q         => gpt0_q_internal,
		NQ        => gpt0_nq_internal);
	--- Timer ---------------------------------------------------------


	--- VGA -----------------------------------------------------------
	vga_0: entity work.vga_top
	port map(
		clk => clk,
		clk25MHz => clk25MHz,
		rst => rst,

		crx_we => crx_we,
		cry_we => cry_we,
		ctl_we => ctl_we,

		crx_oreg => crx,
		cry_oreg => cry,
		ctl_oreg => ctl,

		vga_we_ram => vga_we_ram,
		vga_a_we => vga_a_we,
		vga_d_we => vga_d_we,
		vga_dout => vga_dout,
		vga_din => vga_din,
		vga_addr => vga_addr,

		red => red,
		green => green,
		blue => blue,
		hsync => hsync,
		vsync => vsync);
	--- VGA -----------------------------------------------------------

	--- PS/2 ----------------------------------------------------------

	-- Process a kbd_new into a single edge for the rest of the
	-- system
	ps2_edge_new_character_0: entity work.edge
	port map(
		clk    => clk,
		sin    => kbd_new,
		output => kbd_new_edge);

	ps2_0: entity work.ps2top
	generic map(
		clk_freq => clock_frequency,
		ps2_debounce_counter_size => 8)
	port map(
		clk        => clk,
		ps2_clk    => ps2_keyboard_clk,
		ps2_data   => ps2_keyboard_data,
		ascii_new  => kbd_new,
		ascii_code => kbd_char);
	--- PS/2 ----------------------------------------------------------

	--- LED 8 Segment display -----------------------------------------
	-- @todo split writes to each segment up
	ledseg_0: entity work.ledseg
	port map(
		clk        => clk,
		rst        => rst,

		led_0      => led_0,
		led_1      => led_1,
		led_2      => led_2,
		led_3      => led_3,

		led_0_we   => led_0_we,
		led_1_we   => led_1_we,
		led_2_we   => led_2_we,
		led_3_we   => led_3_we,

		an         => an,
		ka         => ka);
	--- LED 8 Segment display -----------------------------------------

	--- Buttons -------------------------------------------------------

	btnu_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnu, result => btnu_d);
	btnd_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnd, result => btnd_d);
	btnc_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnc, result => btnc_d);
	btnl_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnl, result => btnl_d);
	btnr_d0: entity work.debounce generic map(counter_size => 20) port map(clk => clk, button => btnr, result => btnr_d);

	--- Buttons -------------------------------------------------------

-------------------------------------------------------------------------------
end architecture;

