--------------------------------------------------------------------------------- 
--! @file top_level.vhd
--! @brief This file is the top level of the project.
--!  It presents an interface between the CPU,
--!  RAM, and all the I/O modules.
--!
--!  TODO:
--!   * expand funcionality (colors, etc)
--!   * switch between different sections of memory with addr bit
--!   * integrate VGA memory with CPU core
--!
--! @author     Richard James Howe.
--! @copyright  Copyright 2013 Richard James Howe.
--! @license    LGPL    
--! @email      howe.r.j.89@gmail.com
--------------------------------------------------------------------------------- 

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top_level is
	generic(
	clock_frequency:      positive := 100000000;
	number_of_interrupts: positive := 4;
	uart_baud_rate:       positive := 115200
	);
	port
	(
-- synthesis translate_off

	-- This should be removed once testing is done in the interests of
	-- portability.
	debug_irq:        in  std_logic;
	debug_irc:        in  std_logic_vector(3 downto 0);

	debug_pc:         out std_logic_vector(12 downto 0);
	debug_insn:       out std_logic_vector(15 downto 0);
	debug_mem_dwe:    out std_logic:= '0';   
	debug_mem_din:    out std_logic_vector(15 downto 0);
	debug_mem_dout:   out std_logic_vector(15 downto 0);
	debug_mem_daddr:  out std_logic_vector(12 downto 0);
-- synthesis translate_on

	clk:      in  std_logic                    :=      'X';  -- clock
--  cpu_wait: in  std_logic                    :=      'X';  -- CPU wait
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

--    ps2_mouse_data:     in std_logic           :=      '0'; 
--    ps2_mouse_clk:      in std_logic           :=      '0'; 

--    pic_gpio:           in std_logic_vector(1 downto 0):= (others => 'X')
	);
end;

architecture behav of top_level is
	constant number_of_segments: positive :=  4; -- number of 8 Segment LED displays

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

	-- VGA interface signals
	signal clk25MHz: std_logic:= '0';
	signal clk50MHz: std_logic:= '0';
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
	signal ascii_new: std_logic := '0';  -- new ASCII char available
	signal ascii_new_c, ascii_new_n: std_logic := '0';
	signal ascii_code:  std_logic_vector(6 downto 0); -- ASCII char
	signal ascii_code_c, ascii_code_n:  std_logic_vector(6 downto 0) := (others => '0'); -- ASCII char

	---- 8 Segment Display
	signal led_0_1:    std_logic_vector(15 downto 0) := (others => '0'); -- leds 0 and 1
	signal led_2_3:    std_logic_vector(15 downto 0) := (others => '0'); -- leds 2 and 3
	signal led_0_1_we: std_logic := '0';
	signal led_2_3_we: std_logic := '0';
begin
------- Output assignments (Not in a process) ---------------------------------
	rst   <=  '0';
-------------------------------------------------------------------------------
-- The Main components
-------------------------------------------------------------------------------

-- synthesis translate_off
--  cpu_irq <= debug_irq;
--  cpu_irc <= debug_irc;
-- synthesis translate_on
	cpu_irq    <= gpt0_irq_comp1 or ack_din or stb_dout; -- or sig_1 .. or sig_n
--  cpu_irc(0) <= rst;
	cpu_irc(1) <= gpt0_irq_comp1;
	cpu_irc(2) <= ack_din;
	cpu_irc(3) <= stb_dout;

	-- Testing only -----------------
	-- cpu_wait <= btnc; -- Pause CPU
	---------------------------------

	cpu_instance: entity work.cpu
	generic map(
	number_of_interrupts => number_of_interrupts
	)
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
	cpu_irc   => cpu_irc
	);

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

	-- Xilinx Application Note:
	-- It seems like it buffers the clock correctly here, so no need to
	-- use a DCM.
	---- Clock divider /2. 
	clk50MHz <= '0' when rst = '1' else
	    not clk50MHz when rising_edge(clk);

	---- Clock divider /2. Pixel clock is 25MHz
	clk25MHz <= '0' when rst = '1' else
	    not clk25MHz when rising_edge(clk50MHz);
	---- End note.

	io_nextState: process(clk,rst)
	begin
	 if rst = '1' then
	   -- LEDs/Switches
	   ld_c        <=  (others => '0');
	   -- UART
	   uart_din_c  <=  (others => '0');
	   ack_din_c   <=  '0';
	   stb_dout_c  <=  '0';

	   -- PS/2
	   ascii_code_c <= (others => '0');
	   ascii_new_c  <= '0';
	 elsif rising_edge(clk) then
	   -- LEDs/Switches
	   ld_c        <=  ld_n;
	   -- UART
	   uart_din_c  <=  uart_din_n; 
	   ack_din_c   <=  ack_din_n;
	   uart_dout_c <=  uart_dout_n;
	   stb_dout_c  <=  stb_dout_n;
	   -- PS/2
	   ascii_code_c <= ascii_code_n;
	   ascii_new_c  <= ascii_new_n;
	 end if;
	end process;

	io_select: process(
	cpu_io_wr,cpu_io_re,cpu_io_dout,cpu_io_daddr,
	ld_c,
	sw,rx,btnu,btnd,btnl,btnr,btnc,
	uart_din_c, ack_din_c,
	uart_dout_c, 
	uart_dout, stb_dout, ack_din,
	stb_dout, stb_dout_c, vga_dout,
	ascii_new, ascii_code, ascii_new_c, ascii_code_c,

	gpt0_ctr_r_we
	)
	begin
	-- Outputs
	ld <= ld_c;

	uart_din <= uart_din_c;
	stb_din  <= '0';
	ack_dout <= '0';

	led_0_1 <= (others => '0');
	led_2_3 <= (others => '0');
	led_0_1_we <= '0';
	led_2_3_we <= '0';

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
	vga_a_we <= '0';
	vga_d_we <= '0';
	vga_din <= (others => '0');
	vga_addr <= (others => '0');

	uart_din_n  <=  uart_din_c; 

	-- General Purpose Timer
	gpt0_ctr_r_we <= '0';
	gpt0_ctr_r <= (others => '0');

	if ascii_new = '1' then
	  ascii_new_n <= '1';
	  ascii_code_n <= ascii_code;
	else
	  ascii_new_n <= ascii_new_c;
	  ascii_code_n <= ascii_code_c;
	end if;

	if ack_din = '1' then
	    ack_din_n <= '1';
	else
	    ack_din_n <= ack_din_c;
	end if;

	if stb_dout = '1' then
	    stb_dout_n <= '1';
	    uart_dout_n <= uart_dout;
	    ack_dout <= '1';
	else
	    uart_dout_n <=  uart_dout_c;
	    stb_dout_n <= stb_dout_c;
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
	      led_0_1 <= cpu_io_dout(15 downto 0);
	      led_0_1_we <= '1';
	    when "01100" => -- LED 8 Segment display
	      led_2_3 <= cpu_io_dout(15 downto 0);
	      led_2_3_we <= '1';
	    when "01101" =>
	    when "01110" =>
	    when "01111" =>
	    when "10000" =>
	    when others =>
	  end case;
	elsif cpu_io_re = '1' then
	  -- Get input.
	  case cpu_io_daddr(3 downto 0) is
	    when "0000" => -- Switches, plus direct access to UART bit.
	            cpu_io_din <= "0000000000" & rx & btnu & btnd & btnl & btnr & btnc;
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
	            cpu_io_din <= (0 => ascii_new_c, others => '0');
	    when "0111" =>  -- PS/2 ASCII In an ACK
	            cpu_io_din <= "000000000" &  ascii_code_c;
	            ascii_new_n <= '0';
	    when "1000" => cpu_io_din <= (others => '0');
	    when "1001" => cpu_io_din <= (others => '0');
	    when "1010" => cpu_io_din <= (others => '0');
	    when "1011" => cpu_io_din <= (others => '0');
	    when "1100" => cpu_io_din <= (others => '0');
	    when "1101" => cpu_io_din <= (others => '0');
	    when "1110" => cpu_io_din <= (others => '0');
	    when "1111" => cpu_io_din <= (others => '0');
	    when others => cpu_io_din <= (others => '0');
	  end case;
	end if;
	end process;

	--- UART ----------------------------------------------------------
	uart_deglitch: process (clk)
	begin
	  if rising_edge(clk) then
	      rx_sync <= rx;
	      rx_uart <= rx_sync;
	      tx <= tx_uart;
	  end if;
	end process;

	u_uart: entity work.uart 
	generic map(
	baud_rate => uart_baud_rate,
	clock_frequency => clock_frequency
	)
	port map(
	clock => clk,
	reset => rst,
	data_stream_in => uart_din,
	data_stream_in_stb => stb_din,
	data_stream_in_ack => ack_din,
	data_stream_out => uart_dout,
	data_stream_out_stb => stb_dout,
	data_stream_out_ack => ack_dout,
	rx => rx_uart,
	tx => tx_uart
	);
	--- UART ----------------------------------------------------------

	--- Timer ---------------------------------------------------------
	gpt0_q <= gpt0_q_internal;
	gpt0_nq <= gpt0_nq_internal;
	gptimer0_module: entity work.gptimer
	generic map(
	gptimerbits => 16
	)
	port map(
	clk       => clk,
	rst       => rst,
	ctrin_we  => gpt0_ctr_r_we,
	ctrin     => gpt0_ctr_r,
	irq       => gpt0_irq_comp1,
	Q         => gpt0_q_internal,
	NQ        => gpt0_nq_internal
	);
	--- Timer ---------------------------------------------------------


	--- VGA -----------------------------------------------------------
	vga_module: entity work.vga_top
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
	        vsync => vsync 
	);
	--- VGA -----------------------------------------------------------

	--- PS/2 ----------------------------------------------------------
	ps2_module: entity work.ps2_keyboard_to_ascii
	generic map(
	clk_freq => clock_frequency,
	ps2_debounce_counter_size => 9 -- ceil(log_2(clk_freq*desired_debouce_period))
	)
	port map(
	clk => clk,

	ps2_clk => ps2_keyboard_clk,
	ps2_data => ps2_keyboard_data,

	ascii_new => ascii_new,
	ascii_code => ascii_code
	);
	--- PS/2 ----------------------------------------------------------

	--- LED 8 Segment display -----------------------------------------
	ledseg_module: entity work.ledseg
	generic map(
	number_of_segments => number_of_segments, -- not used at the moment
	clk_freq => clock_frequency               -- --""--
	)
	port map(
	clk => clk,
	rst => rst,
	led_0_1 => led_0_1,
	led_2_3 => led_2_3,
	led_0_1_we => led_0_1_we,
	led_2_3_we => led_2_3_we,
	an => an,
	ka => ka
	);
	--- LED 8 Segment display -----------------------------------------

-------------------------------------------------------------------------------
end architecture;

