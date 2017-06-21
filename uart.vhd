--------------------------------------------------------------------------------
--| @file uart.vhd
--| @brief implements a universal asynchronous receiver transmitter with
--| parameterisable baud rate. tested on a spartan 6 lx9 connected to a
--| silicon labs cp210 usb-uart bridge.
--|
--| @author         peter a bennett
--| @copyright      (c) 2012 peter a bennett
--| @license        Apache 2.0
--| @email          pab850@googlemail.com
--| @contact        www.bytebash.com
--|
--| See https://github.com/pabennett/uart
--|
--| There have been many changes to the original code, consult the git logs
--| for a full list of changes.
--|
--| @note Changes made to range to stop Xilinx warnings and with formatting
--| @todo A better interface to the outside world should be created, instead
--| of using strobe/acknowledge the module should indicate whether a new
--| character is available (level high until acknowledged) and it should indicate
--| when a write can be done (level high when a write is in operation).
--| @note Somewhere along the chain from the computer, to the Nexys3 board,
--| to the UART module, and finally to the H2 core, bytes are being lost in
--| transmission to from the computer. This UART really should be buffered
--| as well.
--|
--|  START 0 1 2 3 4 5 6 7 STOP
--| ----\_/-|-|-|-|-|-|-|-|-|-------
--|
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package uart_pkg is

	component uart_top is
	generic (baud_rate: positive; clock_frequency: positive);
	port (  
		clk:                 in      std_logic;
		rst:                 in      std_logic;

		rx_data:             out     std_logic_vector(7 downto 0);
		rx_fifo_empty:       out     std_logic;
		rx_fifo_full:        out     std_logic;
		rx_re:               in      std_logic;

		tx_data:             in      std_logic_vector(7 downto 0);
		tx_fifo_full:        out     std_logic;
		tx_fifo_empty:       out     std_logic;
		tx_we:               in      std_logic;

		tx:                  out     std_logic;
		rx:                  in      std_logic);

	end component;

	component uart_core is
		generic (baud_rate: positive; clock_frequency: positive);
		port (  
			clk:                 in      std_logic;
			rst:                 in      std_logic;
			data_stream_in:      in      std_logic_vector(7 downto 0);
			data_stream_in_stb:  in      std_logic;
			data_stream_in_ack:  out     std_logic := '0';

			data_stream_out:     out     std_logic_vector(7 downto 0);
			data_stream_out_stb: out     std_logic;
			data_stream_out_ack: in      std_logic;
			tx:                  out     std_logic;
			rx:                  in      std_logic);
	end component;

end package;

--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.fifo;
use work.uart_pkg.uart_core;

entity uart_top is
	generic (baud_rate: positive; clock_frequency: positive);
	port (  
		clk:                 in      std_logic;
		rst:                 in      std_logic;

		rx_data:             out     std_logic_vector(7 downto 0);
		rx_fifo_empty:       out     std_logic;
		rx_fifo_full:        out     std_logic;
		rx_re:               in      std_logic;

		tx_data:             in      std_logic_vector(7 downto 0);
		tx_fifo_full:        out     std_logic;
		tx_fifo_empty:       out     std_logic;
		tx_we:               in      std_logic;

		tx:                  out     std_logic;
		rx:                  in      std_logic);
end entity;

architecture behav of uart_top is
	signal rx_sync, rx_uart, tx_uart: std_logic := '0';

	signal data_stream_in:      std_logic_vector(7 downto 0) := (others => '0');
	signal data_stream_in_stb:  std_logic := '0';
	signal data_stream_in_ack:  std_logic := '0';
	signal data_stream_out:     std_logic_vector(7 downto 0) := (others => '0');
	signal data_stream_out_stb: std_logic := '0';
	signal data_stream_out_ack: std_logic := '0';

begin
	uart_deglitch: process (clk)
	begin
		if rising_edge(clk) then
			rx_sync <= rx;
			rx_uart <= rx_sync;
			tx      <= tx_uart;
		end if;
	end process;

-- 	tx_fifo: work.util.fifo 
-- 		generic map (
-- 			data_width => 8, 
-- 			fifo_depth => 8)
-- 		port map(
-- 			clk   => clk, 
-- 			rst   => rst, 
-- 			din   => data_stream_out, 
-- 			we    => data_stream_out_stb, 
-- 			re    => rx_re, 
-- 			do    => rx_data, 
-- 			full  => rx_fifo_full, 
-- 			empty => rx_fifo_empty);
 


	rx_fifo: work.util.fifo 
		generic map (
			data_width => 8, 
			fifo_depth => 8)
		port map(
			clk   => clk, 
			rst   => rst, 
			din   => data_stream_out, 
			we    => data_stream_out_stb, 
			re    => rx_re, 
			do    => rx_data, 
			full  => rx_fifo_full, 
			empty => rx_fifo_empty);

	process(clk) 
	begin
		if rising_edge(clk) then
			data_stream_in_ack <= '0';
			if data_stream_out_stb = '1' then
				data_stream_in_ack <= '1';
			end if;
		end if;
	end process;

	uart: work.uart_pkg.uart_core
		generic map(
			baud_rate => baud_rate,
			clock_frequency => clock_frequency)
		port map(
			clk                 => clk,
			rst                 => rst,
			data_stream_in      => data_stream_in,
			data_stream_in_stb  => data_stream_in_stb,
			data_stream_in_ack  => data_stream_in_ack,
			data_stream_out     => data_stream_out,
			data_stream_out_stb => data_stream_out_stb,
			data_stream_out_ack => data_stream_out_ack,
			rx                  => rx_uart,
			tx                  => tx_uart);


end;
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_core is
	generic (baud_rate: positive; clock_frequency: positive);
	port (  
	        clk:                 in      std_logic;
	        rst:                 in      std_logic;
	        data_stream_in:      in      std_logic_vector(7 downto 0);
	        data_stream_in_stb:  in      std_logic;
	        data_stream_in_ack:  out     std_logic := '0';
	        data_stream_out:     out     std_logic_vector(7 downto 0);
	        data_stream_out_stb: out     std_logic;
	        data_stream_out_ack: in      std_logic;
	        tx:                  out     std_logic;
	        rx:                  in      std_logic);
end entity;

architecture behav of uart_core is
	
	constant uart_tx_count_max: positive := 7;
	constant uart_rx_count_max: positive := 7;
	----------------------------------------------------------------------------
	-- baud generation
	----------------------------------------------------------------------------
	constant c_tx_divider_val: integer := clock_frequency / baud_rate;
	constant c_rx_divider_val: integer := clock_frequency / (baud_rate * 16);

	signal baud_counter:            integer range 0 to c_tx_divider_val;
	signal baud_tick:               std_logic := '0';
	signal oversample_baud_counter: integer range 0 to c_rx_divider_val := 0;
	signal oversample_baud_tick:    std_logic := '0';

	----------------------------------------------------------------------------
	-- transmitter signals
	----------------------------------------------------------------------------
	type    uart_tx_states is (idle,
				wait_for_tick,
				send_start_bit,
				transmit_data,
				send_stop_bit);
	
	signal  uart_tx_state: uart_tx_states := idle;
	
	signal  uart_tx_data_block:  std_logic_vector(7 downto 0) := (others => '0');
	signal  uart_tx_data:        std_logic := '1';
	signal  uart_tx_count:       integer range 0 to uart_tx_count_max := 0;
	signal  uart_rx_data_in_ack: std_logic := '0';
	----------------------------------------------------------------------------
	-- receiver signals
	----------------------------------------------------------------------------
	type    uart_rx_states is (rx_wait_start_synchronise,
	                            rx_get_start_bit,
	                            rx_get_data,
	                            rx_get_stop_bit,
	                            rx_send_block);
	
	signal  uart_rx_state:        uart_rx_states := rx_get_start_bit;
	signal  uart_rx_bit:          std_logic := '1'; -- @note should the be 0 or 1?
	signal  uart_rx_data_block:   std_logic_vector(7 downto 0) := (others => '0');
	signal  uart_rx_data_vec:     std_logic_vector(1 downto 0) := (others => '0');
	signal  uart_rx_filter:       unsigned(1 downto 0)  := (others => '1');
	signal  uart_rx_count:        integer range 0 to uart_rx_count_max  := 0;
	signal  uart_rx_data_out_stb: std_logic := '0';
	signal  uart_rx_bit_spacing:  unsigned (3 downto 0) := (others => '0');
	signal  uart_rx_bit_tick:     std_logic := '0';
begin

	data_stream_in_ack  <= uart_rx_data_in_ack;
	data_stream_out     <= uart_rx_data_block;
	data_stream_out_stb <= uart_rx_data_out_stb;
	tx                  <= uart_tx_data;

	-- the input clk is 100MHz, this needs to be divided down to the
	-- rate dictated by the baud_rate. for example, if 115200 baud is selected
	-- (115200 baud = 115200 bps - 115.2kbps) a tick must be generated once
	-- every 1/115200
	tx_clk_divider: process (clk, rst)
	begin
		if rst = '1' then
			baud_counter <= 0;
			baud_tick    <= '0';
		elsif rising_edge (clk) then
				if baud_counter = c_tx_divider_val then
					baud_counter <= 0;
					baud_tick    <= '1';
				else
					baud_counter <= baud_counter + 1;
					baud_tick    <= '0';
				end if;
		end if;
	end process;

	-- get data from data_stream_in and send it one bit at a time
	-- upon each baud tick. lsb first.
	-- wait 1 tick, send start bit (0), send data 0-7, send stop bit (1)
	uart_send_data:	process(clk, rst)
	begin
		if rst = '1' then 
			uart_tx_data        <= '1';
			uart_tx_data_block  <= (others => '0');
			uart_tx_count       <= 0;
			uart_tx_state       <= idle;
			uart_rx_data_in_ack <= '0';
		elsif rising_edge(clk) then
			uart_rx_data_in_ack <= '0';
			case uart_tx_state is
			when idle =>
				if data_stream_in_stb = '1' then
					uart_tx_data_block  <= data_stream_in;
					uart_rx_data_in_ack <= '1';
					uart_tx_state       <= wait_for_tick;
				end if;
			when wait_for_tick =>
				if baud_tick = '1' then
					uart_tx_state	 <= send_start_bit;
				end if;
			when send_start_bit =>
				if baud_tick = '1' then
					uart_tx_data  <= '0';
					uart_tx_state <= transmit_data;
					uart_tx_count <= 0;
				end if;
			when transmit_data =>
				if baud_tick = '1' then
					if uart_tx_count < uart_tx_count_max then
						uart_tx_data  <= uart_tx_data_block(uart_tx_count);
						uart_tx_count <= uart_tx_count + 1;
					else
						uart_tx_data  <= uart_tx_data_block(7);
						uart_tx_count <= 0;
						uart_tx_state <= send_stop_bit;
					end if;
				end if;
			when send_stop_bit =>
				if baud_tick = '1' then
					uart_tx_data <= '1';
					uart_tx_state <= idle;
				end if;
			when others =>
				uart_tx_data <= '1';
				uart_tx_state <= idle;
			end case;
		end if;
	end process;

	-- generate an oversampled tick (baud * 16)
	oversample_clk_divider: process (clk, rst)
	begin
		if rst = '1' then
			oversample_baud_counter <= 0;
			oversample_baud_tick    <= '0';
		elsif rising_edge (clk) then
			if oversample_baud_counter = c_rx_divider_val then
				oversample_baud_counter <= 0;
				oversample_baud_tick    <= '1';
			else
				oversample_baud_counter <= oversample_baud_counter + 1;
				oversample_baud_tick    <= '0';
			end if;
		end if;
	end process;

	-- synchronise rxd to the oversampled baud
	rxd_synchronise: process(clk, rst)
	begin
		if rst = '1' then
			uart_rx_data_vec <= (others => '0');
		elsif rising_edge(clk) then
			if oversample_baud_tick = '1' then
				uart_rx_data_vec(0) <= rx;
				uart_rx_data_vec(1) <= uart_rx_data_vec(0);
			end if;
		end if;
	end process;

	-- filter rxd with a 2 bit counter.
	rxd_filter: process(clk, rst)
	begin
		if rst = '1' then
			uart_rx_filter <= (others => '1');
			uart_rx_bit <= '1';
		elsif rising_edge(clk) then
			if oversample_baud_tick = '1' then
				-- filter rxd.
				if uart_rx_data_vec(1) = '1' and uart_rx_filter < 3 then
					uart_rx_filter <= uart_rx_filter + 1;
				elsif uart_rx_data_vec(1) = '0' and uart_rx_filter > 0 then
					uart_rx_filter <= uart_rx_filter - 1;
				end if;
				-- set the rx bit.
				if uart_rx_filter = 3 then
					uart_rx_bit <= '1';
				elsif uart_rx_filter = 0 then
					uart_rx_bit <= '0';
				end if;
			end if;
		end if;
	end process;

	rx_bit_spacing: process (clk, rst)
	begin
		if rising_edge(clk) then
			uart_rx_bit_tick <= '0';
			if oversample_baud_tick = '1' then
				if uart_rx_bit_spacing = 15 then
					uart_rx_bit_tick <= '1';
					uart_rx_bit_spacing <= (others => '0');
				else
					uart_rx_bit_spacing <= uart_rx_bit_spacing + 1;
				end if;
				if uart_rx_state = rx_get_start_bit then
					uart_rx_bit_spacing <= (others => '0');
				end if;
			end if;
		end if;
	end process;

	uart_receive_data: process(clk, rst)
	begin
		if rst = '1' then
			uart_rx_state        <= rx_get_start_bit;
			uart_rx_data_block   <= (others => '0');
			uart_rx_count        <= 0;
			uart_rx_data_out_stb <= '0';
		elsif rising_edge(clk) then
			case uart_rx_state is
			when rx_get_start_bit =>
				if oversample_baud_tick = '1' and uart_rx_bit = '0' then
					uart_rx_state <= rx_get_data;
				end if;
			when rx_get_data =>
				if uart_rx_bit_tick = '1' then
					if uart_rx_count < uart_rx_count_max then
						uart_rx_data_block(uart_rx_count) <= uart_rx_bit;
						uart_rx_count <= uart_rx_count + 1;
					else
						uart_rx_data_block(7) <= uart_rx_bit;
						uart_rx_count <= 0;
						uart_rx_state <= rx_get_stop_bit;
					end if;
				end if;
			when rx_get_stop_bit =>
				if uart_rx_bit_tick = '1' then
					if uart_rx_bit = '1' then
						uart_rx_state        <= rx_send_block;
						uart_rx_data_out_stb <= '1';
					end if;
				end if;
			when rx_send_block =>
				if data_stream_out_ack = '1' then
					uart_rx_data_out_stb <= '0';
					uart_rx_data_block   <= (others => '0');
					uart_rx_state        <= rx_get_start_bit;
				else
					uart_rx_data_out_stb  <= '1';
				end if;
			when others =>
				uart_rx_state <= rx_get_start_bit;
			end case;
		end if;
	end process;
end;

