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
--| @note Changes made to range to stop Xilinx warnings and with formatting,
--| the UART has also been wrapped up in a package and top level component
--| (called "uart_top") to make the interface easier to use and less confusing.
--| This has not be tested yet.
--|
--| @note Somewhere along the chain from the computer, to the Nexys3 board,
--| to the UART module, and finally to the H2 core, bytes are being lost in
--| transmission from the computer. This UART really should be buffered
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
	generic (baud_rate: positive; clock_frequency: positive; fifo_depth: positive := 8);
	port (
		clk:                 in      std_ulogic;
		rst:                 in      std_ulogic;

		rx_data:             out     std_ulogic_vector(7 downto 0);
		rx_fifo_empty:       out     std_ulogic;
		rx_fifo_full:        out     std_ulogic;
		rx_data_re:          in      std_ulogic;

		tx_data:             in      std_ulogic_vector(7 downto 0);
		tx_fifo_full:        out     std_ulogic;
		tx_fifo_empty:       out     std_ulogic;
		tx_data_we:          in      std_ulogic;

		tx:                  out     std_ulogic;
		rx:                  in      std_ulogic);

	end component;

	component uart_core is
		generic (baud_rate: positive; clock_frequency: positive);
		port (
			clk:       in      std_ulogic;
			rst:       in      std_ulogic;
			din:       in      std_ulogic_vector(7 downto 0);
			din_stb:   in      std_ulogic;
			din_ack:   out     std_ulogic := '0';
			din_busy:  out     std_ulogic;

			dout:      out     std_ulogic_vector(7 downto 0);
			dout_stb:  out     std_ulogic;
			dout_ack:  in      std_ulogic;
			dout_busy: out     std_ulogic;

			tx:        out     std_ulogic;
			rx:        in      std_ulogic);
	end component;

end package;

---- UART Package --------------------------------------------------------------

---- UART Top ------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.fifo;
use work.uart_pkg.uart_core;

entity uart_top is
	generic (baud_rate: positive; clock_frequency: positive; fifo_depth: positive := 8);
	port (
		clk:                 in      std_ulogic;
		rst:                 in      std_ulogic;

		rx_data:             out     std_ulogic_vector(7 downto 0);
		rx_fifo_empty:       out     std_ulogic;
		rx_fifo_full:        out     std_ulogic;
		rx_data_re:          in      std_ulogic;

		tx_data:             in      std_ulogic_vector(7 downto 0);
		tx_fifo_full:        out     std_ulogic;
		tx_fifo_empty:       out     std_ulogic;
		tx_data_we:          in      std_ulogic;

		tx:                  out     std_ulogic;
		rx:                  in      std_ulogic);
end entity;

architecture behav of uart_top is
	signal rx_sync, rx_uart, tx_uart: std_ulogic := '0';

	signal din:       std_ulogic_vector(7 downto 0) := (others => '0');
	signal din_stb:   std_ulogic := '0';
	signal din_ack:   std_ulogic := '0';
	signal din_busy:  std_ulogic := '0';

	signal dout:      std_ulogic_vector(7 downto 0) := (others => '0');
	signal dout_stb:  std_ulogic := '0';
	signal dout_ack:  std_ulogic := '0';
	signal dout_busy: std_ulogic := '0';

	signal tx_fifo_re:             std_ulogic := '0';
	signal tx_fifo_empty_internal: std_ulogic := '1';
	signal tx_fifo_full_internal:  std_ulogic := '0';

	signal wrote_c, wrote_n: std_ulogic := '0';
begin
 
	uart_deglitch: process (clk, rst)
	begin
		if rst = '1' then
			wrote_c <= '0';
		elsif rising_edge(clk) then
			rx_sync <= rx;
			rx_uart <= rx_sync;
			tx      <= tx_uart;
			wrote_c <= wrote_n;
		end if;
	end process;

	process(dout_stb, tx_fifo_empty_internal, tx_fifo_full_internal, din_ack, wrote_c, din_busy)
	begin
			dout_ack    <= '0';
			din_stb     <= '0';
			tx_fifo_re  <= '0';
			wrote_n     <= wrote_c;

			if dout_stb = '1' then
				dout_ack <= '1';
			end if;

			if tx_fifo_empty_internal = '0' and din_busy = '0' and din_ack = '0' then
				tx_fifo_re <= '1';
				wrote_n    <= '1';
			elsif wrote_c = '1' then
				din_stb    <= '1';
				wrote_n    <= '0';
			end if;
	end process;

	rx_fifo: work.util.fifo
		generic map (
			data_width => 8,
			fifo_depth => fifo_depth)
		port map(
			clk   => clk,
			rst   => rst,
			di    => dout,
			we    => dout_stb,
			re    => rx_data_re,
			do    => rx_data,
			full  => rx_fifo_full,
			empty => rx_fifo_empty);

	tx_fifo: work.util.fifo
		generic map (
			data_width => 8,
			fifo_depth => fifo_depth)
		port map(
			clk   => clk,
			rst   => rst,
			di    => tx_data,
			we    => tx_data_we,
			re    => tx_fifo_re,
			do    => din,
			full  => tx_fifo_full_internal,
			empty => tx_fifo_empty_internal);

	tx_fifo_empty <= tx_fifo_empty_internal;
	-- @bug This is a hack, it should be just 'tx_fifo_full_internal', but
	-- it does not work correctly, so as a temporary hack the busy signal
	-- is or'd in so the data source can block until the FIFO is 'not full'
	-- and not lose any data thinking it has been transmitted.
	tx_fifo_full  <= '1' when tx_fifo_full_internal = '1'
					or din_busy = '1'
					or wrote_c = '1' 
					or din_ack = '1' 
					else '0';

	uart: work.uart_pkg.uart_core
		generic map(
			baud_rate => baud_rate,
			clock_frequency => clock_frequency)
		port map(
			clk      => clk,
			rst      => rst,
			din      => din,
			din_stb  => din_stb,
			din_ack  => din_ack,
			din_busy => din_busy,
			dout     => dout,
			dout_stb => dout_stb,
			dout_ack => dout_ack,
			dout_busy=> dout_busy,
			rx       => rx_uart,
			tx       => tx_uart);

end;

---- UART Top ------------------------------------------------------------------

---- UART Core -----------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_core is
	generic(baud_rate: positive; clock_frequency: positive);
	port(
		clk:       in      std_ulogic;
		rst:       in      std_ulogic;
		din:       in      std_ulogic_vector(7 downto 0);
		din_stb:   in      std_ulogic;
		din_ack:   out     std_ulogic := '0';
		din_busy:  out     std_ulogic;

		dout:      out     std_ulogic_vector(7 downto 0);
		dout_stb:  out     std_ulogic;
		dout_ack:  in      std_ulogic;
		dout_busy: out     std_ulogic;

		tx:        out     std_ulogic;
		rx:        in      std_ulogic);
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
	signal baud_tick:               std_ulogic := '0';
	signal oversample_baud_counter: integer range 0 to c_rx_divider_val := 0;
	signal oversample_baud_tick:    std_ulogic := '0';

	----------------------------------------------------------------------------
	-- transmitter signals
	----------------------------------------------------------------------------
	type    uart_tx_states is (idle,
				wait_for_tick,
				send_start_bit,
				transmit_data,
				send_stop_bit);

	signal  uart_tx_state: uart_tx_states := idle;

	signal  uart_tx_data_block:  std_ulogic_vector(7 downto 0) := (others => '0');
	signal  uart_tx_data:        std_ulogic := '1';
	signal  uart_tx_count:       integer range 0 to uart_tx_count_max := 0;
	signal  uart_rx_data_in_ack: std_ulogic := '0';
	----------------------------------------------------------------------------
	-- receiver signals
	----------------------------------------------------------------------------
	type    uart_rx_states is (rx_wait_start_synchronise,
	                            rx_get_start_bit,
	                            rx_get_data,
	                            rx_get_stop_bit,
	                            rx_send_block);

	signal  uart_rx_state:        uart_rx_states := rx_get_start_bit;
	signal  uart_rx_bit:          std_ulogic := '1'; -- @note should the be 0 or 1?
	signal  uart_rx_data_block:   std_ulogic_vector(7 downto 0) := (others => '0');
	signal  uart_rx_data_vec:     std_ulogic_vector(1 downto 0) := (others => '0');
	signal  uart_rx_filter:       unsigned(1 downto 0)  := (others => '1');
	signal  uart_rx_count:        integer range 0 to uart_rx_count_max  := 0;
	signal  uart_rx_data_out_stb: std_ulogic := '0';
	signal  uart_rx_bit_spacing:  unsigned (3 downto 0) := (others => '0');
	signal  uart_rx_bit_tick:     std_ulogic := '0';
begin

	din_ack  <= uart_rx_data_in_ack;
	dout     <= uart_rx_data_block;
	dout_stb <= uart_rx_data_out_stb;
	tx       <= uart_tx_data;

	din_busy  <= '0' when uart_tx_state = idle else '1';
	dout_busy <= '0' when uart_rx_state = rx_get_start_bit or uart_rx_state = rx_send_block else '1';

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

	-- get data from din and send it one bit at a time
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
				if din_stb = '1' then
					uart_tx_data_block  <= din;
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
				uart_tx_data  <= '1';
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
			uart_rx_bit    <= '1';
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
				if dout_ack = '1' then
					uart_rx_data_out_stb  <= '0';
					uart_rx_data_block    <= (others => '0');
					uart_rx_state         <= rx_get_start_bit;
				else
					uart_rx_data_out_stb  <= '1';
				end if;
			when others =>
				uart_rx_state <= rx_get_start_bit;
			end case;
		end if;
	end process;
end;

---- UART Core -----------------------------------------------------------------
