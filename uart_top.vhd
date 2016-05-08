-------------------------------------------------------------------------------
-- @file uart_top.vhd
-- @brief Top Level UART interface
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_top is
	generic(
	baud_rate:               positive := 115200;
	clock_frequency:         positive := 100000000;
	     );
	port
	(
	clk:    in  std_logic;  -- clock
	rst:    in  std_logic;

	rx:     in  std_logic;              -- uart rx 
	tx:     out std_logic :=      '0';  -- uart tx

	rx_i:   out std_logic := '0'; -- uart rx interrupt line
	tx_i:   out std_logic := '0'; -- uart tx interrupt line

	);
end entity

architecture behav of uart_top is
	signal  uart_din_c, uart_din_n:   std_logic_vector(7 downto 0) := (others => '0');
	signal  ack_din_c, ack_din_n:     std_logic:= '0';
	signal  uart_dout_c, uart_dout_n: std_logic_vector(7 downto 0):= (others => '0');
	signal  stb_dout_c, stb_dout_n:   std_logic:= '0';
	signal  uart_din, uart_dout:      std_logic_vector(7 downto 0):= (others => '0');
	signal  stb_din, stb_dout:        std_logic:= '0';
	signal  ack_din, ack_dout:        std_logic:= '0';
	signal  tx_uart, rx_uart,rx_sync: std_logic:= '0';
begin

	uart_control_registers_ns: process(clk)
	begin
	if rst = '1' then
	   uart_din_c  <=  (others => '0');
	   uart_dout_c <=  (others => '0');
	   ack_din_c   <=  '0';
	   stb_dout_c  <=  '0';
	elsif rising_edge(clk) then
	   uart_din_c  <=  uart_din_n; 
	   ack_din_c   <=  ack_din_n;
	   uart_dout_c <=  uart_dout_n;
	   stb_dout_c  <=  stb_dout_n;
	end if;
	end process;

	uart_control_registers_io: process()
	begin

	end process;

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
	BAUD_RATE => baud_rate,
	CLOCK_FREQUENCY => clock_frequency
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



end architecture;
