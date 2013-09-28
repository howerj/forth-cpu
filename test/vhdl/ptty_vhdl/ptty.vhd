-- Richard James Howe.
--
-- ptty driver module, this is meant to simulate a 
-- terminal to uart interface for testing.
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity ptty is
  port(
    clk:          in  std_logic;
    rst:          in  std_logic
  );
end;

architecture behav of ptty is
  constant baud_rate:               positive := 115200;
  constant clock_frequency:         positive := 100000000;

  signal  rx:                       std_logic:=      'X'; 
  signal  tx:                       std_logic:=      '0';  
  signal  uart_din, uart_dout:      std_logic_vector(7 downto 0):= (others => '0');
  signal  stb_din, stb_dout:        std_logic:= '0';
  signal  ack_din, ack_dout:        std_logic:= '0';
  signal  tx_uart, rx_uart,rx_sync: std_logic:= '0';
begin
  uart_ptty: entity work.uart
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


  process
    file stdin_file:            text is in  "STD_INPUT";
    file stdout_file:           text is out "STD_OUTPUT";
    variable  inline, outline:  line;
    variable  end_of_line:      boolean;
    variable  char_in:          character;
  begin

    while not endfile(stdin_file) loop
      readline(stdin_file,inline);
      read(inline,char_in,end_of_line);
      write(outline, char_in);
      while end_of_line loop
        read(inline,char_in,end_of_line);
        write(outline, char_in);
      end loop;
      writeline(stdout_file, outline);
    end loop;
    file_close(stdout_file);
    wait;
  end process;

end architecture;
