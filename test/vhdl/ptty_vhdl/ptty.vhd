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
  generic(
    baud_rate:               positive := 115200;
    clock_frequency:         positive := 100000000
  );
  port(
    clk:          in  std_logic;
    rst:          in  std_logic;
    rx:           in  std_logic                    :=      'X';  -- uart rx 
    tx:           out std_logic                    :=      '0';  -- uart tx
    done:         out std_logic                    :=      '0'  -- done flag
  );
end;

architecture behav of ptty is
  constant clk_period:              time:=  1000 ms / clock_frequency;

  signal  uart_din, uart_dout:      std_logic_vector(7 downto 0):= (others => '0');
  signal  stb_din, stb_dout:        std_logic:= '0';
  signal  ack_din, ack_dout:        std_logic:= '0';

  signal  done_internal_signal:     std_logic:= '0';
begin

  done <= done_internal_signal;

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
   rx => rx,
   tx => tx
  );

  readProcess: process(done_internal_signal, stb_dout, uart_dout)
--  process
   file stdout_file:           text is out "STD_OUTPUT";
   variable  outline:  line;
   variable  not_end_of_line:  boolean;
   variable  char_out:         character;
   variable  int:              integer := 0;
  begin
      ack_dout <= '0';
      if stb_dout = '1' then
        int := to_integer(unsigned(uart_dout));
        char_out := character'val(int);
        write(outline, char_out);
        ack_dout <= '1';
        if char_out = cr then
          writeline(stdout_file, outline);
        end if;
      end if;

      if done_internal_signal = '1' then
        file_close(stdout_file);
      end if;

      assert done_internal_signal = '0' report "Finished" severity note;
  end process;

  writeProcess:process
    file stdin_file:            text is in  "STD_INPUT";
    variable  inline:           line;
    variable  not_end_of_line:  boolean;
    variable  char_in:          character;
    variable  int:              integer := 0;
  begin
    done_internal_signal <= '0';
    while not endfile(stdin_file) loop
      readline(stdin_file,inline);
      not_end_of_line := true;
      while not_end_of_line loop
        read(inline,char_in,not_end_of_line);
        int := character'pos(char_in);
        uart_din <= std_logic_vector(to_unsigned(int, uart_din'length));
        stb_din <= '1';
        wait for clk_period;
        wait until ack_din = '1'; -- Wait for however long it takes!
        stb_din <= '0';
        wait for clk_period;
      end loop;
    end loop;
    done_internal_signal <= '1';
    wait;
  end process;

end architecture;
