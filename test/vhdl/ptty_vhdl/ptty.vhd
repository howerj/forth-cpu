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
begin
--  greetings: process
--    variable s: line;
--  begin
--    write(s, string'("ptty, a tty to uart interface."));
--    writeline(output,s);
--    write(s, string'("ptty>"));
--    writeline(output,s);
--    wait;
--  end process;

  process
    file      infile, outfile:  text;
    variable  f_status:         FILE_OPEN_STATUS;
    variable  buf_in, buf_out:  LINE;
    variable  count:            integer;
  begin
    file_open(f_status, infile, "STD_INPUT", read_mode);
    file_open(f_status, outfile, "STD_OUTPUT",write_mode);

    write(buf_out, string'("?"));
    writeline(outfile, buf_out);

    readline(infile, buf_in);
    read(buf_in, count);

    write(buf_out, string'("<"));
    write(buf_out, count);
    write(buf_out, string'(">"));
    writeline(outfile, buf_out);

    file_close(outfile);
    wait;
  end process;

end architecture;
