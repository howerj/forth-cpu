-------------------------------------------------------------------------------
--! @file queue.vhd
--! @brief queue for signals
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity queue is
  generic(
    bufxsignals: positive := 4
  );
  port(
    clk:          in std_logic;
    rst:          in std_logic;

    flush:        in std_logic;   --! flush queue
    full:         out std_logic;  --! queue is full

    unbuf:    in  std_logic_vector(bufxsignals downto 0);
    buf:      out std_logic_vector(bufxsignals downto 0)
  );
end entity;

architecture rtl of queue is
begin

end architecture;
