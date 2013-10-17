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
    bufxsignals:  positive := 4;
    queuelen:     positive := 4
  );
  port(
    clk:          in std_logic;
    rst:          in std_logic;

    flush:        in std_logic;   --! flush queue
    full:         out std_logic;  --! queue is full

    unbuf:        in  std_logic_vector(bufxsignals downto 0);
    buf:          out std_logic_vector(bufxsignals downto 0)
  );
end entity;

architecture rtl of queue is
  type queuebuf_t is array (0 to queuelen -1) of std_logic_vector(bufxsignals downto 0);
  signal queuebuf_c: queuebuf_t:= (others => (others => '0'));
  signal queuebuf_n: queuebuf_t:= (others => (others => '0'));
begin
  reg_current:process(clk, rst)
  begin
    if rst = '1' then
      queuebuf_c <= (others => (others => '0'));
    elsif rising_edge(clk) then
      queuebuf_c <= queuebuf_n;
    end if;
  end process;

  reg_next: process(queuebuf_c)
  begin
    queuebuf_n <= queuebuf_c; --! default

  end process;
end architecture;
