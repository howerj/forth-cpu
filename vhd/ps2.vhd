--------------------------------------------------------------------------------- 
--! @file ps2.vhd
--! @brief ps2.vhd, implements a not-fully-compliant PS/2
--!  keyboard driver. 
--! @author     Richard James Howe.
--! @copyright  Copyright 2013 Richard James Howe.
--! @license    LGPL    
--! @email      howe.r.j.89@gmail.com
--------------------------------------------------------------------------------- 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ps2 is
  port
  (
    clk:      in  std_logic := '0';  -- clock
    rst:      in  std_logic := '0';  -- reset pin

    ps2_clk:  in  std_logic := '0';  -- PS/2 Keyboard Clock
    ps2_data: in  std_logic := '0';  -- PS/2 Keyboard Data
    ack_in:   in  std_logic := '0';  -- external component signals data read

    stb_out:  out std_logic := '0';  -- recieved data, waiting to be read
    scanCode: out std_logic_vector(7 downto 0); -- actual data we want
    scanError:out std_logic := '0'   -- an error has occured, woops.
  );
end entity;

architecture behav of ps2 is
  signal  filter: std_logic_vector(7 downto 0);
  signal  parity_bit_n, parity_bit_c: std_logic := 'X';
  signal  bit_count_n, bit_count_c:  unsigned(3 downto 0);
  signal  scode_n, scode_c: std_logic_vector(7 downto 0) := (others => '0');

  signal  ps2_clk_filtered: std_logic := '0';
  signal  ps2_clk_falling: std_logic := '0';

  type    ps2_state is (waiting, reading, fail);
  signal  state_n, state_c: ps2_state;
begin
  scanCode <= scode_c;

  -- Filter signal
  process(clk,rst)
  begin
    if rst = '1' then
      state_c   <= waiting;
      scode_c   <= (others => '0');
      filter    <= (others => '0');
      bit_count_c <= (others => '0');
      ps2_clk_filtered <= '0';
      ps2_clk_falling  <= '0';
    elsif rising_edge(clk) then
      scode_c <= scode_n;
      state_c <= state_n;
      bit_count_c <= bit_count_n;
      parity_bit_c <= parity_bit_n;

      filter  <= ps2_clk & filter(filter'high downto 1);
      ps2_clk_falling <= '0';
      if filter = X"FF" then
        ps2_clk_filtered <= '1';
      elsif filter = X"00" then
        ps2_clk_filtered <= '0';
        if ps2_clk_filtered = '1' then
          ps2_clk_falling <= '1';
        end if;
      end if;
    end if;
  end process;

  process(
      parity_bit_c, bit_count_c, ps2_clk_filtered, ps2_clk_falling, state_c,
      scode_c, ps2_data, ack_in
    )
  begin
    parity_bit_n <= parity_bit_c;
    bit_count_n  <= bit_count_c;
    state_n      <= state_c;
    scode_n      <= scode_c;

    scanError    <= '0';
    stb_out      <= '1';

    case state_c is
      when waiting =>
        parity_bit_n <= '0';
        bit_count_n  <= (others => '0');
        if ps2_clk_falling = '1' and ps2_data = '0' then
          state_n <= reading;
        end if;
      when reading =>
        if bit_count_c >= 9 then
          state_n <= fail;
        elsif ps2_clk_falling = '1' then
          bit_count_n  <= bit_count_c + 1;
          scode_n      <= ps2_data & scode_c(scode_c'high downto 1);
          parity_bit_n <= parity_bit_c xor ps2_data;
        end if;
      when fail    =>
        scanError <= '1';
        if ack_in = '1' then
          state_n <= waiting;
        end if;
      when others => 
          state_n <= fail;
    end case;
  end process;

  -- Read in scan code
  -- Convert code?
end architecture;
