-- SPI, AD5641 Interface
-- This SPI channel is output only.

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity spi_ad5641 is
  port(
    clk, rst: in std_logic;

    idata_we: in std_logic;
    idata:    in std_logic_vector(15 downto 0); -- Input data
    
    cs, oclk, odata, done: out std_logic               -- SPI, output only
      );
end entity;

architecture rtl of spi_ad5641 is
  signal idata_c, idata_n:        std_logic_vector(15 downto 0) := (others => '0');
  signal enable_c, enable_n:      std_logic := '0';
  signal done_i, sout_s:          std_logic := '0';
  signal done_c, done_n:          std_logic := '0';
begin
  cs    <= enable_c;
  oclk  <= clk    when enable_c = '1' else '0';
  odata <= sout_s when enable_c = '1' else '0';

  done  <= done_c;

  shift_reg_16_instance: entity work.shift_reg_16
    port map(
      clk => clk,
      rst => rst,
      enable => enable_c,
      sin => idata_c,
      sout => sout_s,
      done => done_i
    );

  registerProcess: process (clk,rst)
  begin
    if rst = '1' then
      idata_c     <= (others => '0');
      enable_c    <= '0';
      done_c      <= '0';
    elsif rising_edge(clk) then
      idata_c     <= idata_n;
      enable_c    <= enable_n;
      done_c      <= done_n;
    end if;
  end process registerProcess;

  mainProcess: process(
    idata, idata_c, idata_we,
    enable_c,
    done_i, done_c
  )
  begin
    idata_n    <= idata_c;
    enable_n   <= enable_c;
    done_n     <= done_c;

    if idata_we = '1' then
      idata_n  <= idata;
      enable_n <= '1';
      done_n   <= '0';
    end if;

    if done_i = '1' then
      enable_n <= '0';
      done_n   <= '1';
    end if;
  end process mainProcess;
end architecture;
