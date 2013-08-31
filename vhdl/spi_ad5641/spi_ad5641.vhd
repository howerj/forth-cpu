-- SPI, AD5641 Interface
-- This SPI channel is output only.

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity spi_ad5641 is
  port(
    clk, rst: in std_logic;

    ctrl_we:  in std_logic;                     -- Control register write enable
    ctrl_reg: in std_logic_vector(15 downto 0); -- Control register

    idata_we: in std_logic;
    idata:    in std_logic_vector(15 downto 0); -- Input data
    
    cs, oclk, odata: out std_logic               -- SPI, output only
      );
end entity;

architecture rtl of spi_ad5641 is
  signal idata_c, idata_n:       std_logic_vector(15 downto 0) := (others => '0');
  signal ctrl_reg_c, ctrl_reg_n: std_logic_vector(15 downto 0) := (others => '0');
begin
  cs <= '0';
  odata <= '0';

  oclk <= clk;

  registerProcess: process (clk,rst)
  begin
    if rst = '1' then
      idata_c     <= (others => '0');
      ctrl_reg_c  <= (others => '0');
    elsif rising_edge(clk) then
      idata_c     <= idata_n;
      ctrl_reg_c  <= ctrl_reg_n;
    end if;
  end process registerProcess;

  mainProcess: process(
    idata, idata_c, idata_we,
    ctrl_reg, ctrl_reg_c, ctrl_we
  )
  begin
    idata_n    <= idata_c;
    ctrl_reg_n <= ctrl_reg_c;

    if idata_we = '1' then
      idata_n  <= idata;
    end if;

    if ctrl_we = '1' then
      ctrl_reg_n <= ctrl_reg;
    end if;
  end process mainProcess;
end architecture;
