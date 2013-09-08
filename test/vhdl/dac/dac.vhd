-- Richard James Howe.
--
-- DAC driver module, this module takes some BRAM and
-- drives via SPI a DAC
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dac is
  port(
    clk:          in   std_logic;
    clk25MHz:     in   std_logic;
    rst:          in   std_logic;

    ctr_r_we:     in std_logic;                     -- ctr_r write enable
    comp1_r_we:   in std_logic;                     -- comp1_r write enable
    comp2_r_we:   in std_logic;                     -- comp2_r write enable
    direct_r_we:  in std_logic;                     -- direct_r write enable
    ram_s_we:     in std_logic;                     -- ram_s write enable

    ctr_r:        in std_logic_vector(15 downto 0); -- Control register
    comp1_r:      in std_logic_vector(15 downto 0); -- Compare value one
    comp2_r:      in std_logic_vector(15 downto 0); -- Compare value two
    direct_r:     in std_logic_vector(15 downto 0); -- Load DAV value directly

    ram_s_addr:   in std_logic_vector(15 downto 0); -- DAC RAM Address
    ram_s_data_i: in std_logic_vector(15 downto 0); -- DAC RAM Data (Input)
    ram_s_data_o: in std_logic_vector(15 downto 0); -- DAC RAM Data (Output)

    -- DAC interrupts
    irq_comp1:    out std_logic;                    -- Compare one Interrupt
    irq_comp2:    out std_logic;                    -- Compare two Interrupt

    cs, oclk, odata, done: out std_logic            -- SPI, output only
  );
end;

architecture behav of dac is
  signal ctr_r_c, ctr_r_n:        std_logic_vector(15 downto 0)  := (others => '0');
  signal comp1_r_c, comp1_r_n:    std_logic_vector(15 downto 0)  := (others => '0');
  signal comp2_r_c, comp2_r_n:    std_logic_vector(15 downto 0)  := (others => '0');
  signal direct_r_c, direct_r_n:  std_logic_vector(15 downto 0)  := (others => '0');
begin
  clockRegisters: process(clk,rst)
  begin
    if rst = '1' then
      ctr_r_c   <=  (others => '0');
      comp1_r_c <=  (others => '0');
      comp2_r_c <=  (others => '0');
      direct_r_c <=  (others => '0');
    elsif rising_edge(clk) then
      ctr_r_c   <=  ctr_r_n;
      comp1_r_c <=  comp1_r_n;
      comp2_r_c <=  comp2_r_n;
      direct_r_c <=  direct_r_n;
    end if;
  end process;

  assignRegisters: process( 
    ctr_r_we, comp1_r_we, comp2_r_we, direct_r_we,
    ctr_r_c, comp1_r_c, comp2_r_c, direct_r_c,
    ctr_r, comp1_r, comp2_r, direct_r
  )
  begin

  --- BEGIN Set register next state BEGIN ---
      if ctr_r_we = '1' then
        ctr_r_n   <=  ctr_r;
      else
        ctr_r_n   <=  ctr_r_c;
      end if;

      if comp1_r_we = '1' then
        comp1_r_n   <=  comp1_r;
      else
        comp1_r_n   <=  comp1_r_c;
      end if;

      if comp2_r_we = '1' then
        comp2_r_n   <=  comp2_r;
      else
        comp2_r_n   <=  comp2_r_c;
      end if;

      if direct_r_we = '1' then
        direct_r_n   <=  direct_r;
      else
        direct_r_n   <=  direct_r_c;
      end if;

  --- END Set register next state END ---
  end process;




end architecture;
