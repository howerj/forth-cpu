-- General Purpose Timer
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity gptimer is
  port(
    clk:        in std_logic;
    rst:        in std_logic;

    ctr_r_we:   in std_logic;                     -- ctr_r write enable
    comp1_r_we: in std_logic;                     -- comp1_r write enable
    comp2_r_we: in std_logic;                     -- comp2_r write enable

    ctr_r:      in std_logic_vector(15 downto 0); -- Control register
    comp1_r:    in std_logic_vector(15 downto 0); -- Compare value one
    comp2_r:    in std_logic_vector(15 downto 0); -- Compare value two

    interrupt:  out std_logic;                    -- Generate interrupt
    timersig:   out std_logic                     -- Any timer signal
  );
end entity;

architecture rtl of gptimer is
  signal ctr_r_c, ctr_r_n:      std_logic_vector(15 downto 0)  := (others => '0');
  signal comp1_r_c, comp1_r_n:  std_logic_vector(15 downto 0)  := (others => '0');
  signal comp2_r_c, comp2_r_n:  std_logic_vector(15 downto 0)  := (others => '0');
begin
  clockRegisters: process(clk,rst)
  begin
    if rst = '1' then
      ctr_r_c   <=  (others => '0');
      comp1_r_c <=  (others => '0');
      comp2_r_c <=  (others => '0');
    elsif rising_edge(clk) then
      ctr_r_c   <=  ctr_r_n;
      comp1_r_c <=  comp1_r_n;
      comp2_r_c <=  comp2_r_n;
    end if;
  end process;

  assignRegisters: process( 
    ctr_r_we, comp1_r_we, comp2_r_we,
    ctr_r_c, comp1_r_c, comp2_r_c
  )
  begin
      ctr_r_n   <=  ctr_r_c;
      comp1_r_n <=  comp1_r_c;
      comp2_r_n <=  comp2_r_c;
  end process;

end architecture;
