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
  -- Registers for holding input
  signal ctr_r_c, ctr_r_n:        std_logic_vector(15 downto 0)  := (others => '0');
  signal comp1_r_c, comp1_r_n:    std_logic_vector(15 downto 0)  := (others => '0');
  signal comp2_r_c, comp2_r_n:    std_logic_vector(15 downto 0)  := (others => '0');
  -- Registers for holding output
  signal timersig_c, timersig_n:  std_logic := '0';

  -- Signals for dividing up the control register for clarity in simulation
  signal ctrl_enabled:            std_logic := '0';
  signal ctrl_updown:             std_logic := '0';
  signal ctrl_comp1_action:       std_logic_vector(1 downto 0)   := (others => '0');
  signal ctrl_comp2_action:       std_logic_vector(1 downto 0)   := (others => '0');
begin
  clockRegisters: process(clk,rst)
  begin
    if rst = '1' then
      ctr_r_c   <=  (others => '0');
      comp1_r_c <=  (others => '0');
      comp2_r_c <=  (others => '0');

      timersig_c <= '0';
    elsif rising_edge(clk) then
      ctr_r_c   <=  ctr_r_n;
      comp1_r_c <=  comp1_r_n;
      comp2_r_c <=  comp2_r_n;

      timersig_c <= timersig_n;
    end if;
  end process;

  assignRegisters: process( 
    ctr_r_we, comp1_r_we, comp2_r_we,
    ctr_r_c, comp1_r_c, comp2_r_c,

    timersig_c
  )
  begin
  --- BEGIN Set register *default* next state BEGIN ---
      timersig_n <= timersig_c;
  --- END Set register *default* next state END ---

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
  --- END Set register next state END ---

  end process;

end architecture;
