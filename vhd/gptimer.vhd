-------------------------------------------------------------------------------
--! @file gptimer.vhd
--! @brief General Purpose Timer, for interrupt generation
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
--! TODO:
--!  * Check this synthesizes correctly.
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity gptimer is
  port(
    clk:          in std_logic;
    rst:          in std_logic;

    -- Write enables
    ctrin_we:     in std_logic;                     -- write enable

    -- Registers
    ctrin:        in std_logic_vector(15 downto 0); -- Control register

    -- Timer interrupts
    irq:          out std_logic;                    -- Compare Interrupt
    Q:            out std_logic;                    -- Timer signal
    NQ:           out std_logic                     -- Timer signal inverted
  );
end entity;

architecture rtl of gptimer is
  signal ctrl_c, ctrl_n: std_logic_vector(15 downto 0) := (others => '0');
  signal q_c, q_n:       std_logic:= '0';

  signal ctr_localrst: std_logic  := '0';
  signal ctr_enabled:  std_logic  := '0';
  signal ctr_irq_en:   std_logic  := '0';

  signal internalrst:  std_logic  := '0';

  signal compare:      std_logic_vector(12 downto 0) := (others => '0');
  signal count:        unsigned(12 downto 0)         := (others => '0');
begin
  --! Output assignents not in proc
  Q  <= q_c;
  NQ <= not q_c;

  --! Interal assigments
  ctr_enabled     <= ctrl_c(15);
  ctr_localrst    <= ctrl_c(14);
  ctr_irq_en      <= ctrl_c(13);
  compare         <= ctrl_c(12 downto 0); 

  --! Register current state assignment
  clockRegisters: process(clk, rst)
  begin
    if rst = '1' then
      q_c    <= '0';
      ctrl_c <= (others => '0');
    elsif rising_edge(clk) then
      q_c    <= q_n;
      ctrl_c <= ctrl_n;
    end if;
  end process;

  --! Counter proc
  process (clk, rst)
  begin
    if rst = '1' then
      count <= (others => '0');
    elsif rising_edge(clk) then
      if ctr_localrst = '1' or internalrst = '1' then
        count <= (others => '0');
      elsif ctr_enabled = '1' then
        count <= count + 1;
      else
        count <= count;
      end if;
    end if;
  end process;

  --! Compare proc 
  process(count, ctrin_we, ctrin, ctrl_c, compare, q_c, ctr_irq_en)
  begin
    irq         <= '0';
    q_n         <= q_c;
    ctrl_n      <= ctrl_c;
    internalrst <= '0';

    ctrl_n(14)  <= '0'; -- reset!

    if ctrin_we = '1' then
      ctrl_n <= ctrin;
    end if;

    if count = unsigned(compare) then
      if ctr_irq_en = '1' then
        irq <= '1';
      end if;
      internalrst <= '1';
      q_n <= not q_c;
    end if;
  end process;
end architecture;
