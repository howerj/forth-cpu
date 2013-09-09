-- General Purpose Timer
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com
-- TODO:
--  * Check this synthesizes correctly.

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity gptimer is
  port(
    clk:          in std_logic;
    rst:          in std_logic;

    timer_reset:  in std_logic;                     -- Has same function as rst

    -- Write enables
    ctr_r_we:     in std_logic;                     -- ctr_r write enable
    comp1_r_we:   in std_logic;                     -- comp1_r write enable
    comp2_r_we:   in std_logic;                     -- comp2_r write enable
    load1_r_we:   in std_logic;                     -- load1_r write enable
    load2_r_we:   in std_logic;                     -- load2_r write enable
    load_s_we:    in std_logic;                     -- load_r write enable

    -- Registers
    ctr_r:        in std_logic_vector(15 downto 0); -- Control register
    comp1_r:      in std_logic_vector(15 downto 0); -- Compare value one
    comp2_r:      in std_logic_vector(15 downto 0); -- Compare value two
    load1_r:      in std_logic_vector(15 downto 0); -- Load value one
    load2_r:      in std_logic_vector(15 downto 0); -- Load value two
    load_s:       in std_logic_vector(15 downto 0); -- Load this into the counter 

    -- Timer interrupts
    irq_comp1:    out std_logic;                    -- Compare one Interrupt
    irq_comp2:    out std_logic;                    -- Compare two Interrupt
    Q:            out std_logic;                    -- Timer signal
    NQ:           out std_logic                     -- Timer signal inverted
  );
end entity;

architecture rtl of gptimer is
  -- Registers for holding input
  signal ctr_r_c, ctr_r_n:        std_logic_vector(15 downto 0)  := (others => '0');
  signal comp1_r_c, comp1_r_n:    std_logic_vector(15 downto 0)  := (others => '0');
  signal comp2_r_c, comp2_r_n:    std_logic_vector(15 downto 0)  := (others => '0');

  signal load1_r_c, load1_r_n:    std_logic_vector(15 downto 0)  := (others => '0');
  signal load2_r_c, load2_r_n:    std_logic_vector(15 downto 0)  := (others => '0');
  -- Registers for holding output
  signal timersig_c, timersig_n:  std_logic := '0';

  -- Signals for dividing up the control register for clarity in simulation
  signal ctrl_enabled:            std_logic := '0';
  signal ctrl_updown:             std_logic := '0';
  signal ctrl_comp1_action:       std_logic_vector(1 downto 0)   := (others => '0');
  signal ctrl_comp2_action:       std_logic_vector(1 downto 0)   := (others => '0');
  signal ctrl_comp1_reset:        std_logic := '0';
  signal ctrl_comp2_reset:        std_logic := '0';
  signal ctrl_irq_en:             std_logic := '0';
  signal ctrl_comp1_load:         std_logic := '0';
  signal ctrl_comp2_load:         std_logic := '0';

  signal count:                   unsigned(15 downto 0)          := (others => '0');
  signal reset_c, reset_n:        std_logic := '0';

  signal load_actual:             std_logic_vector(15 downto 0)  := (others => '0');
  signal load_actual_we:          std_logic := '0';
begin
  ctrl_enabled      <= ctr_r_c(15);
  ctrl_updown       <= ctr_r_c(14);
  ctrl_comp1_action <= ctr_r_c(13 downto 12);
  ctrl_comp2_action <= ctr_r_c(11 downto 10);
  ctrl_comp1_reset  <= ctr_r_c(9);
  ctrl_comp2_reset  <= ctr_r_c(8);
  ctrl_irq_en       <= ctr_r_c(7);
  ctrl_comp1_load   <= ctr_r_c(6);
  ctrl_comp2_load   <= ctr_r_c(5);

  Q   <=      timersig_c when ctrl_enabled = '1' else '0';
  NQ  <= not  timersig_c when ctrl_enabled = '1' else '1';

  clockRegisters: process(clk,rst,timer_reset)
  begin
    if rst = '1' or timer_reset = '1' then
      ctr_r_c   <=  (others => '0');
      comp1_r_c <=  (others => '0');
      comp2_r_c <=  (others => '0');
      load1_r_c <=  (others => '0');
      load2_r_c <=  (others => '0');

      timersig_c <= '0';
      reset_c    <= '0';
    elsif rising_edge(clk) then
      ctr_r_c   <=  ctr_r_n;
      comp1_r_c <=  comp1_r_n;
      comp2_r_c <=  comp2_r_n;
      load1_r_c <=  load1_r_n;
      load2_r_c <=  load2_r_n;

      timersig_c <= timersig_n;
      reset_c    <= reset_n;
    end if;
  end process;

  counterProcess: process(rst,clk,timer_reset)
  begin
    if rst = '1' or timer_reset = '1' then
      count <= (others => '0');
    elsif rising_edge(clk) then
      if reset_c = '0' then
        if ctrl_enabled = '1' then
          if load_actual_we = '1' then
            count <= unsigned(load_actual);
          elsif ctrl_updown = '0' then
            count <= count + 1;
          else
            count <= count - 1;
          end if;
        else
          count <= count;
        end if;
      else
        count <= (others => '0');
      end if;
    end if;
  end process;

  compareProcess: process(
    comp1_r_c, comp2_r_c,
    load1_r_c, load2_r_c,
    count,
    timersig_c,
    reset_c,
    ctrl_comp1_reset, ctrl_comp2_reset,
    ctrl_comp1_action, ctrl_comp2_action,
    ctrl_irq_en,
    ctrl_comp1_load, ctrl_comp2_load,

    load_s_we,
    load_s
  )
  begin
    timersig_n      <= timersig_c;
    reset_n         <= '0';
    irq_comp1       <= '0';
    irq_comp2       <= '0';

    load_actual     <= (others => '0');
    load_actual_we  <= '0';

    if count = unsigned(comp1_r_c) then
      if ctrl_irq_en = '1' then
        irq_comp1 <= ctrl_irq_en;
      end if;
      case ctrl_comp1_action is
        when "00"   => timersig_n  <= timersig_c;
        when "01"   => timersig_n  <= '0';
        when "10"   => timersig_n  <= '1';
        when "11"   => timersig_n  <= not timersig_c;
        when others => timersig_n <= 'X';
      end case;

      if ctrl_comp1_reset = '1' then
        reset_n <= '1';
      end if;

      if ctrl_comp1_load = '1' then
        load_actual_we <= '1';
        load_actual <= load1_r_c;
      end if;
    end if;

    if count = unsigned(comp2_r_c) then
      if ctrl_irq_en = '1' then
        irq_comp2 <= ctrl_irq_en;
      end if;

      case ctrl_comp2_action is
        when "00"   => timersig_n  <= timersig_c;
        when "01"   => timersig_n  <= '0';
        when "10"   => timersig_n  <= '1';
        when "11"   => timersig_n  <= not timersig_c;
        when others => timersig_n  <= 'X';
      end case;

      timersig_n <= not timersig_c;
      if ctrl_comp2_reset = '1' then
        reset_n <= '1';
      end if;

      if ctrl_comp2_load = '1' then
        load_actual_we <= '1';
        load_actual <= load2_r_c;
      end if;
    end if;

    if load_s_we = '1' then
      load_actual_we <= '1';
      load_actual    <= load_s;
    end if;

  end process;

  assignRegisters: process( 
    ctr_r_we, comp1_r_we, comp2_r_we, load1_r_we, load2_r_we,
    ctr_r_c, comp1_r_c, comp2_r_c, load1_r_c, load2_r_c,
    ctr_r, comp1_r, comp2_r, load1_r, load2_r
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

      if load1_r_we = '1' then
        load1_r_n   <=  load1_r;
      else
        load1_r_n   <=  load1_r_c;
      end if;

      if load2_r_we = '1' then
        load2_r_n   <=  load2_r;
      else
        load2_r_n   <=  load2_r_c;
      end if;
  --- END Set register next state END ---
  end process;


end architecture;
