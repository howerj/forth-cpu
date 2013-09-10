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
    clk:          in  std_logic;
    clk25MHz:     in  std_logic;
    rst:          in  std_logic;

    ctr_r_we:     in  std_logic;                     -- ctr_r write enable
    comp1_r_we:   in  std_logic;                     -- comp1_r write enable
    load1_r_we:   in  std_logic;                     -- load1_r write enable
    load_s_we:    in  std_logic;                     -- load1_r write enable
    direct_r_we:  in  std_logic;                     -- direct_r write enable
    ram_s_we:     in  std_logic;                     -- ram_s write enable

    ctr_r:        in  std_logic_vector(15 downto 0); -- Control register
    comp1_r:      in  std_logic_vector(12 downto 0); -- Compare value one
    load1_r:      in  std_logic_vector(12 downto 0); -- Compare value two
    load_s:       in  std_logic_vector(12 downto 0); -- Compare value two
    direct_r:     in  std_logic_vector(15 downto 0); -- Load DAV value directly

    ram_s_addr:   in  std_logic_vector(12 downto 0); -- DAC RAM Address
    ram_s_data_i: in  std_logic_vector(15 downto 0); -- DAC RAM Data (Input)
    ram_s_data_o: out std_logic_vector(15 downto 0); -- DAC RAM Data (Output)

    -- DAC interrupts
    irq_comp1:    out std_logic;                    -- Compare one Interrupt

    cs, oclk, odata, done: out std_logic            -- SPI, output only
  );
end;

architecture behav of dac is
  signal ctr_r_c, ctr_r_n:        std_logic_vector(15 downto 0)  := (others => '0');
  signal comp1_r_c, comp1_r_n:    std_logic_vector(12 downto 0)  := (others => '0');
  signal load1_r_c, load1_r_n:    std_logic_vector(12 downto 0)  := (others => '0');
  signal direct_r_c, direct_r_n:  std_logic_vector(15 downto 0)  := (others => '0');

  signal ctrl_enabled:            std_logic := '0';
  signal ctrl_comp1_action:       std_logic_vector(1 downto 0)   := (others => '0');
  signal ctrl_comp1_reset:        std_logic := '0';
  signal ctrl_irq_en:             std_logic := '0';
  signal ctrl_comp1_load:         std_logic := '0';

  signal count:                   unsigned(12 downto 0)          := (others => '0');

  signal load_actual:             std_logic_vector(12 downto 0)  := (others => '0');
  signal load_actual_we:          std_logic := '0';

  signal spi_done:                std_logic;

  signal spi_data:                std_logic_vector(15 downto 0)  := (others => '0');
  signal spi_data_we:             std_logic := '0';

  signal memory_b_i_DEAD:         std_logic_vector(15 downto 0)  := (others => '0');
  signal memory_b_i_we_DEAD:      std_logic := '0';
begin
  ctrl_enabled      <= ctr_r_c(15);
  ctrl_comp1_action <= ctr_r_c(13 downto 12);
  ctrl_comp1_reset  <= ctr_r_c(9);
  ctrl_irq_en       <= ctr_r_c(7);
  ctrl_comp1_load   <= ctr_r_c(6);

  done              <= spi_done;

  instance_mem_dac: entity work.mem_dac
  port map(
    a_clk     => clk,
    a_dwe     => ram_s_we,
    a_addr    => ram_s_addr(12 downto 0),
    a_din     => ram_s_data_i,
    a_dout    => ram_s_data_o,

    b_clk     => clk,
    b_dwe     => memory_b_i_we_DEAD,
    b_addr    => std_logic_vector(count),
    b_din     => memory_b_i_DEAD,
    b_dout    => spi_data
  );

  instance_dac_spi: entity work.spi_ad5641
  port map(
    clk       => clk, 
    rst       => rst,
    idata_we  => spi_data_we,
    idata     => spi_data,
    cs        => cs, 
    oclk      => oclk, 
    odata     => odata, 
    done      => spi_done
  );

  clockRegisters: process(clk,rst)
  begin
    if rst = '1' then
      ctr_r_c   <=  (others => '0');
      comp1_r_c <=  (others => '0');
      load1_r_c <=  (others => '0');
      direct_r_c <=  (others => '0');
    elsif rising_edge(clk) then
      ctr_r_c   <=  ctr_r_n;
      comp1_r_c <=  comp1_r_n;
      load1_r_c <=  load1_r_n;
      direct_r_c <=  direct_r_n;
    end if;
  end process;

  counterProcess: process(rst,clk)
  begin
    if rst = '1' then
      count <= (others => '0');
    elsif rising_edge(clk) then
      if ctrl_enabled = '1' then
        if load_actual_we = '1' then
          count <= unsigned(load_actual);
        elsif spi_done = '1' then
          count <= count + 1;
        else
          count <= count;
        end if;
      else
        count <= count;
      end if;
    end if;
  end process;

  compareProcess: process(
    comp1_r_c, 
    load1_r_c, 
    count,
    ctrl_comp1_reset,
    ctrl_comp1_action,
    ctrl_irq_en,
    ctrl_comp1_load, 

    load_s_we,
    load_s
  )
  begin
    irq_comp1       <= '0';

    load_actual     <= (others => '0');
    load_actual_we  <= '0';

    if count = unsigned(comp1_r_c) then
      if ctrl_irq_en = '1' then
        irq_comp1 <= ctrl_irq_en;
      end if;

      case ctrl_comp1_action is
        when "00"   => 
        when "01"   => 
        when "10"   => 
        when "11"   => 
        when others =>
      end case;

      if ctrl_comp1_load = '1' then
        load_actual_we <= '1';
        load_actual <= load1_r_c;
      end if;
    end if;

    if load_s_we = '1' then
      load_actual_we <= '1';
      load_actual    <= load_s;
    end if;
  end process;


  assignRegisters: process( 
    ctr_r_we, comp1_r_we, load1_r_we, direct_r_we,
    ctr_r_c, comp1_r_c, load1_r_c, direct_r_c,
    ctr_r, comp1_r, load1_r, direct_r
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

      if load1_r_we = '1' then
        load1_r_n   <=  load1_r;
      else
        load1_r_n   <=  load1_r_c;
      end if;

      if direct_r_we = '1' then
        direct_r_n   <=  direct_r;
      else
        direct_r_n   <=  direct_r_c;
      end if;

  --- END Set register next state END ---
  end process;
end architecture;
