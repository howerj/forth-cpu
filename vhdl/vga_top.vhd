-- Richard James Howe.
--  VGA top level
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity vga_top is
  port(
    clk:      in  std_logic;
    clk25MHz: in  std_logic;
    rst:      in  std_logic;

    -- VGA Text buffer interface
    vga_we:   in  std_logic; -- Write enable
    vga_dout: out std_logic_vector(7 downto 0):= (others => '0');
    vga_din:  in  std_logic_vector(7 downto 0);
    vga_addr: in  std_logic_vector(11 downto 0);

    -- VGA control registers
    crx_we:   in  std_logic; -- Write enable
    ctl_we:   in  std_logic; -- ...
    cry_we:   in  std_logic; -- ... 
    crx_oreg: in  std_logic_vector(6 downto 0); -- Cursor position X
    cry_oreg: in  std_logic_vector(5 downto 0); -- Cursor position Y
    ctl_oreg: in  std_logic_vector(6 downto 0); -- Control register

    -- VGA output signals
    red:      out std_logic_vector(2 downto 0) := (others => '0'); 
    green:    out std_logic_vector(2 downto 0) := (others => '0'); 
    blue:     out std_logic_vector(1 downto 0) := (others => '0'); 
    hsync:    out std_logic                    :=      '0';
    vsync:    out std_logic                    :=      '0'
      );
end;

architecture behav of vga_top is

  -- Internal signals for mapping ouput<-->VGA module
  signal  R_internal:      std_logic:= '0';
  signal  G_internal:      std_logic:= '0';
  signal  B_internal:      std_logic:= '0';

  -- Text RAM signals, RAM<-->VGA module
  signal  text_dwe:        std_logic:= '0';
  signal  text_dout:       std_logic_vector(7 downto 0):=  (others => '0');
  signal  text_din:        std_logic_vector(7 downto 0):=  (others => '0');
  signal  text_addr:       std_logic_vector(11 downto 0):= (others => '0');

  -- Font ROM signals, ROM<-->VGA module
  signal  font_addr:       std_logic_vector(11 downto 0):= (others => '0');
  signal  font_data:       std_logic_vector(7 downto 0):=  (others => '0');

  -- Internal control registers
  signal  ocrx_c, ocrx_n:  std_logic_vector(6 downto 0):=  (others => '0');
  signal  ocry_c, ocry_n:  std_logic_vector(5 downto 0):=  (others => '0');
  signal  octl_c, octl_n:  std_logic_vector(6 downto 0):=  (others => '0');
begin
  -- Output assignments, syncs elsewhere
  red   <=  R_internal & R_internal & R_internal;
  green <=  G_internal & G_internal & G_internal;
  blue  <=  B_internal & B_internal;

  -- Internal control registers
  -- Next state on clk edge rising
  vga_control_registers_ns: process(clk,rst)
  begin
    if rst='1' then
      ocrx_c      <=  (others => '0');
      ocry_c      <=  (others => '0');
      octl_c      <=  (others => '0');
    elsif rising_edge(clk) then
      ocrx_c      <=  ocrx_n;
      ocry_c      <=  ocry_n;
      octl_c      <=  octl_n;
    end if;
  end process;

  -- Internal control register
  -- We write into the registers here.
  vga_control_registers_we: process(
    crx_we,cry_we,ctl_we,
    crx_oreg, cry_oreg, ctl_oreg,
    ocrx_c, ocry_c, octl_c
  )
  begin
    ocrx_n <= ocrx_c;
    ocry_n <= ocry_c;
    octl_n <= octl_c;

    if crx_we = '1' then
      ocrx_n <= crx_oreg;
    end if;

    if cry_we = '1' then
      ocry_n <= cry_oreg;
    end if;

    if ctl_we = '1' then
      octl_n <= ctl_oreg;
    end if;
  end process;

  -- The actual VGA module
  u_vga : entity work.vga80x40 port map (
    reset     => rst,
    clk25MHz  => clk25MHz,

    text_a    => text_addr,
    text_d    => text_dout,

    font_a    => font_addr,
    font_d    => font_data,

    ocrx    => crx_oreg,
    ocry    => cry_oreg,
    octl    => ctl_oreg,

    R       => R_internal,
    G       => G_internal,
    B       => B_internal,
    hsync     => hsync,
    vsync     => vsync
  );

  -- 80x40 Text buffer 
  u_text: entity work.mem_text port map (
  a_clk  => clk, 
  -- External interface
  a_dwe  => vga_we,
  a_addr => vga_addr,
  a_din  => vga_din,
  a_dout => vga_dout,
  -- Internal interface 
  b_clk  => clk25MHz,
  b_dwe  => '0',
  b_addr => text_addr,
  b_din  => (others => '0'),
  b_dout => text_dout
  );

  -- Font memory
  u_font: entity work.mem_font port map (
  a_clk => clk25MHz,
  a_addr => font_addr,
  a_dout => font_data
  );
   
end architecture;
