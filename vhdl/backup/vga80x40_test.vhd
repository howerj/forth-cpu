--
-- Text Screen Video Controller.
-- Pixel resolution is 640x480/60Hz, 8 colors (3-bit DAC).
--
-- 2007 Javier Valcarce García, javier.valcarce@gmail.com
-- $Id$

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity vga80x40_test is
  port (
    reset    : in  std_logic;
    clk50MHz : in  std_logic;
    R        : out std_logic;
    G        : out std_logic;
    B        : out std_logic;
    hsync    : out std_logic;
    vsync    : out std_logic
    );
end vga80x40_test;


architecture behavioral of vga80x40_test is

    component vga80x40
    port (
      reset       : in  std_logic;
      clk25MHz    : in  std_logic;
      R           : out std_logic;
      G           : out std_logic;
      B           : out std_logic;
      TEXT_A      : out std_logic_vector(11 downto 0);
      TEXT_D      : in  std_logic_vector(07 downto 0);
      FONT_A      : out std_logic_vector(11 downto 0);
      FONT_D      : in  std_logic_vector(07 downto 0);
      hsync       : out std_logic;
      vsync       : out std_logic;
      ocrx    : in  std_logic_vector(7 downto 0);
      ocry    : in  std_logic_vector(7 downto 0);
      octl    : in  std_logic_vector(7 downto 0)
      );   
  end component;
  
  component mem_text
    port (
      a_clk  : in  std_logic;
      a_dwe  : in  std_logic;
      a_addr : in  std_logic_vector(11 downto 0);
      a_din  : in  std_logic_vector(07 downto 0);
      a_dout : out std_logic_vector(07 downto 0);

      b_clk  : in  std_logic;
      b_dwe  : in  std_logic;
      b_addr : in  std_logic_vector(11 downto 0);
      b_din  : in  std_logic_vector(07 downto 0);
      b_dout : out std_logic_vector(07 downto 0));
  end component;

    component mem_font
    port (
    a_clk: IN std_logic;
    a_addr: IN std_logic_VECTOR(11 downto 0);
    a_dout: OUT std_logic_VECTOR(7 downto 0));
    end component;

  signal clk25MHz    : std_logic := '0';
  signal crx_oreg    : std_logic_vector(7 downto 0):= (others =>'0');
  signal cry_oreg    : std_logic_vector(7 downto 0):= (others =>'0');
  signal ctl_oreg    : std_logic_vector(7 downto 0):= (others =>'0');


  -- Text Buffer RAM Memory Signals, Port B (to CPU core)
  signal ram_diA : std_logic_vector(07 downto 0):= (others =>'0');
  signal ram_doA : std_logic_vector(07 downto 0):= (others =>'0');
  signal ram_adA : std_logic_vector(11 downto 0):= (others =>'0');
  signal ram_weA : std_logic:='0';

  -- Text Buffer RAM Memory Signals, Port B (to VGA core)
  signal ram_diB : std_logic_vector(07 downto 0):= (others =>'0');
  signal ram_doB : std_logic_vector(07 downto 0):= (others =>'0');
  signal ram_adB : std_logic_vector(11 downto 0):= (others =>'0');
  signal ram_weB : std_logic:='0';
  
  
  -- Font Buffer RAM Memory Signals
  signal rom_adB : std_logic_vector(11 downto 0):= (others =>'0');
  signal rom_doB : std_logic_vector(07 downto 0):= (others =>'0');
  
begin

  --Clock divider /2. Pixel clock is 25MHz
  clk25MHz <= '0' when reset = '1' else
              not clk25MHz when rising_edge(clk50MHz);
  
  U_VGA : vga80x40 port map (
    reset       => reset,
    clk25MHz    => clk25MHz,
    R           => R,
    G           => G,
    B           => B,
    hsync       => hsync,
    vsync       => vsync,
    TEXT_A      => ram_adB,
    TEXT_D      => ram_doB,
    FONT_A      => rom_adB,
    FONT_D      => rom_doB,
    ocrx    => crx_oreg,
    ocry    => cry_oreg,
    octl    => ctl_oreg);

  U_TEXT: mem_text port map (
    a_clk  => clk25MHz,
    a_dwe  => ram_weA,
    a_addr => ram_adA,
    a_din  => ram_diA,
    a_dout => ram_doA,

    b_clk  => clk25MHz,
    b_dwe  => ram_weB,
    b_addr => ram_adB,
    b_din  => ram_diB,
    b_dout => ram_doB
    );
  U_FONT: mem_font port map (
    a_clk => CLK25mhZ,
    a_addr => rom_adB,
    a_dout => rom_doB);
     
  ram_weA <= '0';
  ram_weB <= '0';
  ram_diA <= (others => '0');
  ram_adA <= (others => '0');
  ram_diB <= (others => '0');

  crx_oreg    <= std_logic_vector(TO_UNSIGNED(40, 8));
  cry_oreg    <= std_logic_vector(TO_UNSIGNED(20, 8));
  ctl_oreg    <= "11110010";
  
end behavioral;
