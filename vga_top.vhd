-------------------------------------------------------------------------------
-- @file vga_top.vhd
-- @brief VGA top level
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL
-- @email      howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity vga_top is
	port(
	clk:        in  std_logic;
	clk25MHz:   in  std_logic;
	rst:        in  std_logic;

	-- VGA Text buffer interface
	vga_we_ram: in std_logic; -- Write enable RAM
	vga_a_we:   in  std_logic; -- Write enable address
	vga_d_we:   in  std_logic; -- Write enable data
	vga_dout:   out std_logic_vector(15 downto 0):= (others => '0');
	vga_din:    in  std_logic_vector(15 downto 0);
	vga_addr:   in  std_logic_vector(12 downto 0);

	-- VGA Font interface
	vga_font_dout:   out std_logic_vector(7 downto 0):= (others => '0');

	-- VGA control registers
	crx_we:   in  std_logic; -- Write enable
	ctl_we:   in  std_logic; -- ...
	cry_we:   in  std_logic; -- ...
	crx_oreg: in  std_logic_vector(6 downto 0); -- Cursor position X
	cry_oreg: in  std_logic_vector(5 downto 0); -- Cursor position Y
	ctl_oreg: in  std_logic_vector(7 downto 0); -- Control register

	-- VGA output signals
	red:      out std_logic_vector(2 downto 0) := (others => '0');
	green:    out std_logic_vector(2 downto 0) := (others => '0');
	blue:     out std_logic_vector(1 downto 0) := (others => '0');
	hsync:    out std_logic                    :=      '0';
	vsync:    out std_logic                    :=      '0');
end;

architecture behav of vga_top is
	-- Setup for text buffer memory
	constant text_addr_length: positive := 13;
	constant text_data_length: positive := 16;
	constant text_file_name:   string   := "text.bin";
	constant text_file_type:   string   := "bin";
	-- Setup for font buffer memory
	constant font_addr_length: positive := 12;
	constant font_data_length: positive := 8;
	constant font_file_name:   string   := "font.bin";
	constant font_file_type:   string   := "bin";

	-- Internal signals for mapping ouptut<-->VGA module
	signal  R_internal:      std_logic:= '0';
	signal  G_internal:      std_logic:= '0';
	signal  B_internal:      std_logic:= '0';

	-- Text RAM signals, RAM<-->VGA module
	signal  text_dout:       std_logic_vector(15 downto 0) := (others => '0');
	signal  text_din:        std_logic_vector(15 downto 0) := (others => '0');
	signal  text_addr:       std_logic_vector(11 downto 0) := (others => '0');
	signal  text_addr_full:  std_logic_vector(12 downto 0) := (others => '0');

	-- Font ROM signals, ROM<-->VGA module
	signal  font_addr:       std_logic_vector(11 downto 0) := (others => '0');
	signal  font_dout:       std_logic_vector( 7 downto 0) := (others => '0');

	-- Internal control registers
	signal  ocrx_c, ocrx_n:  std_logic_vector( 6 downto 0) := (others => '0');
	signal  ocry_c, ocry_n:  std_logic_vector( 5 downto 0) := (others => '0');
	signal  octl_c, octl_n:  std_logic_vector( 7 downto 0) := (others => '0');
	-- Internal registers for buffering write operation to RAM memory
	signal  din_c,  din_n:   std_logic_vector(15 downto 0) := (others => '0');
	signal  addr_c, addr_n:  std_logic_vector(12 downto 0) := (others => '0');
begin
	-- Output assignments, syncs elsewhere
	red   <=  R_internal & R_internal & R_internal;
	green <=  G_internal & G_internal & G_internal;
	blue  <=  B_internal & B_internal;

	-- Internal control registers
	-- Next state on clk edge rising
	vga_ns: process(clk,rst)
	begin
	if rst = '1' then
		ocrx_c      <=  (others => '0');
		ocry_c      <=  (others => '0');
		octl_c      <=  (others => '0');
		din_c       <=  (others => '0');
		addr_c      <=  (others => '0');
	elsif rising_edge(clk) then
		ocrx_c      <=  ocrx_n;
		ocry_c      <=  ocry_n;
		octl_c      <=  octl_n;
		din_c       <=  din_n;
		addr_c      <=  addr_n;
	end if;
	end process;

	-- Internal control register
	-- We write into the registers here.
	vga_creg_we: process(
		crx_we,cry_we,ctl_we,
		crx_oreg, cry_oreg, ctl_oreg,
		ocrx_c, ocry_c, octl_c,
		din_c, addr_c,
		vga_a_we, vga_din,
		vga_d_we, vga_addr)
	begin
		ocrx_n <= ocrx_c;
		ocry_n <= ocry_c;
		octl_n <= octl_c;
		din_n  <= din_c;
		addr_n <= addr_c;
		
		if crx_we = '1' then
		  ocrx_n <= crx_oreg;
		end if;

		if cry_we = '1' then
		  ocry_n <= cry_oreg;
		end if;

		if ctl_we = '1' then
		  octl_n <= ctl_oreg;
		end if;

		if vga_d_we = '1' then
			din_n <= vga_din;
		end if;

		if vga_a_we = '1' then
			addr_n <= vga_addr;
		end if;
	end process;

	-- The actual VGA module
	u_vga : entity work.vga port map (
		reset     => rst,
		clk25MHz  => clk25MHz,

		text_a    => text_addr,
		text_d    => text_dout(7 downto 0),

		font_a    => font_addr,
		font_d    => font_dout,

		ocrx      => ocrx_c,
		ocry      => ocry_c,
		octl      => octl_c(6 downto 0),

		R         => R_internal,
		G         => G_internal,
		B         => B_internal,
		hsync     => hsync,
		vsync     => vsync);

	text_addr_full <= octl_c(7) & text_addr;
	--| @brief This RAM module holds the text we want to display on to the
	--| monitor. The text buffer holds at least 80*40 characters.
	u_text: entity work.memory
	generic map(
	    addr_length   => text_addr_length,
	    data_length   => text_data_length,
	    file_name     => text_file_name,
	    file_type     => text_file_type)
	port map (
	a_clk  => clk,
	-- External interface
	a_dwe  => vga_we_ram,
	a_dre  => '1',
	a_addr => addr_c,
	a_din  => din_c,
	a_dout => vga_dout,
	-- Internal interface
	b_clk  => clk25MHz,
	b_dwe  => '0',
	b_dre  => '1',
	b_addr => text_addr_full,
	b_din  => (others => '0'),
	b_dout => text_dout);

	--| VGA Font memory
	u_font: entity work.memory
	generic map(
		addr_length   => font_addr_length,
		data_length   => font_data_length,
		file_name     => font_file_name,
		file_type     => font_file_type)
	port map (
		-- External interface
		a_clk => clk,
		a_dwe  => '0',
		a_dre  => '1',
		a_addr => (others => '0'),
		a_din  => (others => '0'),
		a_dout => vga_font_dout,
		-- Internal interface
		b_clk  => clk25MHz,
		b_dwe  => '0',
		b_dre  => '1',
		b_addr => font_addr,
		b_din  => (others => '0'),
		b_dout => font_dout);
	
end architecture;
