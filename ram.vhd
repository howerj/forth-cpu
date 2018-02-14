-------------------------------------------------------------------------------
--| @file      ram.vhd
--| @brief     Bus Interface to Nexys3 on board memory devices
--| @author    Richard James Howe
--| @copyright Copyright 2017 Richard James Howe.
--| @license   MIT
--| @email     howe.r.j.89@gmail.com
--|
--| This component is for interfacing with the two memory devices available
--| on the Nexys3 board.
--|
--| The devices are:
--|   - PC28F128P33BF60 (Non-Volatile Flash with a CSI Interface)
--|   - MT45W1MW16BDGB  (SRAM)
--|
--| They both share the same data, address lines, output enable, and write
--| enable signals. They are selected with a Chip Select (RamCS = SRAM,
--| FlashCS = Flash device). The Flash has an addition reset line (FlashRP).
--|
--| This interface is very simple, it does not bother with timing and
--| only has minimal logic and state, it is up to the consumer of this
--| module to implement the bus timing - which in this case is a Soft CPU
--| Core.
--|
-------------------------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram_interface is
	port(
		clk:               in    std_ulogic;
		rst:               in    std_ulogic;

		mem_addr_16_1:     in    std_ulogic_vector(16 downto 1);
		mem_addr_16_1_we:  in    std_ulogic;
		mem_addr_26_17:    in    std_ulogic_vector(26 downto 17);
		mem_addr_26_17_we: in    std_ulogic;
		mem_control_i:     in    std_ulogic_vector(5 downto 0);
		mem_control_we:    in    std_ulogic;
		mem_data_i:        in    std_ulogic_vector(15 downto 0);
		mem_data_i_we:     in    std_ulogic;

		mem_data_o:        out   std_ulogic_vector(15 downto 0);

		RamCS:             out   std_ulogic := '1';

		MemOE:             out   std_ulogic := '0'; -- negative logic
		MemWR:             out   std_ulogic := '0'; -- negative logic
		MemAdv:            out   std_ulogic := '0'; -- negative logic
		MemWait:           out   std_ulogic := '0'; -- positive!

		FlashCS:           out   std_ulogic := '0';
		FlashRp:           out   std_ulogic := '1';
		MemAdr:            out   std_ulogic_vector(26 downto 1) := (others => '0');
		MemDB:             inout std_logic_vector(15 downto 0)  := (others => 'Z'));
end entity;

architecture rtl of ram_interface is
	signal mem_data_buf_i: std_ulogic_vector(mem_data_i'range)    := (others => '0');
	signal mem_control_o:  std_ulogic_vector(mem_control_i'range) := (others => '0');
	signal mem_we:         std_ulogic := '0';
	signal mem_oe:         std_ulogic := '0';
	signal mem_addr_low:   std_ulogic_vector(mem_addr_16_1'range)  := (others => '0');
	signal mem_addr_high:  std_ulogic_vector(mem_addr_26_17'range) := (others => '0');
begin
	MemAdr <= '0' & mem_addr_high & mem_addr_low(mem_addr_low'high downto mem_addr_low'low + 1);

	mem_addr_16_1_reg: entity work.reg
		generic map(N => mem_addr_16_1'length)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_addr_16_1_we,
			di  => mem_addr_16_1,
			do  => mem_addr_low);

	mem_addr_26_17_reg: entity work.reg
		generic map(N => 10)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_addr_26_17_we,
			di  => mem_addr_26_17,
			do  => mem_addr_high);

	mem_control_reg: entity work.reg
		generic map(N => 6)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_control_we,
			di  => mem_control_i,
			do  => mem_control_o);

	mem_data_i_reg: entity work.reg
		generic map(N => mem_data_i'length)
		port map(
			clk => clk,
			rst => rst,
			we  => mem_data_i_we,
			di  => mem_data_i,
			do  => mem_data_buf_i);

	FlashCS    <= '0' when mem_control_o(5 downto 4) /= "00" and mem_control_o(0) = '1' else '1';
	RamCS      <= '0' when mem_control_o(5 downto 4) /= "00" and mem_control_o(1) = '1' else '1';
	MemWait    <= mem_control_o(2);
	FlashRp    <= '0' when mem_control_o(3) = '1' else '1';
	MemAdv     <= '0' when mem_oe = '1' or mem_we = '1' else '1';
	mem_oe     <= '1' when mem_control_o(5 downto 4) = "01"  else '0';
	mem_we     <= '1' when mem_control_o(5 downto 4) = "10"  else '0';

	MemOE      <= not mem_oe;
	MemWR      <= not mem_we;

	mem_data_o <= std_ulogic_vector(MemDB) when mem_oe = '1' else (others => '0');
	MemDB      <= std_logic_vector(mem_data_buf_i) when mem_we = '1' else (others => 'Z');

end architecture;


