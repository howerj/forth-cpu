---------------------------------------------------------------------------------
--| @file gen.vhd
--| @brief A data source
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| @todo Create the inverse of this device - a logger that fills up a block
--| RAM using a counter.
---------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.bram_pkg.single_port_block_ram;
use work.util.counter;

entity gen is
	generic(addr_length: positive := 12;
		data_length: positive := 16;
		file_name:   string   := "memory.bin";
		file_type:   string   := "bin");
	port(
		clk:  in  std_logic;
		rst:  in  std_logic;

		ce:   in  std_logic;
		cr:   in  std_logic;

		dout: out std_logic_vector(data_length - 1 downto 0));
end entity;

architecture structural of gen is
	signal addr: std_logic_vector(addr_length - 1 downto 0);
begin
	count: work.util.counter
		generic map(
			length => addr_length)
		port map(
			clk  => clk,
			rst  => rst,
			ce   => ce,
			cr   => cr,
			dout => addr);

	ram: work.bram_pkg.single_port_block_ram
		generic map(
			addr_length => addr_length,
			data_length => data_length,
			file_name   => file_name,
			file_type   => file_type)
		port map(
			clk  => clk,
			addr => addr,
			dwe  => '0',
			dre  => '1',
			din  => (others => '0'),
			dout => dout);

end architecture;
