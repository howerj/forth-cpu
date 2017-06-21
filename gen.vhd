---------------------------------------------------------------------------------
--| @file gen.vhd
--| @brief A data source
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| @todo Load address in, dual port version whose data can be modified,
--| version using millisecond timer.
---------------------------------------------------------------------------------


library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.bram_pkg.single_port_block_ram;

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

architecture behav of gen is
	signal c_c, c_n: unsigned(addr_length - 1 downto 0) := (others => '0');
begin

	process(clk, rst)
	begin
		if rst = '1' then
			c_c <= (others => '0');
		elsif rising_edge(clk) then
			c_c <= c_n;
		end if;
	end process;

	process(c_c, cr, ce)
	begin
		c_n <= c_c;

		if cr = '1' then
			c_n <= (others => '0');
		elsif ce = '1' then
			c_n <= c_c + 1;
		end if;
	end process;

	ram: work.bram_pkg.single_port_block_ram
		generic map(
			addr_length => addr_length,
			data_length => data_length,
			file_name   => file_name,
			file_type   => file_type)
		port map(
			clk => clk,
			addr => std_logic_vector(c_c),
			dwe => '0',
			dre => '1',
			din => (others => '0'),
			dout => dout);

end architecture;
