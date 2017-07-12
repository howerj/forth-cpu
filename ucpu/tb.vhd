-- Test bench for ucpu
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb is
end tb;

architecture testing of tb is
	constant clock_frequency: positive := 100_000_000;
	constant clk_period:      time     := 1000 ms / clock_frequency;

	constant data_length: positive := 8;
	constant addr_length: positive := data_length - 2;

	signal wait_flag: std_logic := '0';
	signal clk:       std_logic := '0';
	signal rst:       std_logic := '0';

	signal a_addr: std_logic_vector(addr_length - 1 downto 0);
	signal a_dout: std_logic_vector(data_length - 1 downto 0) := (others => '0');

	signal b_dwe:  std_logic;
	signal b_dre:  std_logic;
	signal b_addr: std_logic_vector(addr_length - 1 downto 0);
	signal b_din:  std_logic_vector(data_length - 1 downto 0);
	signal b_dout: std_logic_vector(data_length - 1 downto 0) := (others => '0');

begin
	bram_0: entity work.dual_port_block_ram
	generic map(
		addr_length => addr_length,
		data_length => data_length,
		file_name   => "ucpu.bin",
		file_type   => "bin")
	port map(
		a_clk   =>  clk,
		a_dwe   =>  '0',
		a_dre   =>  '1',
		a_addr  =>  a_addr,
		a_din   =>  (others => '0'),
		a_dout  =>  a_dout,

		b_clk   =>  clk,
		b_dwe   =>  b_dwe,
		b_dre   =>  b_dre,
		b_addr  =>  b_addr,
		b_din   =>  b_din,
		b_dout  =>  b_dout);

	ucpu_0: entity work.ucpu
	generic map(width => data_length)
	port map(
		clk => clk,
		rst => rst,
		pc  => a_addr,
		op  => a_dout,

		re  => b_dre,
		we  => b_dwe,
		di  => b_dout,
		do  => b_din,
		adr => b_addr);

	clk_process: process
	begin
		while wait_flag = '0' loop
			clk <= '1';
			wait for clk_period / 2;
			clk <= '0';
			wait for clk_period / 2;
		end loop;
		wait;
	end process;

	stimulus_process: process
	begin
		rst <= '1';
		wait for clk_period * 2;
		rst <= '0';

		wait for clk_period * 1000;
	
		wait_flag <= '1';
		wait;
	end process;

end architecture;
