--------------------------------------------------------------------------------
--! @file ledseg.vhd
--! @brief controls a number of led displays, 8 segment LEDs, there
--! is no enable, just write 0 to the displays to turn them off.
--!
--! @author     Richard James Howe.
--! @copyright  Copyright 2013 Richard James Howe.
--! @license    MIT
--! @email      howe.r.j.89@gmail.com
--!
--! @note This module could be made more generic, but the interface would
--! have to change. There would be little value in this. What should be
--! changed so it accepts bytes no 16-bit values.
--------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util;

entity ledseg is
	port(
		clk:      in   std_logic;
		rst:      in   std_logic;

		led_0:    in   std_logic_vector(7 downto 0); 
		led_1:    in   std_logic_vector(7 downto 0); 
		led_2:    in   std_logic_vector(7 downto 0); 
		led_3:    in   std_logic_vector(7 downto 0); 

		led_0_we: in   std_logic;
		led_1_we: in   std_logic;
		led_2_we: in   std_logic;
		led_3_we: in   std_logic;

		-- Physical outputs
		an:       out  std_logic_vector(3 downto 0);  -- anodes, controls on/off
		ka:       out  std_logic_vector(7 downto 0)); -- cathodes, data on display
end;

architecture behav of ledseg is
	-- use smaller counter number for testing.
	-- @todo This speed should depend on a generic clock parameter
	constant highest_counter_bit: integer := 18;
	constant segment_length: positive := 8;
	subtype led8segment is std_logic_vector(segment_length - 1 downto 0);

	signal led_0_o: led8segment := (others => '0');
	signal led_1_o: led8segment := (others => '0');
	signal led_2_o: led8segment := (others => '0');
	signal led_3_o: led8segment := (others => '0');

	signal counter:    unsigned(highest_counter_bit downto 0) := (others => '0');
	signal counter_hb: std_logic := '0';
	signal shift_reg:  std_logic_vector(3 downto 0) := (0 => '1', others => '0');
begin
	counter_hb <= counter(highest_counter_bit);
	an <= shift_reg;

	led0: entity work.reg generic map(N => segment_length) port map(clk => clk, rst => rst, we => led_0_we, di => led_0, do => led_0_o); 
	led1: entity work.reg generic map(N => segment_length) port map(clk => clk, rst => rst, we => led_1_we, di => led_1, do => led_1_o); 
	led2: entity work.reg generic map(N => segment_length) port map(clk => clk, rst => rst, we => led_2_we, di => led_2, do => led_2_o); 
	led3: entity work.reg generic map(N => segment_length) port map(clk => clk, rst => rst, we => led_3_we, di => led_3, do => led_3_o); 

	-- @todo generic counter and shift register modules should be made
	process(clk, rst)
	begin
		if rst = '1' then
			counter <= (others => '0');
		elsif rising_edge(clk) then
			counter   <= counter + 1;
		end if;
	end process;

	process(counter_hb, shift_reg)
	begin
		if rising_edge(counter_hb) then
			shift_reg <= shift_reg(2 downto 0) & shift_reg(3);
		else
			shift_reg <= shift_reg;
		end if;
	end process;

	process(led_0_o, led_1_o, led_2_o, led_3_o, shift_reg)
	begin
		ka <= (others => '0');

		if '1' = shift_reg(0) then
			ka <= led_0_o;
		elsif '1' = shift_reg(1) then
			ka <= led_1_o;
		elsif '1' = shift_reg(2) then
			ka <= led_2_o;
		elsif '1' = shift_reg(3) then
			ka <= led_3_o;
		end if;
	end process;
end architecture;
