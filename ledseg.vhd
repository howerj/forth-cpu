--------------------------------------------------------------------------------
--| @file ledseg.vhd
--| @brief controls a number of led displays, 8 segment LEDs, there
--| is no enable, just write 0 to the displays to turn them off.
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2013 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| @todo Turn this module into a package and use generics 
--------------------------------------------------------------------------------

--| This module implements a 8 segment display driver, with 4 displays in total:
--|
--|    _____________________ an (selects segment)
--|    |     |     |     |
--|   __    __    __    __
--|  |  |  |  |  |  |  |  |
--|  |__|  |__|  |__|  |__|
--|  |  |  |  |  |  |  |  |
--|  |__|. |__|. |__|. |__|.
--|   |____|_____|_____|____ ka (value to display on segment)
--|
--| Each of the display shares a common anode for all of its LEDs, this can be
--| used to select an individual display

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.reg;
use work.util.timer_us;
use work.util.invert;

entity ledseg is
	generic(clock_frequency: positive);
	port(
		clk:      in   std_logic;
		rst:      in   std_logic;

		led_0:    in   std_logic_vector(3 downto 0); 
		led_1:    in   std_logic_vector(3 downto 0); 
		led_2:    in   std_logic_vector(3 downto 0); 
		led_3:    in   std_logic_vector(3 downto 0); 

		led_0_we: in   std_logic;
		led_1_we: in   std_logic;
		led_2_we: in   std_logic;
		led_3_we: in   std_logic;

		-- Physical outputs
		an:       out  std_logic_vector(3 downto 0);  -- anodes, controls on/off
		ka:       out  std_logic_vector(7 downto 0)); -- cathodes, data on display
end;

architecture rtl of ledseg is
	constant bcd_length: positive := 4;
	constant timer_period_us: natural := 1500; -- 15 milliseconds
	subtype bcd  is std_logic_vector(bcd_length - 1 downto 0);
	subtype eseg is std_logic_vector(7 downto 0);

	signal led_0_o: bcd := (others => '0');
	signal led_1_o: bcd := (others => '0');
	signal led_2_o: bcd := (others => '0');
	signal led_3_o: bcd := (others => '0');

	signal do_shift:  std_logic := '0';
	signal shift_reg: std_logic_vector(3 downto 0) := (0 => '1', others => '0');

	-- 8 Segment LED lookup table converts a BCD character into a value
	-- that can be displayed on an 8 segment display. The layout of which
	-- is as follows:
	--
	--       A
	--      --- 
	--   F |   | B
	--     |___|
	--   E | G | C
	--     |___| . DP
	--       D  
	--
	-- The following encoding is used to convert the input BCD character
	-- into a value that can be put onto the display.
	--
	--  -----------------------------------------
	-- |   | DP| G | F | E | D | C | B | A | Hex |
	-- |BCD| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 | Hex |
	--  -----------------------------------------
	-- | 0 |   |   | 1 | 1 | 1 | 1 | 1 | 1 | 3 F |
	-- | 1 |   |   |   |   |   | 1 | 1 |   | 0 6 |
	-- | 2 |   | 1 |   | 1 | 1 |   | 1 | 1 | 5 B |
	-- | 3 |   | 1 |   |   | 1 | 1 | 1 | 1 | 4 F |
	-- | 4 |   | 1 | 1 |   |   | 1 | 1 |   | 6 6 |
	-- | 5 |   | 1 | 1 |   | 1 | 1 |   | 1 | 6 D |
	-- | 6 |   | 1 | 1 | 1 | 1 | 1 |   | 1 | 7 D |
	-- | 7 |   |   |   |   |   | 1 | 1 | 1 | 0 7 |
	-- | 8 |   | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 7 F |
	-- | 9 |   | 1 | 1 |   | 1 | 1 | 1 | 1 | 6 F |
	-- |   |   |   |   |   |   |   |   |   | 0 0 |
	-- | . | 1 |   |   |   |   |   |   |   | 8 0 |
	-- | - |   | 1 |   |   |   |   |   |   | 4 0 |
	--  -----------------------------------------
	--
	-- The table is then inverted before it goes to the output.
	--
	function bcd_to_8segment(a: bcd) return eseg is
		variable r: std_logic_vector(7 downto 0);
	begin
		case a is
			when "0000" => r := x"3F"; -- 0        
			when "0001" => r := x"06"; -- 1
			when "0010" => r := x"5B"; -- 2
			when "0011" => r := x"4F"; -- 3
			when "0100" => r := x"66"; -- 4
			when "0101" => r := x"6D"; -- 5
			when "0110" => r := x"7D"; -- 6
			when "0111" => r := x"07"; -- 7
			when "1000" => r := x"7F"; -- 8
			when "1001" => r := x"6F"; -- 9
			when "1010" => r := x"00"; -- Blank
			when "1011" => r := x"80"; -- .
			when "1100" => r := x"40"; -- -
			when "1101" => r := x"00"; -- Unused
			when "1110" => r := x"00"; -- Unused
			when "1111" => r := x"00"; -- Unused
			when others => r := x"00"; -- Unused
		end case;
		return invert(r);
	end function bcd_to_8segment;
begin
	an <= invert(shift_reg);

	led0: entity work.reg generic map(N => bcd_length) port map(clk => clk, rst => rst, we => led_0_we, di => led_0, do => led_0_o); 
	led1: entity work.reg generic map(N => bcd_length) port map(clk => clk, rst => rst, we => led_1_we, di => led_1, do => led_1_o); 
	led2: entity work.reg generic map(N => bcd_length) port map(clk => clk, rst => rst, we => led_2_we, di => led_2, do => led_2_o); 
	led3: entity work.reg generic map(N => bcd_length) port map(clk => clk, rst => rst, we => led_3_we, di => led_3, do => led_3_o); 

	timer: entity work.timer_us
		generic map(clock_frequency => clock_frequency, timer_period_us => timer_period_us) 
		port map(clk => clk, rst => rst, co => do_shift);

	process(clk, do_shift, shift_reg)
	begin
		if rising_edge(clk) then
			if do_shift = '1' then
				shift_reg <= shift_reg(2 downto 0) & shift_reg(3);
			end if;
		else
			shift_reg <= shift_reg;
		end if;
	end process;

	process(led_0_o, led_1_o, led_2_o, led_3_o, shift_reg)
	begin
		ka <= (others => '0');

		if '1' = shift_reg(0) then
			ka <= bcd_to_8segment(led_0_o);
		elsif '1' = shift_reg(1) then
			ka <= bcd_to_8segment(led_1_o);
		elsif '1' = shift_reg(2) then
			ka <= bcd_to_8segment(led_2_o);
		elsif '1' = shift_reg(3) then
			ka <= bcd_to_8segment(led_3_o);
		end if;
	end process;
end architecture;

