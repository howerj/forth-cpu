--------------------------------------------------------------------------------
--| @file led.vhd
--| @brief controls a number of led displays, 8 segment LEDs, there
--| is no enable, just write 0 to the displays to turn them off.
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2013 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--------------------------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package led_pkg is
	constant character_length: positive := 4;

	subtype led_character is std_ulogic_vector(character_length - 1 downto 0);
	subtype led_8_segment is std_ulogic_vector(7 downto 0);

	component led_8_segment_display is
		generic(
			asynchronous_reset:  boolean := true; -- use asynchronous reset if true, synchronous if false
			delay:               time    := 0 ns; -- simulation only, gate delay

			clock_frequency:        positive;
			use_bcd_not_hex:        boolean := true;
			refresh_rate_us:        natural := 1500;
			number_of_led_displays: positive := 4);
		port(
			clk:      in   std_ulogic;
			rst:      in   std_ulogic;

			leds_we:  in   std_ulogic;
			leds:     in   std_ulogic_vector((number_of_led_displays * character_length) - 1 downto 0);

			-- Physical outputs
			an:       out  std_ulogic_vector(number_of_led_displays - 1 downto 0);  -- anodes, controls on/off
			ka:       out  std_ulogic_vector(7 downto 0)); -- cathodes, data on display
	end component;

end package;


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
use work.led_pkg.all;

entity led_8_segment_display is
	generic(
		asynchronous_reset:     boolean := true; -- use asynchronous reset if true, synchronous if false
		delay:                  time    := 0 ns; -- simulation only, gate delay

		clock_frequency:        positive;
		use_bcd_not_hex:        boolean  := true;
		refresh_rate_us:        natural  := 1500;
		number_of_led_displays: positive := 4);
	port(
		clk:      in   std_ulogic;
		rst:      in   std_ulogic;

		leds_we:  in   std_ulogic;
		leds:     in   std_ulogic_vector((number_of_led_displays * character_length) - 1 downto 0);

		-- Physical outputs
		an:       out  std_ulogic_vector(number_of_led_displays - 1 downto 0);  -- anodes, controls on/off
		ka:       out  std_ulogic_vector(7 downto 0)); -- cathodes, data on display
end;

architecture rtl of led_8_segment_display is

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
	-- |BCD| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |Hi Lo|
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
	-- | A |   | 1 | 1 | 1 |   | 1 | 1 | 1 | 7 7 |
	-- | b |   | 1 | 1 | 1 | 1 | 1 |   |   | 7 C |
	-- | C |   |   | 1 | 1 | 1 |   |   | 1 | 3 9 |
	-- | d |   | 1 |   | 1 | 1 | 1 | 1 |   | 5 E |
	-- | E |   | 1 | 1 | 1 | 1 |   |   | 1 | 7 9 |
	-- | F |   | 1 | 1 | 1 |   |   |   | 1 | 7 1 |
	--  -----------------------------------------
	--
	-- The table is then inverted before it goes to the output.
	--

	function hex_to_8segment(a: led_character) return led_8_segment is
		variable r: std_ulogic_vector(7 downto 0);
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
			when "1010" => r := x"77"; -- A
			when "1011" => r := x"7C"; -- b
			when "1100" => r := x"39"; -- C
			when "1101" => r := x"5E"; -- d
			when "1110" => r := x"79"; -- E
			when "1111" => r := x"71"; -- F
			when others => r := x"00"; -- Unused
		end case;
		return r;
	end function;

	function bcd_to_8segment(a: led_character) return led_8_segment is
		variable r: std_ulogic_vector(7 downto 0);
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
		return r;
	end function;

	function char_to_8segment(a: led_character) return led_8_segment is
	begin
		if use_bcd_not_hex then
			return invert(bcd_to_8segment(a));
		else
			return invert(hex_to_8segment(a));
		end if;
	end function;

	signal leds_o: std_ulogic_vector(leds'range) := (others => '0');

	signal do_shift:  std_ulogic := '0';
	signal shift_reg: std_ulogic_vector(number_of_led_displays - 1 downto 0) := (0 => '1', others => '0');

	signal leds_reg_o: std_ulogic_vector(leds'range) := (others => '0');
	signal leds_reg_we_o: std_ulogic := '0';
begin
	an <= invert(shift_reg) after delay;

	segment_reg: entity work.reg
		generic map(
			asynchronous_reset => asynchronous_reset,
			delay              => delay,
			N                  => number_of_led_displays * character_length)
		port map(
			clk => clk,
			rst => rst,
			we  => leds_we,
			di  => leds,
			do  => leds_reg_o);

	segment_reg_re: entity work.reg
		generic map(
			asynchronous_reset => asynchronous_reset,
			delay              => delay,
			N                  => 1)
		port map(
			clk   => clk,
			rst   => rst,
			we    => '1',
			di(0) => leds_we,
			do(0) => leds_reg_we_o);

	led_gen: for i in number_of_led_displays - 1 downto 0 generate
		led_i: entity work.reg
			generic map(
				asynchronous_reset => asynchronous_reset,
				delay              => delay,
				N                  => character_length)
			port map(
				clk => clk,
				rst => rst,
				we  => leds_reg_we_o,
				di  => leds_reg_o((i*character_length) + character_length - 1 downto (i*character_length)),
				do  => leds_o((i*character_length) + character_length - 1 downto (i*character_length)));
	end generate;

	timer: entity work.timer_us
		generic map(
			asynchronous_reset => asynchronous_reset,
			delay              => delay,
			clock_frequency    => clock_frequency,
			timer_period_us    => refresh_rate_us)
		port map(
			clk             => clk,
			rst             => rst,
			co              => do_shift);

	process(rst, clk, do_shift, shift_reg)
	begin
		if rst = '1' and asynchronous_reset then
			shift_reg    <= (others => '0') after delay;
			shift_reg(0) <= '1' after delay;
		elsif rising_edge(clk) then
			if rst = '1' and not asynchronous_reset then
				shift_reg    <= (others => '0') after delay;
				shift_reg(0) <= '1' after delay;
			else
				if do_shift = '1' then
					shift_reg <= shift_reg(number_of_led_displays - 2 downto 0) & shift_reg(number_of_led_displays - 1) after delay;
				else
					shift_reg <= shift_reg after delay;
				end if;
			end if;
		end if;
	end process;

	process(leds_o, shift_reg)
	begin
		ka <= (others => '0');
		for i in  number_of_led_displays - 1 downto 0 loop
			if '1' = shift_reg(number_of_led_displays - i - 1) then
				ka <= char_to_8segment(leds_o(i*character_length + character_length - 1 downto (i*character_length))) after delay;
			end if;
		end loop;
	end process;
end architecture;

