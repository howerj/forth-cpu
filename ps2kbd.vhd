--------------------------------------------------------------------------------
--
--   FileName:         ps2kbd.vhd
--   Dependencies:     debounce.vhd
--   Design Software:  Quartus II 32-bit Version 12.1 Build 177 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 11/25/2013 Scott Larson
--     Initial Public Release
--
--   @note This file has been modified from the original one found at
--   <https://eewiki.net/pages/viewpage.action?pageId=28279002>
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity ps2kbd is
	generic(
		clock_frequency: integer := 50000000;  -- system clock frequency in hz
		debounce_counter_size: integer := 8);  -- set such that (2^size)/clock_frequency = 5us (size = 8 for 50mhz)
	port(
		clk:          in  std_logic; -- system clock
		ps2_clk:      in  std_logic; -- clock signal from PS/2 keyboard
		ps2_data:     in  std_logic; -- data signal from PS/2 keyboard
		ps2_code_new: out std_logic; -- flag that new PS/2 code is available on ps2_code bus
		ps2_code:     out std_logic_vector(7 downto 0)); -- code received from PS/2
end ps2kbd;

architecture rtl of ps2kbd is

	signal sync_ffs: std_logic_vector(1 downto 0);        -- synchronizer flip-flops for PS/2 signals
	signal ps2_clk_int: std_logic;                        -- debounced clock signal from PS/2 keyboard

	attribute buffer_type: string;
	attribute buffer_type of ps2_clk_int: signal is "BUFG";

	signal ps2_data_int: std_logic;                       -- debounced data signal from PS/2 keyboard
	signal ps2_word: std_logic_vector(10 downto 0);       -- stores the ps2 data word
	signal parity_error: std_logic;                       -- validate parity, start, and stop bits
	signal count_idle: integer range 0 to clock_frequency/18000; --counter to determine PS/2 is idle
	
	--declare debounce component for debouncing ps2 input signals
	component debounce is
	generic(counter_size : integer); -- debounce period (in seconds) = 2^counter_size/(clk freq in hz)
	port(
		clk:    in  std_logic;  -- input clock
		button: in  std_logic;  -- input signal to be debounced
		result: out std_logic); -- debounced signal
	end component;
begin

	--synchronizer flip-flops
	process(clk)
	begin
		if rising_edge(clk) then         -- rising edge of system clock
			sync_ffs(0) <= ps2_clk;  -- synchronize PS/2 clock signal
			sync_ffs(1) <= ps2_data; -- synchronize PS/2 data signal
		end if;
	end process;

	-- debounce ps2 input signals
	debounce_ps2_clk: debounce
	generic map(counter_size => debounce_counter_size)
	port map(clk => clk, button => sync_ffs(0), result => ps2_clk_int);

	debounce_ps2_data: debounce
	generic map(counter_size => debounce_counter_size)
	port map(clk => clk, button => sync_ffs(1), result => ps2_data_int);

	-- input ps2 data
	process(ps2_clk_int)
	begin
		if(ps2_clk_int'event and ps2_clk_int = '0') then    --falling edge of ps2 clock
			ps2_word <= ps2_data_int & ps2_word(10 downto 1);   --shift in ps2 data bit
		end if;
	end process;
	
	-- verify that parity, start, and stop bits are all correct
	parity_error <= not (not ps2_word(0) and ps2_word(10) and (ps2_word(9) xor ps2_word(8) xor
	    ps2_word(7) xor ps2_word(6) xor ps2_word(5) xor ps2_word(4) xor ps2_word(3) xor
	    ps2_word(2) xor ps2_word(1)));

	-- determine if ps2 port is idle (i.e. last transaction is finished) and output result
	process(clk)
	begin
		if rising_edge(clk) then                           -- rising edge of system clock
			if(ps2_clk_int = '0') then                 -- low ps2 clock, PS/2 is active
				count_idle <= 0;                   -- reset idle counter
			elsif(count_idle /= clock_frequency/18_000) then  -- ps2 clock has been high less than a half clock period (<55us)
				count_idle <= count_idle + 1;      -- continue counting
			end if;

			if(count_idle = clock_frequency/18_000 and parity_error = '0') then  -- idle threshold reached and no errors detected
				ps2_code_new <= '1';                           -- set flag that new PS/2 code is available
				ps2_code <= ps2_word(8 downto 1);              -- output new PS/2 code
			else                                                   -- PS/2 port active or error detected
				ps2_code_new <= '0';                           -- set flag that PS/2 transaction is in progress
			end if;
		end if;
	end process;
	
end;
