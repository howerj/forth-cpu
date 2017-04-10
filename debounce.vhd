--------------------------------------------------------------------------------
--
--   FileName:         debounce.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 32-bit Version 11.1 Build 173 SJ Full Version
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
--   Version 1.0 3/26/2012 Scott Larson
--     Initial Public Release
--
-- @note This file has been modified from the original one found
-- on the web from: <https://eewiki.net/pages/viewpage.action?pageId=28279002>
-- @todo make a module that accepts an array std_logic_vector to be debounced.
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity debounce is
	generic(counter_size  :  integer := 19); --counter size (19 bits gives 10.5ms with 50mhz clock)
	port(
		clk     : in  std_logic;  --input clock
		button  : in  std_logic;  --input signal to be debounced
		result  : out std_logic); --debounced signal
end debounce;

architecture logic of debounce is
	signal flipflops   : std_logic_vector(1 downto 0); --input flip flops
	signal counter_set : std_logic;                    --sync reset to zero
	signal counter_out : unsigned(counter_size downto 0) := (others => '0'); --counter output
begin

	counter_set <= flipflops(0) xor flipflops(1);   --determine when to start/reset counter
	
	process(clk)
	begin
		if(clk'event and clk = '1') then
			flipflops(0) <= button;
			flipflops(1) <= flipflops(0);
			if(counter_set = '1') then                  --reset counter because input is changing
				counter_out <= (others => '0');
			elsif(counter_out(counter_size) = '0') then --stable input time is not yet met
				counter_out <= counter_out + 1;
			else                                        --stable input time is met
			result <= flipflops(1);
			end if;
		end if;
	end process;
end logic;
