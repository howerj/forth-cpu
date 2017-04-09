-- @file edge.vhd 
-- @brief rising edge detector
-- @license MIT
library ieee,work,std;
use ieee.std_logic_1164.all;

entity edge is
	port(
		clk : in std_logic;
		sin : in std_logic;
       		output : out std_logic);
end edge;

architecture behav of edge is
	signal d: std_logic;
begin
	process(clk)
	begin
		if rising_edge(clk) then
			d <= sin;
		end if;
	end process;
	output <= (not d) and sin;
end behav;
		
