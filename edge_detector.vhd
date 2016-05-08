library ieee,work,std;
use ieee.std_logic_1164.all;

entity edge_detector is
	port(
		clk : in std_logic;
		sin : in std_logic;
       		output : out std_logic
	);
end edge_detector;

architecture behav of edge_detector is
	signal d: std_logic;
begin
	process(clk)
	begin
		if clk = '1' and clk'event then
			d <= sin;
		end if;
	end process;
	output <= (not d) and sin;
end behav;
		
