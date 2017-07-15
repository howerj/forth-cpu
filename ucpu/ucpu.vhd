-- @file ucpu.vhd
-- @brief An incredible simple microcontroller 
-- @license MIT
-- @author Richard James Howe
-- @copyright Richard James Howe (2017)
--
-- Based on:
-- https://stackoverflow.com/questions/20955863/vhdl-microprocessor-microcontroller
--
--  INSTRUCTION    CYCLES 87 6543210 OPERATION
--  ADD WITH CARRY   2    00 ADDRESS A = A   + MEM[ADDRESS] 
--  NOR              2    01 ADDRESS A = A NOR MEM[ADDRESS]
--  STORE            1    10 ADDRESS MEM[ADDRESS] = A
--  JCC              1    11 ADDRESS IF(CARRY) { PC = ADDRESS, CLEAR CARRY }
--
-- @todo Make a single port RAM version
--
-- It would be interesting to make a customizable CPU in which the
-- instructions could be customized based upon what. Another interesting
-- possibility is making a simple assembler purely in VHDL, which should
-- be possible, but difficult.
--

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ucpu is
	generic(width: positive range 8 to 32 := 8);
	port(
		clk, rst: in  std_logic;

		pc:       out std_logic_vector(width - 3 downto 0);
		op:       in  std_logic_vector(width - 1 downto 0);

		adr:      out std_logic_vector(width - 3 downto 0);
		di:       in  std_logic_vector(width - 1 downto 0);
		re, we:   out std_logic;
		do:       out std_logic_vector(width - 1 downto 0));
end entity;

architecture rtl of ucpu is
	signal a_c,   a_n:               unsigned(di'high + 1 downto di'low) := (others => '0'); -- accumulator
	signal pc_c,  pc_n, pc_plus_one: unsigned(pc'range)                  := (others => '0');
	signal alu:                      std_logic_vector(1 downto 0)        := (others => '0');
	signal state_c, state_n:         std_logic                           := '0'; 
begin
	pc          <= std_logic_vector(pc_n);
	do          <= std_logic_vector(a_c(do'range));
	alu         <= op(op'high downto op'high - 1);
	adr         <= op(adr'range);
	we          <= '1' when alu = "10" else '0'; -- STORE
	re          <= alu(1) nor state_c;           -- FETCH for ADD and NOR
	pc_plus_one <= pc_c + 1;
	state_n     <= alu(1) nor state_c;           -- FETCH not taken or FETCH done

	process(clk, rst)
	begin
		if rst = '1' then
			a_c     <= (others => '0');
			pc_c    <= (others => '0');
			state_c <= '0';
		elsif rising_edge(clk) then
			a_c     <= a_n;
			pc_c    <= pc_n;
			state_c <= state_n;
		end if;
	end process;

	process(op, alu, di, a_c, pc_c, pc_plus_one, state_c)
	begin
		a_n     <= a_c;
		pc_n    <= pc_c;

		if state_c = '0' then -- FETCH for ADD or NOT, EXECUTE JCC or STORE
			if alu(1) = '1' then pc_n <= pc_plus_one; end if;

			if alu = "11" then -- JCC
				if a_c(a_c'high) = '0' then 
					a_n(a_n'high) <= '0'; -- clear carry
					pc_n          <= unsigned(op(pc_n'range)); 
				end if;
			end if;
		else -- EXECUTE for ADD and NOR
			pc_n    <= pc_plus_one;
			assert alu(1) = '0' severity failure;
			if alu(0) = '0' then a_n <= '0' & a_c(di'range) + unsigned('0' & di); end if;
			if alu(0) = '1' then a_n <= a_c nor '0' & unsigned(di); end if;
		end if;
	end process;
end architecture;

