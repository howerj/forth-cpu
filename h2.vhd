-------------------------------------------------------------------------------
--| @file h2.vhd
--| @brief The H2 Processor:
--| J1 processor translation and extension. Moved bit 12 to bit 4 to
--| allow for more ALU instructions.
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2013 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| TODO:
--|  * Use more generics: The instruction width and even features of this
--|  CPU could be made to be optional.
--|  * Assertions should be added to the core
--|  * Turn this into a literate file, describing the CPU
--|
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- only needed for calculations relating to generics

entity h2 is
	generic(
		-- Instruction width should be made to a generic option
		interrupt_address_length: positive := 3;
		start_address:            positive := 8;
		stack_size_log2:          positive := 5);
	port(
		clk:      in  std_logic;
		rst:      in  std_logic;

		-- IO interface
		stop: in  std_logic;
		io_wr:    out std_logic;
		io_re:    out std_logic; -- hardware reads can have side effects
		io_din:   in  std_logic_vector(15 downto 0);
		io_dout:  out std_logic_vector(15 downto 0);
		io_daddr: out std_logic_vector(15 downto 0);

		-- Interrupts; irq = request, irq_addr = place to jump to 
		irq:      in  std_logic;
		irq_addr: in  std_logic_vector(interrupt_address_length - 1 downto 0);

		-- RAM interface, Dual port
		pco:      out std_logic_vector(12 downto 0); -- program counter
		insn:     in  std_logic_vector(15 downto 0); -- instruction

		dwe:      out std_logic; -- data write enable
		dre:      out std_logic; -- data read enable
		din:      in  std_logic_vector(15 downto 0);
		dout:     out std_logic_vector(15 downto 0);
		daddr:    out std_logic_vector(12 downto 0)
	);
end;

architecture rtl of h2 is
	constant stack_size: integer := 2 ** stack_size_log2;

	-- Program counter.
	signal pc_c:  std_logic_vector(12 downto 0) := std_logic_vector(to_unsigned(start_address, 13));
	signal pc_n:  std_logic_vector(12 downto 0) := (others => '0');

	-- Stack Type
	type    stack is array (stack_size - 1 downto 0) of std_logic_vector(15 downto 0);
	subtype depth is std_logic_vector(stack_size_log2 - 1 downto 0);

	-- Variable stack (RAM Template)
	signal vstkp_c:  depth := (others => '0');
	signal vstkp_n:  depth := (others => '0');
	signal vstk_ram: stack := (others => (others => '0'));

	-- Return stack (RAM Template)
	signal rstkp_c:  depth := (others => '0');
	signal rstkp_n:  depth := (others => '0');
	signal rstk_ram: stack := (others => (others => '0'));

	-- Stack deltas
	signal dd: depth := (others => '0');
	signal rd: depth := (others => '0');

	-- is_instr_x signals, booleans, does the instruction have a certain property.
	signal is_instr_alu:       std_logic :=  '0';
	signal is_instr_lit:       std_logic :=  '0';
	signal is_instr_jmp:       std_logic :=  '0';
	signal is_instr_cjmp:      std_logic :=  '0';
	signal is_instr_call:      std_logic :=  '0';
	signal is_instr_interrupt: std_logic :=  '0';
	signal is_ram_write:       std_logic :=  '0';

	-- Comparisons on stack items
	signal comp_more:  std_logic :=  '0';
	signal comp_equal: std_logic :=  '0';
	signal comp_umore: std_logic :=  '0';
	signal comp_zero:  std_logic :=  '0';

	signal int_en_c, int_en_n:  std_logic :=  '0';
	signal irq_c, irq_n: std_logic :=  '0';
	signal irq_addr_c, irq_addr_n: std_logic_vector(interrupt_address_length - 1 downto 0) :=  (others => '0');

	-- Top of stack, and next on stack.
	signal tos_c, tos_n: std_logic_vector(15 downto 0) := (others => '0');
	signal nos:          std_logic_vector(15 downto 0) := (others => '0');

	-- Top of return stack.
	signal rtos_c: std_logic_vector(15 downto 0) := (others => '0');

	-- aluop is what is fed into the alu.
	signal aluop: std_logic_vector(4 downto 0)  := (others => '0');
	signal pc_plus_one: std_logic_vector(12 downto 0) := (others => '0');

	-- Stack signals
	signal dstkW: std_logic := '0';
	signal rstkW: std_logic := '0';
	signal rstkD: std_logic_vector(15 downto 0) := (others => '0');
begin
	assert(stack_size > 4 and stack_size < 128) report "stack size out of bounds";

	is_instr_alu        <=  '1' when insn(15 downto 13) = "011" else '0';
	is_instr_lit        <=  '1' when insn(15) = '1' else '0';
	is_instr_jmp        <=  '1' when insn(15 downto 13) = "000" else '0';
	is_instr_cjmp       <=  '1' when insn(15 downto 13) = "001" else '0';
	is_instr_call       <=  '1' when insn(15 downto 13) = "010" else '0';
	is_instr_interrupt  <=  '1' when irq_c = '1' and int_en_c = '1' else '0';
	is_ram_write        <=  '1' when is_instr_alu = '1' and insn(5) = '1' else '0';

	comp_more  <= '1' when signed(tos_c)   > signed(nos)   else '0';
	comp_umore <= '1' when unsigned(tos_c) > unsigned(nos) else '0';
	comp_equal <= '1' when tos_c = nos else '0';
	comp_zero  <= '1' when unsigned(tos_c(15 downto 0)) = 0 else '0';

	-- Stack assignments
	nos    <=  vstk_ram(to_integer(unsigned(vstkp_c)));
	rtos_c <=  rstk_ram(to_integer(unsigned(rstkp_c)));

	-- I/O assignments
	pco    <=  pc_n;

	-- @note The loading and read timings really need looking at and
	-- comparing with previous versions of this file, and with the original
	-- j1.v source.
	dout   <=  nos;
	daddr  <=  tos_c(12 downto 0) when is_ram_write = '1' else tos_n(12 downto 0);
	dwe    <=  '1' when is_ram_write = '1' and tos_c(14 downto 13) /= "11" else '0';
	dre    <=  '1' when tos_n(15 downto 14) = "00" else '0';

	io_dout   <=  nos;
	io_daddr  <=  tos_c(15 downto 0);
	io_wr     <= '1' when is_ram_write = '1' and tos_c(14 downto 13) = "11" else '0';
	-- io_re is handled in the ALU

	pc_plus_one <= std_logic_vector(unsigned(pc_c) + 1);

	-- Sign extend the stack deltas in the instruction
	dd        <= (0 => insn(0), others => insn(1));
	rd        <= (0 => insn(2), others => insn(3));

	dstkW     <= '1' when is_instr_lit = '1' or (is_instr_alu = '1' and insn(7) = '1') else '0';

	stackWrite: process(clk)
	begin
		if rising_edge(clk) then
			if dstkW = '1' then
				vstk_ram(to_integer(unsigned(vstkp_n))) <= tos_c(15 downto 0);
			end if;
			if rstkW = '1' then
				rstk_ram(to_integer(unsigned(rstkp_n))) <= rstkD;
			end if;
		end if;
	end process;

	alu_sel: process(insn, is_instr_interrupt)
	begin
		if is_instr_interrupt = '1' then -- same as call or ubranch
			aluop <= (others => '0');
		else
			case insn(14 downto 13) is
			when "00" => aluop <= (others => '0');            -- ubranch
			when "01" => aluop <= (others => '0');            -- call
			when "10" => aluop <= (0 => '1', others => '0');  -- 0branch
			when "11" => aluop <= insn(12 downto 8);          -- alu operation.
			when others => aluop <= (others => '0');
			end case;
		end if;
	end process;

	-- ALU
	alu: process(
		is_instr_lit,
		tos_c, nos, rtos_c,
		din, insn, aluop,
		io_din,
		vstkp_c, rstkp_c,
		comp_more,
		comp_equal,comp_umore,comp_zero,
		int_en_c,
		stop)
	begin
		io_re          <=  '0'; -- hardware *READS* can have side effects
		tos_n          <=  tos_c;
		int_en_n       <=  int_en_c;
	if stop = '1' then
		-- Do nothing
	elsif is_instr_lit = '1' then
		tos_n   <=  "0" & insn(14 downto 0);
	else
		case aluop is -- ALU operation, 12 downto 8
		when "00000" => tos_n <= tos_c;
		when "00001" => tos_n <= nos;
		when "00010" => tos_n <= std_logic_vector(unsigned(nos) + unsigned(tos_c));
		when "00011" => tos_n <= tos_c and nos;
		when "00100" => tos_n <= tos_c or nos;
		when "00101" => tos_n <= tos_c xor nos;
		when "00110" => tos_n <= not tos_c;
		when "00111" => tos_n <= (others => comp_equal);
		when "01000" => tos_n <= (others => comp_more);
		when "01001" => tos_n <= std_logic_vector(unsigned(nos) srl to_integer(unsigned(tos_c(3 downto 0))));
		when "01010" => tos_n <= std_logic_vector(unsigned(tos_c) - 1);
		when "01011" => tos_n <= rtos_c;
		when "01100" =>
			-- input: 0x6000 - 0x7FFF is external input
			if tos_c(14 downto 13) /= "00" then
				tos_n <= io_din;
				io_re <= '1';
			else
				tos_n <= din;
			end if;
		when "01101" => tos_n <=  std_logic_vector(unsigned(nos) sll to_integer(unsigned(tos_c(3 downto 0))));
		when "01110" => tos_n(4 downto 0) <= vstkp_c;
		when "01111" => tos_n <=  (others => comp_umore);
		when "10000" => int_en_n <= tos_c(0);
		when "10001" => tos_n <= (others => int_en_c);
		when "10010" => tos_n(4 downto 0) <= rstkp_c;
		when "10011" => tos_n <= (others => comp_zero);
		when others  => tos_n <= tos_c;
				report "Invalid ALU operation: " & integer'image(to_integer(unsigned(aluop))) severity error;
		end case;
	end if;
	end process;

	-- Reset and state-machine clock.
	nextState: process(clk, rst)
	begin
		if rst = '1' then
			vstkp_c    <= (others => '0');
			rstkp_c    <= (others => '0');
			pc_c       <= std_logic_vector(to_unsigned(start_address, pc_c'length));
			tos_c      <= (others => '0');
			int_en_c   <= '0';
			irq_c      <= '0';
			irq_addr_c <= (others => '0');
		elsif rising_edge(clk) then
			vstkp_c    <= vstkp_n;
			rstkp_c    <= rstkp_n;
			pc_c       <= pc_n;
			tos_c      <= tos_n;
			int_en_c   <= int_en_n;
			irq_c      <= irq_n;
			irq_addr_c <= irq_addr_n;
		end if;
	end process;

	stackUpdate: process(
		pc_c,
		insn,
		vstkp_c, vstk_ram, dd,
		rstkp_c, rstk_ram, rd,
		tos_c,
		is_instr_jmp, is_instr_cjmp, is_instr_call, is_instr_lit, is_instr_alu,
		is_instr_interrupt, 
		pc_plus_one,
		stop)
	begin
		vstkp_n <= vstkp_c;
		rstkp_n <= rstkp_c;

		-- main control
		if stop = '1' then
			-- Do nothing
			rstkW <= '0';
			rstkD <= (others => '0');
		elsif is_instr_interrupt = '1' then
			assert unsigned(rstkp_c) + 1 < stack_size;

			-- Interrupts are similar to a call
			rstkp_n <= std_logic_vector(unsigned(rstkp_c) + 1);
			rstkW   <= '1';
			rstkD   <= "000" & pc_c; -- return to *current* instruction
		elsif is_instr_lit = '1' then
			assert unsigned(vstkp_c) + 1 < stack_size;

			vstkp_n <= std_logic_vector(unsigned(vstkp_c) + 1);
			rstkW   <= '0';
			rstkD   <= "000" & pc_plus_one;
		elsif is_instr_alu = '1' then
			assert (unsigned(vstkp_c) + unsigned(dd)) < stack_size;
			assert (unsigned(rstkp_c) + unsigned(rd)) < stack_size;

			rstkW   <= insn(6);
			rstkD   <= tos_c;
			vstkp_n <= std_logic_vector(unsigned(vstkp_c) + unsigned(dd));
			rstkp_n <= std_logic_vector(unsigned(rstkp_c) + unsigned(rd));
		else
			if is_instr_cjmp = '1' then
				-- @todo add in assertion here
				vstkp_n <= std_logic_vector(unsigned(vstkp_c) - 1);
			end if;

			if is_instr_call = '1' then
				assert unsigned(rstkp_c) < stack_size;
				rstkp_n <= std_logic_vector(unsigned(rstkp_c) + 1);
				rstkW   <= '1';
				rstkD   <= "000" & pc_plus_one;
			else
				rstkW   <= '0';
				rstkD   <= "000" & pc_plus_one;
			end if;
		end if;
	end process;

	pcUpdate: process(
		pc_c,insn,rtos_c,pc_plus_one,
		is_instr_jmp,is_instr_cjmp,is_instr_call,is_instr_alu,
		is_instr_interrupt,irq_c,irq_addr_c,irq_addr,irq,
		comp_zero,
		stop)
	begin
		pc_n       <= pc_c;
		irq_n      <= irq_c;
		irq_addr_n <= irq_addr_c;
		irq_n      <= irq;

		if irq = '1' then irq_addr_n <= irq_addr; end if;

		if stop = '1' then
			-- Do nothing
		elsif is_instr_interrupt = '1' then -- Update PC on interrupt
			irq_n      <= '0';
			irq_addr_n <= (others => '0');
			pc_n       <= (others => '0');
			pc_n(interrupt_address_length - 1 downto 0) <= irq_addr_c;
		else -- Update PC on normal operations
			if is_instr_jmp = '1' or (is_instr_cjmp = '1' and comp_zero = '1') or is_instr_call = '1' then
				pc_n    <=  insn(12 downto 0);
			elsif is_instr_alu = '1' and insn(4) = '1' then
				pc_n    <=  rtos_c(12 downto 0);
			else
				pc_n    <=  pc_plus_one;
			end if;
		end if;
	end process;
end architecture;

