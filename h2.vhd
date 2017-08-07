-------------------------------------------------------------------------------
--| @file h2.vhd
--| @brief The H2 Processor: J1 processor translation and extension.
--| Moved bit 12 to bit 4 to allow for more ALU instructions.
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2017 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
--| TODO:
--|  * Use more generics: The instruction width and even features of this
--|  CPU (such as interrupts and ALU operations) could be made to be optional.
--|  * Turn this component into a package
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
		cpu_id:                   std_logic_vector(15 downto 0) := X"CAFE";
		interrupt_address_length: positive := 3;
		start_address:            natural  := 0;
		stack_size_log2:          positive := 6;
		use_interrupts:           boolean  := true);
	port(
		clk:      in  std_logic;
		rst:      in  std_logic;

		-- IO interface
		stop:     in  std_logic;

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
		daddr:    out std_logic_vector(12 downto 0));
end;

architecture rtl of h2 is

	-- Program counter.
	signal pc_c:  std_logic_vector(12 downto 0) := std_logic_vector(to_unsigned(start_address, 13));
	signal pc_n:  std_logic_vector(12 downto 0) := (others => '0');

	-- Stack Type
	constant stack_size: integer := 2 ** stack_size_log2;
	type     stack is array (stack_size - 1 downto 0) of std_logic_vector(15 downto 0);
	subtype  depth is std_logic_vector(stack_size_log2 - 1 downto 0);

	-- Variable stack (RAM Template)
	signal vstkp_c, vstkp_n:  depth := (others => '0');
	signal vstk_ram: stack          := (others => (others => '0'));
	signal dd: depth                := (others => '0'); -- variable stack delta

	-- Return stack (RAM Template)
	signal rstkp_c, rstkp_n:  depth := (others => '0');
	signal rstk_ram: stack          := (others => (others => '0'));
	signal rd: depth                := (others => '0'); -- return stack delta

	type instruction_info_type is record
		alu:     std_logic;
		lit:     std_logic;
		branch:  std_logic;
		branch0: std_logic;
		call:    std_logic;
	end record;

	signal is_instr: instruction_info_type := ('0', '0', '0', '0', '0');

	signal is_interrupt:     std_logic :=  '0';
	signal is_ram_write:     std_logic :=  '0';

	type compare_type is record
		more:  std_logic;
		equal: std_logic;
		umore: std_logic;
		zero:  std_logic;
	end record;

	signal compare: compare_type := ('0', '0', '0', '0');

	-- Interrupts
	signal int_en_c, int_en_n:     std_logic :=  '0';
	signal irq_c, irq_n:           std_logic :=  '0';
	signal irq_addr_c, irq_addr_n: std_logic_vector(interrupt_address_length - 1 downto 0) :=  (others => '0');

	-- Top of stack, and next on stack.
	signal tos_c, tos_n: std_logic_vector(15 downto 0) := (others => '0');
	signal nos:          std_logic_vector(15 downto 0) := (others => '0');

	-- Top of return stack.
	signal rtos_c:       std_logic_vector(15 downto 0) := (others => '0');

	-- aluop is what is fed into the alu.
	signal aluop: std_logic_vector(4 downto 0)         := (others => '0');
	signal pc_plus_one: std_logic_vector(12 downto 0)  := (others => '0');

	-- Stack signals
	signal dstk_we:   std_logic := '0';
	signal rstk_we:   std_logic := '0';
	signal rstk_data: std_logic_vector(15 downto 0) := (others => '0');

begin
	assert stack_size > 4    report "stack size too small: " & integer'image(stack_size) severity failure;

	-- instruction decoding is performed here
	is_instr.alu     <=  '1' when insn(15 downto 13) = "011" else '0';
	is_instr.lit     <=  '1' when insn(15) = '1' else '0';
	is_instr.branch  <=  '1' when insn(15 downto 13) = "000" else '0';
	is_instr.branch0 <=  '1' when insn(15 downto 13) = "001" else '0';
	is_instr.call    <=  '1' when insn(15 downto 13) = "010" else '0';

	is_interrupt     <=  '1' when irq_c = '1' and int_en_c = '1' and use_interrupts else '0';
	is_ram_write     <=  '1' when is_instr.alu = '1' and insn(5) = '1' else '0';

	compare.more  <= '1' when signed(tos_c)   > signed(nos)   else '0';
	compare.umore <= '1' when unsigned(tos_c) > unsigned(nos) else '0';
	compare.equal <= '1' when tos_c = nos else '0';
	compare.zero  <= '1' when unsigned(tos_c(15 downto 0)) = 0 else '0';

	-- Stack assignments
	nos    <=  vstk_ram(to_integer(unsigned(vstkp_c)));
	rtos_c <=  rstk_ram(to_integer(unsigned(rstkp_c)));

	-- I/O assignments
	pco    <=  pc_n;

	-- @note The loading and read timings really need looking at and
	-- comparing with previous versions of this file, and with the original
	-- j1.v source.
	-- @note The lowest bit is not used for reading and writing to BRAM,
	-- this is so it can be used for character addressing within
	-- applications. All RAM I/O is 16-bit aligned however. The lowest bit
	-- can be used by any external I/O. If the lowest bit is set this
	-- should really raise a bus error, but this is not implemented.
	dout   <=  nos;
	daddr  <=  tos_c(13 downto 1) when is_ram_write = '1' else tos_n(13 downto 1);
	dwe    <=  '1' when is_ram_write = '1' and tos_c(15 downto 14) = "00" else '0';
	dre    <=  '1' when tos_n(15 downto 14) = "00" else '0';

	io_dout   <=  nos;
	io_daddr  <=  tos_c(15 downto 0);
	io_wr     <= '1' when is_ram_write = '1' and tos_c(15 downto 14) /= "00" else '0';
	-- @note io_re is handled in the ALU

	pc_plus_one <= std_logic_vector(unsigned(pc_c) + 1);

	-- Sign extend the stack deltas in the instruction
	dd        <= (0 => insn(0), others => insn(1));
	rd        <= (0 => insn(2), others => insn(3));

	dstk_we     <= '1' when is_instr.lit = '1' or (is_instr.alu = '1' and insn(7) = '1') else '0';

	stack_write: process(clk)
	begin
		if rising_edge(clk) then
			if dstk_we = '1' then
				vstk_ram(to_integer(unsigned(vstkp_n))) <= tos_c(15 downto 0);
			end if;
			if rstk_we = '1' then
				rstk_ram(to_integer(unsigned(rstkp_n))) <= rstk_data;
			end if;
		end if;
	end process;

	alu_select: process(insn, is_instr, is_interrupt)
	begin
		if is_interrupt = '1' or is_instr.call = '1' or is_instr.branch = '1' then
			aluop <= (others => '0');
		elsif is_instr.branch0 = '1' then
			aluop <= (0 => '1', others => '0');
		elsif is_instr.alu = '1' then
			aluop <= insn(12 downto 8);
		else
			aluop <= (others => '0');
			-- This assert fails at time = 0 ns
			-- assert is_instr.lit = '1' report "undefined ALUOP";
		end if;
	end process;

	-- ALU
	alu: process(
		is_instr.lit,
		tos_c, nos, rtos_c,
		din, insn, aluop,
		io_din,
		vstkp_c, rstkp_c,
		compare,
		int_en_c,
		stop)
	begin
		io_re          <=  '0'; -- hardware reads can have side effects
		tos_n          <=  tos_c;
		int_en_n       <=  int_en_c;
	if stop = '1' then
		-- Do nothing
	elsif is_instr.lit = '1' then
		tos_n   <=  "0" & insn(14 downto 0);
	else
		-- @todo Experiment with these instructions to see if removing
		-- some or rearranging them speeds things up, the instructions
		-- really should be rationalized to be in an order that makes
		-- more sense.
		case aluop is -- ALU operation, 12 downto 8
		when "00000" => tos_n <= tos_c;
		when "00001" => tos_n <= nos;
		when "00010" => tos_n <= std_logic_vector(unsigned(nos) + unsigned(tos_c));
		when "00011" => tos_n <= tos_c and nos;
		when "00100" => tos_n <= tos_c or nos;
		when "00101" => tos_n <= tos_c xor nos;
		when "00110" => tos_n <= not tos_c;
		when "00111" => tos_n <= (others => compare.equal);
		when "01000" => tos_n <= (others => compare.more);
		when "01001" => tos_n <= std_logic_vector(unsigned(nos) srl to_integer(unsigned(tos_c(3 downto 0))));
		when "01010" => tos_n <= std_logic_vector(unsigned(tos_c) - 1);
		when "01011" => tos_n <= rtos_c;
		when "01100" =>
			-- input: 0x4000 - 0x7FFF is external input
			if tos_c(15 downto 14) /= "00" then
				tos_n <= io_din;
				io_re <= '1';
			else
				tos_n <= din;
			end if;
		when "01101" => tos_n <=  std_logic_vector(unsigned(nos) sll to_integer(unsigned(tos_c(3 downto 0))));
		when "01110" => tos_n(15 downto 0) <= (others => '0');
				tos_n(vstkp_c'range) <= vstkp_c;
		when "01111" => tos_n    <= (others => compare.umore);
		when "10000" => int_en_n <= tos_c(0);
		when "10001" => tos_n    <= (others => int_en_c);
		when "10010" => tos_n(15 downto 0) <= (others => '0');
				tos_n(rstkp_c'range) <= rstkp_c;
		when "10011" => tos_n    <= (others => compare.zero);
		when "10100" => tos_n    <= cpu_id;
		when others  => tos_n <= tos_c;
				report "Invalid ALU operation: " & integer'image(to_integer(unsigned(aluop))) severity error;
		end case;
	end if;
	end process;

	-- Reset and state-machine clock.
	next_state: process(clk, rst)
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

	stack_update: process(
		pc_c,
		insn,
		vstkp_c, vstk_ram, dd,
		rstkp_c, rstk_ram, rd,
		tos_c,
		is_instr,
		is_interrupt,
		pc_plus_one,
		stop)
	begin
		vstkp_n <= vstkp_c;
		rstkp_n <= rstkp_c;

		-- main control
		if stop = '1' then
			-- Do nothing
			rstk_we   <= '0';
			rstk_data <= (others => '0');
		elsif is_interrupt = '1' then
			assert to_integer(unsigned(rstkp_c)) + 1 < stack_size;

			-- Interrupts are similar to a call
			rstkp_n   <= std_logic_vector(unsigned(rstkp_c) + 1);
			rstk_we   <= '1';
			rstk_data <= "00" & pc_c & "0"; -- return to current instruction
		elsif is_instr.lit = '1' then
			assert to_integer(unsigned(vstkp_c)) + 1 < stack_size;

			vstkp_n   <= std_logic_vector(unsigned(vstkp_c) + 1);
			rstk_we   <= '0';
			rstk_data <= "00" & pc_plus_one & "0";
		elsif is_instr.alu = '1' then
			-- For return stack: implication insn(6) -> stack within bounds
			assert (not insn(6) = '1') or ((to_integer(unsigned(rstkp_c)) + to_integer(signed(rd))) < stack_size);
			assert                        ((to_integer(unsigned(vstkp_c)) + to_integer(signed(dd))) < stack_size);

			rstk_we   <= insn(6);
			rstk_data <= tos_c;
			vstkp_n   <= std_logic_vector(unsigned(vstkp_c) + unsigned(dd));
			rstkp_n   <= std_logic_vector(unsigned(rstkp_c) + unsigned(rd));
		else
			if is_instr.branch0 = '1' then
				vstkp_n <= std_logic_vector(unsigned(vstkp_c) - 1);
			end if;

			if is_instr.call = '1' then
				rstkp_n   <= std_logic_vector(unsigned(rstkp_c) + 1);
				rstk_we   <= '1';
				rstk_data <= "00" & pc_plus_one & "0";
			else
				rstk_we   <= '0';
				rstk_data <= "00" & pc_plus_one & "0";
			end if;
		end if;
	end process;

	pc_update: process(
		pc_c,insn, rtos_c, pc_plus_one,
		is_instr,
		is_interrupt, irq_c, irq_addr_c, irq_addr,irq,
		compare.zero,
		stop)
	begin
		pc_n       <= pc_c;
		irq_n      <= irq_c;
		irq_addr_n <= irq_addr_c;
		irq_n      <= irq;

		if irq = '1' then irq_addr_n <= irq_addr; end if;

		if stop = '1' then
			-- Do nothing
		elsif is_interrupt = '1' then -- Update PC on interrupt
			irq_n      <= '0';
			irq_addr_n <= (others => '0');
			pc_n       <= (others => '0');
			pc_n(interrupt_address_length - 1 downto 0) <= irq_addr_c;
		else -- Update PC on normal operations
			if is_instr.branch = '1' or (is_instr.branch0 = '1' and compare.zero = '1') or is_instr.call = '1' then
				pc_n <=  insn(12 downto 0);
			elsif is_instr.alu = '1' and insn(4) = '1' then
				pc_n <=  rtos_c(13 downto 1);
			else
				pc_n <=  pc_plus_one;
			end if;
		end if;
	end process;
end architecture;

