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
		pc:       out std_logic_vector(12 downto 0); -- program counter
		insn:     in  std_logic_vector(15 downto 0); -- instruction

		dwe:      out std_logic; -- data write enable
		dre:      out std_logic; -- data read enable
		din:      in  std_logic_vector(15 downto 0);
		dout:     out std_logic_vector(15 downto 0);
		daddr:    out std_logic_vector(12 downto 0));
end;

architecture rtl of h2 is

	signal pc_c:        std_logic_vector(12 downto 0) := std_logic_vector(to_unsigned(start_address, 13));
	signal pc_n:        std_logic_vector(12 downto 0) := (others => '0');
	signal pc_plus_one: std_logic_vector(12 downto 0) := (others => '0');

	constant stack_size: integer := 2 ** stack_size_log2;
	type     stack_type is array (stack_size - 1 downto 0) of std_logic_vector(15 downto 0);
	subtype  depth is unsigned(stack_size_log2 - 1 downto 0);

	signal vstkp_c, vstkp_n:  depth := (others => '0');             -- variable stack pointer
	signal vstk_ram: stack_type     := (others => (others => '0')); -- variable stack
	signal dstk_we: std_logic       := '0';                         -- variable stack write enable
	signal dd: depth                := (others => '0');             -- variable stack delta

	signal rstkp_c, rstkp_n:  depth := (others => '0');             -- return stack pointer
	signal rstk_ram: stack_type     := (others => (others => '0')); -- return stack
	signal rstk_we: std_logic       := '0';                         -- return stack write enable
	signal rd: depth                := (others => '0');             -- return stack delta

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

	signal int_en_c, int_en_n:     std_logic :=  '0'; -- interrupt enable
	signal irq_c, irq_n:           std_logic :=  '0'; -- interrupt request
	signal irq_addr_c, irq_addr_n: std_logic_vector(interrupt_address_length - 1 downto 0) :=  (others => '0');

	signal tos_c, tos_n: std_logic_vector(15 downto 0) := (others => '0'); -- top of stack 
	signal nos:          std_logic_vector(15 downto 0) := (others => '0'); -- next on stack
	signal rtos_c:       std_logic_vector(15 downto 0) := (others => '0'); -- top of return stack
	signal rstk_data:    std_logic_vector(15 downto 0) := (others => '0'); -- return stack input
	signal aluop:        std_logic_vector( 4 downto 0) := (others => '0'); -- ALU operation

begin
	assert stack_size > 4 report "stack size too small: " & integer'image(stack_size) severity failure;

	is_instr.alu     <= '1' when insn(15 downto 13) = "011" else '0';
	is_instr.lit     <= '1' when insn(15)           = '1'   else '0';
	is_instr.branch  <= '1' when insn(15 downto 13) = "000" else '0';
	is_instr.branch0 <= '1' when insn(15 downto 13) = "001" else '0';
	is_instr.call    <= '1' when insn(15 downto 13) = "010" else '0';
	is_interrupt     <= '1' when irq_c = '1' and int_en_c = '1' and use_interrupts else '0';
	is_ram_write     <= '1' when is_instr.alu = '1' and insn(5) = '1' else '0';
	compare.more     <= '1' when signed(tos_c)   > signed(nos)   else '0';
	compare.umore    <= '1' when unsigned(tos_c) > unsigned(nos) else '0';
	compare.equal    <= '1' when tos_c = nos else '0';
	compare.zero     <= '1' when unsigned(tos_c(15 downto 0)) = 0 else '0';
	nos              <= vstk_ram(to_integer(vstkp_c));
	rtos_c           <= rstk_ram(to_integer(rstkp_c));
	pc               <= pc_n;
	pc_plus_one      <= std_logic_vector(unsigned(pc_c) + 1);
	dout             <= nos;
	daddr            <= tos_c(13 downto 1) when is_ram_write = '1' else tos_n(13 downto 1);
	dwe              <= '1' when is_ram_write = '1' and tos_c(15 downto 14) = "00" else '0';
	dre              <= '1' when tos_n(15 downto 14) = "00" else '0';
	io_dout          <= nos;
	io_daddr         <= tos_c;
	io_wr            <= '1' when is_ram_write = '1' and tos_c(15 downto 14) /= "00" else '0';
	dd               <= (0 => insn(0), others => insn(1)); -- sign extend
	rd               <= (0 => insn(2), others => insn(3)); -- sign extend
	dstk_we          <= '1' when is_instr.lit = '1' or (is_instr.alu = '1' and insn(7) = '1') else '0';

	stack_write: process(clk)
	begin
		if rising_edge(clk) then
			if dstk_we = '1' then
				vstk_ram(to_integer(vstkp_n)) <= tos_c;
			end if;
			if rstk_we = '1' then
				rstk_ram(to_integer(rstkp_n)) <= rstk_data;
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
		end if;
	end process;

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
		io_re    <=  '0';      -- hardware reads can have side effects
		tos_n    <=  tos_c;
		int_en_n <=  int_en_c;
	if stop = '1' then
		null;
	elsif is_instr.lit = '1' then
		tos_n   <=  "0" & insn(14 downto 0);
	else
		case aluop is 
		when "00000" => tos_n <= tos_c;
		when "00001" => tos_n <= nos;
		when "01011" => tos_n <= rtos_c;
		when "10100" => tos_n <= cpu_id;

		when "00011" => tos_n <= tos_c and nos;
		when "00100" => tos_n <= tos_c or nos;
		when "00101" => tos_n <= tos_c xor nos;
		when "00110" => tos_n <= not tos_c;

		when "00111" => tos_n <= (others => compare.equal);
		when "01000" => tos_n <= (others => compare.more);
		when "01111" => tos_n <= (others => compare.umore);
		when "10011" => tos_n <= (others => compare.zero);

		when "01001" => tos_n <= std_logic_vector(unsigned(nos) srl to_integer(unsigned(tos_c(3 downto 0))));
		when "01101" => tos_n <= std_logic_vector(unsigned(nos) sll to_integer(unsigned(tos_c(3 downto 0))));
		when "00010" => tos_n <= std_logic_vector(unsigned(nos) + unsigned(tos_c));
		when "01010" => tos_n <= std_logic_vector(unsigned(tos_c) - 1);

		when "01100" =>
			-- input: 0x4000 - 0x7FFF is external input
			if tos_c(15 downto 14) /= "00" then
				tos_n <= io_din;
				io_re <= '1';
			else
				tos_n <= din;
			end if;
		when "01110" => tos_n <= (others => '0');
				tos_n(vstkp_c'range) <= std_logic_vector(vstkp_c);
		when "10010" => tos_n <= (others => '0');
				tos_n(rstkp_c'range) <= std_logic_vector(rstkp_c);

		when "10001" => tos_n    <= (others => int_en_c);
		when "10000" => int_en_n <= tos_c(0);

		when others  => tos_n <= tos_c;
				report "Invalid ALU operation: " & integer'image(to_integer(unsigned(aluop))) severity error;
		end case;
	end if;
	end process;

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
		vstkp_n   <= vstkp_c;
		rstkp_n   <= rstkp_c;
		rstk_we   <= '0';
		rstk_data <= "00" & pc_plus_one & "0";

		if stop = '1' then -- Do nothing
			rstk_we   <= '0';
			rstk_data <= (others => '0');
		elsif is_interrupt = '1' then -- Interrupts are similar to a call
			assert to_integer(rstkp_c) + 1 < stack_size;

			rstkp_n   <= rstkp_c + 1;
			rstk_we   <= '1';
			rstk_data <= "00" & pc_c & "0"; -- return to current instruction
		elsif is_instr.lit = '1' then
			assert to_integer(vstkp_c) + 1 < stack_size;

			vstkp_n   <= vstkp_c + 1;
			rstk_we   <= '0';
			rstk_data <= "00" & pc_plus_one & "0";
		elsif is_instr.alu = '1' then
			assert (not insn(6) = '1') or ((to_integer(rstkp_c) + to_integer(signed(rd))) < stack_size);
			assert                        ((to_integer(vstkp_c) + to_integer(signed(dd))) < stack_size);

			rstk_we   <= insn(6);
			rstk_data <= tos_c;
			vstkp_n   <= vstkp_c + unsigned(dd);
			rstkp_n   <= rstkp_c + unsigned(rd);
		elsif is_instr.branch0 = '1' then
			vstkp_n   <= vstkp_c - 1;
		elsif is_instr.call = '1' then
			rstkp_n   <= rstkp_c + 1;
			rstk_we   <= '1';
			rstk_data <= "00" & pc_plus_one & "0";
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
			null;
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

