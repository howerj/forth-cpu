-------------------------------------------------------------------------------
--| @file h2.vhd
--| @brief The H2 Processor: J1 processor translation and extension.
--| Moved bit 12 to bit 4 to allow for more ALU instructions.
--|
--| @author         Richard James Howe.
--| @copyright      Copyright 2017, 2019 Richard James Howe.
--| @license        MIT
--| @email          howe.r.j.89@gmail.com
--|
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package h2_pkg is
	subtype word    is std_ulogic_vector(15 downto 0);
	subtype address is std_ulogic_vector(12 downto 0);

	constant hardware_cpu_id: word   := X"0666";
	constant simulation_cpu_id: word := X"1984";

	component h2 is
		generic(
			cpu_id:                   word     := hardware_cpu_id; -- Value for the CPU ID instruction
			interrupt_address_length: positive := 3;               -- Log_2 of the number of interrupts
			start_address:            natural  := 0;               -- Initial program counter value
			stack_size_log2:          positive := 6;               -- Log_2 of the Size of the stack
			use_interrupts:           boolean  := true);           -- Enable Interrupts in the H2 Core
		port(
			clk:      in  std_ulogic;
			rst:      in  std_ulogic;

			-- IO interface
			stop:     in  std_ulogic; -- Assert high to halt the H2 core

			io_wr:    out std_ulogic; -- Output Write Enable
			io_re:    out std_ulogic; -- Input  Read  Enable
			io_din:   in  word;      -- Data  Input from register
			io_dout:  out word;      -- Data  Output to register
			io_daddr: out word;      -- Data  Address for I/O action

			irq:      in  std_ulogic; -- Interrupt Request
			irq_addr: in  std_ulogic_vector(interrupt_address_length - 1 downto 0); -- Address to jump to on Interrupt Request

			-- RAM interface, Dual port
			pc:       out address;   -- program counter
			insn:     in  word;      -- instruction

			dwe:      out std_ulogic; -- RAM data write enable
			dre:      out std_ulogic; -- RAM data read enable
			din:      in  word;       -- RAM data input
			dout:     out word;       -- RAM data output
			daddr:    out address);   -- RAM address
	end component;
end;

library ieee,work,std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- only needed for calculations relating to generics
use work.h2_pkg.all;

entity h2 is
	generic(
		cpu_id:                   word     := hardware_cpu_id; -- Value for the CPU ID instruction
		interrupt_address_length: positive := 3;               -- Log_2 of the number of interrupts
		start_address:            natural  := 0;               -- Initial program counter value
		stack_size_log2:          positive := 6;               -- Log_2 of the Size of the stack
		use_interrupts:           boolean  := true);           -- Enable Interrupts in the H2 Core
	port(
		clk:      in  std_ulogic;
		rst:      in  std_ulogic;

		-- IO interface
		stop:     in  std_ulogic; -- Assert high to halt the H2 core

		io_wr:    out std_ulogic; -- Output Write Enable
		io_re:    out std_ulogic; -- Input  Read  Enable
		io_din:   in  word;       -- Data  Input from register
		io_dout:  out word;       -- Data  Output to register
		io_daddr: out word;       -- Data  Address for I/O action

		irq:      in  std_ulogic; -- Interrupt Request
		irq_addr: in  std_ulogic_vector(interrupt_address_length - 1 downto 0); -- Address to jump to on Interrupt Request

		-- RAM interface, Dual port
		pc:       out address;    -- program counter
		insn:     in  word;       -- instruction

		dwe:      out std_ulogic; -- RAM data write enable
		dre:      out std_ulogic; -- RAM data read enable
		din:      in  word;       -- RAM data input
		dout:     out word;       -- RAM data output
		daddr:    out address);   -- RAM address
end;

architecture rtl of h2 is
	signal pc_c:        address := std_ulogic_vector(to_unsigned(start_address, address'length));
	signal pc_n:        address := (others => '0');
	signal pc_plus_one: address := (others => '0');

	constant stack_size: integer := 2 ** stack_size_log2;
	type     stack_type is array (stack_size - 1 downto 0) of word;
	subtype  depth is unsigned(stack_size_log2 - 1 downto 0);

	signal vstkp_c, vstkp_n:  depth := (others => '0');             -- variable stack pointer
	signal vstk_ram: stack_type     := (others => (others => '0')); -- variable stack
	signal dstk_we: std_ulogic      := '0';                         -- variable stack write enable
	signal dd: depth                := (others => '0');             -- variable stack delta

	signal rstkp_c, rstkp_n:  depth := (others => '0');             -- return stack pointer
	signal rstk_ram: stack_type     := (others => (others => '0')); -- return stack
	signal rstk_we: std_ulogic      := '0';                         -- return stack write enable
	signal rd: depth                := (others => '0');             -- return stack delta

	type instruction_info_type is record
		alu:     std_ulogic;
		lit:     std_ulogic;
		branch:  std_ulogic;
		branch0: std_ulogic;
		call:    std_ulogic;
	end record;

	signal is_instr: instruction_info_type := ('0', '0', '0', '0', '0');
	signal is_interrupt: std_ulogic := '0';
	signal is_ram_write: std_ulogic := '0';

	type compare_type is record
		more:  std_ulogic; -- signed greater than; T > N?
		equal: std_ulogic; -- equality; N = T?
		umore: std_ulogic; -- unsigned greater than; T > N?
		zero:  std_ulogic; -- zero test; T = 0?
	end record;

	signal compare: compare_type := ('0', '0', '0', '0');

	signal stop_c, stop_n:     std_ulogic := '0'; -- processor wait state

	signal irq_en_c, irq_en_n: std_ulogic := '0'; -- interrupt enable
	signal irq_c, irq_n:       std_ulogic := '0'; -- pending interrupt request
	signal irq_addr_c, irq_addr_n: std_ulogic_vector(irq_addr'range) := (others => '0'); -- address of pending interrupt request vector

	signal tos_c, tos_n: word := (others => '0'); -- top of stack
	signal nos:          word := (others => '0'); -- next on stack
	signal rtos_c:       word := (others => '0'); -- top of return stack
	signal rstk_data:    word := (others => '0'); -- return stack input
	signal aluop:        std_ulogic_vector(4 downto 0) := (others => '0'); -- ALU operation

	signal instruction:  word := (others => '0'); -- processed 'insn'
begin
	assert stack_size > 4 report "stack size too small: " & integer'image(stack_size) severity failure;

	is_instr.alu     <= '1' when instruction(15 downto 13) = "011" else '0';
	is_instr.lit     <= '1' when instruction(15)           = '1'   else '0';
	is_instr.branch  <= '1' when instruction(15 downto 13) = "000" else '0';
	is_instr.branch0 <= '1' when instruction(15 downto 13) = "001" else '0';
	is_instr.call    <= '1' when instruction(15 downto 13) = "010" else '0';
	is_ram_write     <= '1' when is_instr.alu = '1' and instruction(5) = '1' else '0';
	compare.more     <= '1' when   signed(tos_c) >   signed(nos) else '0';
	compare.umore    <= '1' when unsigned(tos_c) > unsigned(nos) else '0';
	compare.equal    <= '1' when tos_c = nos else '0';
	compare.zero     <= '1' when unsigned(tos_c(15 downto 0)) = 0 else '0';
	nos              <= vstk_ram(to_integer(vstkp_c));
	rtos_c           <= rstk_ram(to_integer(rstkp_c));
	pc               <= pc_n;
	pc_plus_one      <= std_ulogic_vector(unsigned(pc_c) + 1);
	dout             <= nos;
	daddr            <= tos_c(13 downto 1) when is_ram_write = '1' else tos_n(13 downto 1);
	dwe              <= '1' when is_ram_write = '1' and tos_c(15 downto 14) = "00" else '0';
	dre              <= '1' when                        tos_n(15 downto 14) = "00" else '0';
	io_dout          <= nos;
	io_daddr         <= tos_c;
	io_wr            <= '1' when is_ram_write = '1' and tos_c(15 downto 14) /= "00" else '0';
	is_interrupt     <= '1' when irq_c = '1' and irq_en_c = '1' and use_interrupts else '0';
	irq_n            <= irq;
	irq_addr_n       <= irq_addr;
	stop_n           <= stop;
	dd(0)            <= instruction(0);
	rd(0)            <= instruction(2);
	dd(dd'high downto 1) <= (others => '1') when instruction(1) = '1' else (others => '0'); -- sign extend
	rd(rd'high downto 1) <= (others => '1') when instruction(3) = '1' else (others => '0'); -- sign extend
	dstk_we          <= '1' when (is_instr.lit = '1' or (is_instr.alu = '1' and instruction(7) = '1')) else '0';

	next_state: process(clk, rst)
	begin
		if rst = '1' then
			vstkp_c    <= (others => '0');
			rstkp_c    <= (others => '0');
			pc_c       <= std_ulogic_vector(to_unsigned(start_address, pc_c'length));
			tos_c      <= (others => '0');
			stop_c     <= '0';
			irq_en_c   <= '0';
			irq_c      <= '0';
			irq_addr_c <= (others => '0');
		elsif rising_edge(clk) then
			vstkp_c    <= vstkp_n;
			rstkp_c    <= rstkp_n;
			pc_c       <= pc_n;
			tos_c      <= tos_n;
			stop_c     <= stop_n;
			irq_en_c   <= irq_en_n;
			irq_c      <= irq_n;
			irq_addr_c <= irq_addr_n;
		end if;
	end process;

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

	decode: process(insn, irq_addr_c, is_interrupt, stop_c, pc_c)
	begin
		instruction <= insn;
		if stop_c = '1' then -- assert a BRANCH instruction to current location on CPU halt
			instruction <= "000" & pc_c;
		elsif is_interrupt = '1' then -- assemble a CALL instruction on interrupt
			instruction                   <= (others => '0');
			instruction(15 downto 13)     <= "010";      -- turn into a CALL
			instruction(irq_addr_c'range) <= irq_addr_c; -- address to call
		end if;
	end process;

	alu_select: process(instruction, is_instr)
	begin
		aluop <= (others => '0');
		if is_instr.lit = '1' then
			aluop <= "10101";
		elsif is_instr.branch0 = '1' then
			aluop <= (0 => '1', others => '0');
		elsif is_instr.alu = '1' then
			aluop <= instruction(12 downto 8);
		end if;
	end process;

	alu_unit: process(
		tos_c, nos, rtos_c,
		din, instruction, aluop,
		io_din,
		vstkp_c, rstkp_c,
		compare,
		irq_en_c)
	begin
		io_re    <=  '0'; -- hardware reads can have side effects
		tos_n    <=  tos_c;
		irq_en_n <=  irq_en_c;
		case aluop is
		-- Register Operations
		when "00000" => tos_n <= tos_c;
		when "00001" => tos_n <= nos;
		when "01011" => tos_n <= rtos_c;
		when "10100" => tos_n <= cpu_id;
		when "10101" => tos_n <= "0" & instruction(14 downto 0); -- undocumented, may be removed
		-- Logical Operations
		when "00011" => tos_n <= tos_c and nos;
		when "00100" => tos_n <= tos_c or  nos;
		when "00101" => tos_n <= tos_c xor nos;
		when "00110" => tos_n <= not tos_c;
		-- Comparison Operations
		when "00111" => tos_n <= (others => compare.equal);
		when "01000" => tos_n <= (others => compare.more);
		when "01111" => tos_n <= (others => compare.umore);
		when "10011" => tos_n <= (others => compare.zero);
		-- Arithmetic Operations
		when "01001" => tos_n <= word(unsigned(nos) srl to_integer(unsigned(tos_c(3 downto 0))));
		when "01101" => tos_n <= word(unsigned(nos) sll to_integer(unsigned(tos_c(3 downto 0))));
		when "00010" => tos_n <= word(unsigned(nos) + unsigned(tos_c));
		when "01010" => tos_n <= word(unsigned(tos_c) - 1);
		-- Input (output is handled elsewhere)
		when "01100" => -- input: 0x4000 - 0x7FFF is external input
			if tos_c(15 downto 14) /= "00" then
				tos_n <= io_din;
				io_re <= '1';
			else
				tos_n <= din;
			end if;
		-- Stack Depth
		when "01110" => tos_n <= (others => '0');
				tos_n(vstkp_c'range) <= std_ulogic_vector(vstkp_c);
		when "10010" => tos_n <= (others => '0');
				tos_n(rstkp_c'range) <= std_ulogic_vector(rstkp_c);
		-- CPU Status Set/Get
		when "10001" => tos_n    <= (others => '0');
				tos_n(0) <= irq_en_c;
		when "10000" => tos_n    <= nos;
				irq_en_n <= tos_c(0);
		-- Default/Invalid instructions
		when others  => tos_n <= tos_c;
				report "Invalid ALU operation: " & integer'image(to_integer(unsigned(aluop))) severity error;
		end case;
	end process;

	stack_update: process(
		pc_c, instruction, tos_c,
		vstkp_c, dd,
		rstkp_c, rd,
		is_instr, pc_plus_one, is_interrupt)
	begin
		vstkp_n   <= vstkp_c;
		rstkp_n   <= rstkp_c;
		rstk_we   <= '0';
		rstk_data <= "00" & pc_plus_one & "0";

		if is_instr.lit = '1' then
			assert to_integer(vstkp_c) + 1 < stack_size;
			vstkp_n   <= vstkp_c + 1;
		end if;
		if is_instr.alu = '1' then
			assert (not instruction(6) = '1') or ((to_integer(rstkp_c) + to_integer(signed(rd))) < stack_size);
			assert ((to_integer(vstkp_c) + to_integer(signed(dd))) < stack_size);
			rstk_we   <= instruction(6);
			rstk_data <= tos_c;
			vstkp_n   <= vstkp_c + unsigned(dd);
			rstkp_n   <= rstkp_c + unsigned(rd);
		end if;
		if is_instr.branch0 = '1' then
			vstkp_n   <= vstkp_c - 1;
		end if;
		if is_instr.call = '1' then
			if is_interrupt = '1' then
				rstk_data <= "00" & pc_c & "0";
			end if;
			rstkp_n   <= rstkp_c + 1;
			rstk_we   <= '1';
		end if;
	end process;

	pc_update: process(
		instruction, rtos_c, pc_plus_one,
		is_instr,
		compare.zero)
	begin
		pc_n <= pc_plus_one;
		if is_instr.branch = '1' or (is_instr.branch0 = '1' and compare.zero = '1') or is_instr.call = '1' then
			pc_n <=  instruction(12 downto 0);
		elsif is_instr.alu = '1' and instruction(4) = '1' then
			pc_n <=  rtos_c(13 downto 1);
		end if;
	end process;
end architecture;

