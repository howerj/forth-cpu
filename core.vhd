--------------------------------------------------------------------------------
--| @file core.vhd
--| @brief This contains the CPU, memory and interrupt handler instances
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2013,2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use work.util.n_bits;
use work.h2_pkg.all;

package core_pkg is
	type cpu_debug_interface is record
		pc:        address;
		insn:      word;
		dwe:       std_logic;
		dre:       std_logic;
		din:       word;
		dout:      word;
		daddr:     address;
	end record;

	component core is
	generic(number_of_interrupts: positive := 8);
	port(
		-- synthesis translate_off
		debug:           out cpu_debug_interface;
		-- synthesis translate_on

		clk:             in   std_logic;
		rst:             in   std_logic;

		stop:            in   std_logic; -- Halts the CPU

		io_wr:           out  std_logic; -- I/O Write enable
		io_re:           out  std_logic; -- hardware *READS* can have side effects
		io_din:          in   word;
		io_dout:         out  word:= (others => 'X');
		io_daddr:        out  word:= (others => 'X');

		-- Interrupts
		cpu_irq:         in std_logic;
		cpu_irc:         in std_logic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask:    in std_logic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask_we: in std_logic);
	end component;

	component interrupt_request_handler is
	generic(
		number_of_interrupts:   positive := 8;
		lowest_interrupt_first: boolean  := true);
	port(
		clk:     in  std_logic;
		rst:     in  std_logic;

		irq_i:   in  std_logic;
		irc_i:   in  std_logic_vector(number_of_interrupts - 1 downto 0);

		mask:    in  std_logic_vector(number_of_interrupts - 1 downto 0);
		mask_we: in  std_logic;

		irq_o:   out std_logic;
		addr_o:  out std_logic_vector(n_bits(number_of_interrupts) - 1 downto 0));
	end component;
end package;

----- CPU ----------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use work.util.n_bits;
use work.core_pkg.all;
use work.h2_pkg.all;
use work.util.file_format;
use work.util.FILE_HEX;

entity core is
	generic(number_of_interrupts: positive := 8);
	port(
		-- synthesis translate_off
		debug:           out cpu_debug_interface;
		-- synthesis translate_on

		clk:             in   std_logic;
		rst:             in   std_logic;

		stop:            in   std_logic; -- Halts the CPU

		io_wr:           out  std_logic; -- I/O Write enable
		io_re:           out  std_logic; -- hardware *READS* can have side effects
		io_din:          in   word;
		io_dout:         out  word := (others => 'X');
		io_daddr:        out  word := (others => 'X');

		-- Interrupts
		cpu_irq:         in std_logic;
		cpu_irc:         in std_logic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask:    in std_logic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask_we: in std_logic);
end;

architecture structural of core is
	constant interrupt_address_length: natural     := n_bits(number_of_interrupts);
	constant file_name:                string      := "h2.hex";
	constant file_type:                file_format := FILE_HEX;

	signal pc:    address   := (others => '0'); -- Program counter
	signal insn:  word      := (others => '0'); -- Instruction issued by program counter
	signal dwe:   std_logic := '0'; -- Write enable
	signal dre:   std_logic := '0'; -- Read enable
	signal din:   word      := (others => '0');
	signal dout:  word      := (others => '0');
	signal daddr: address   := (others => '0');

	signal h2_irq:       std_logic := '0';
	signal h2_irq_addr:  std_logic_vector(interrupt_address_length - 1 downto 0) := (others=>'0');
begin
	-- synthesis translate_off
	debug.pc    <= pc;
	debug.insn  <= insn;
	debug.dwe   <= dwe;
	debug.dre   <= dre;
	debug.din   <= din;
	debug.dout  <= dout;
	debug.daddr <= daddr;
	-- synthesis translate_on

	irqh_0: work.core_pkg.interrupt_request_handler
	generic map(number_of_interrupts => number_of_interrupts)
	port map(
		clk    => clk,
		rst    => rst,

		irq_i  => cpu_irq,
		irc_i  => cpu_irc,

		irq_o  => h2_irq,
		addr_o => h2_irq_addr,

		mask    => cpu_irc_mask,
		mask_we => cpu_irc_mask_we);

	h2_0: work.h2_pkg.h2 -- The actual CPU instance (H2)
	generic map(interrupt_address_length => interrupt_address_length)
	port map(
		clk       =>    clk,
		rst       =>    rst,

		-- External interface with the 'outside world'
		stop      =>  stop,
		io_wr     =>  io_wr,
		io_re     =>  io_re,
		io_din    =>  io_din,
		io_dout   =>  io_dout,
		io_daddr  =>  io_daddr,

		irq       =>  h2_irq,
		irq_addr  =>  h2_irq_addr,

		-- Instruction and instruction address to CPU
		pc        =>  pc,
		insn      =>  insn,
		-- Fetch/Store
		dwe       =>  dwe,
		dre       =>  dre,
		din       =>  din,
		dout      =>  dout,
		daddr     =>  daddr);

	mem_h2_0: entity work.dual_port_block_ram
	generic map(
		addr_length   => address'length,
		data_length   => word'length,
		file_name     => file_name,
		file_type     => file_type)
	port map(
		-- Port A, Read only, CPU instruction/address
		a_clk   =>    clk,
		a_dwe   =>    '0',
		a_dre   =>    '1',
		a_addr  =>    pc,
		a_din   =>    (others => '0'),
		a_dout  =>    insn,
		-- Port B, Read/Write controlled by CPU instructions
		b_clk   =>    clk,
		b_dwe   =>    dwe,
		b_dre   =>    dre,
		b_addr  =>    daddr,
		b_din   =>    dout,
		b_dout  =>    din);

end architecture;

--------------------------------------------------------------------------------
--| @brief Interrupt request handler, while the CPU can handle interrupts
--|        it does not to a good job of it. This allows customization of
--|        priority.
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| This is a simple interrupt handler, interrupts are decoded in priority
--| order which can be set by a generic. If an interrupt occurs and then
--| another interrupt of the same type occurs before it has been processed
--| the second interrupt will be lost.
--|
--------------------------------------------------------------------------------

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- only needed for calculations relating to generics
use work.util.reg;
use work.util.n_bits;
use work.util.select_bit;
use work.util.priority;

entity interrupt_request_handler is
	generic(
		number_of_interrupts:   positive := 8;
		lowest_interrupt_first: boolean  := true);
	port(
		clk:     in  std_logic;
		rst:     in  std_logic;

		irq_i:   in  std_logic;
		irc_i:   in  std_logic_vector(number_of_interrupts - 1 downto 0);

		mask:    in  std_logic_vector(number_of_interrupts - 1 downto 0);
		mask_we: in  std_logic;

		irq_o:   out std_logic;
		addr_o:  out std_logic_vector(n_bits(number_of_interrupts) - 1 downto 0));
end;

architecture rtl of interrupt_request_handler is
	constant addr_length: natural := n_bits(number_of_interrupts);
	signal irq_n: std_logic := '0';
	signal irc_n: std_logic_vector(irc_i'range) := (others => '0');

	signal addr:  std_logic_vector(addr_length - 1 downto 0) := (others => '0');
	signal irq:   std_logic := '0';

	signal mask_n: std_logic_vector(mask'range) := (others => '0');
begin

	irq_in: entity work.reg
		generic map(
			N      => 1)
		port map(
			clk    =>  clk,
			rst    =>  rst,
			we     =>  '1',
			di(0)  =>  irq_i,
			do(0)  =>  irq_n);

	irc_in: entity work.reg
		generic map(
			N    => number_of_interrupts)
		port map(
			clk  =>  clk,
			rst  =>  rst,
			we   =>  '1',
			di   =>  irc_i,
			do   =>  irc_n);

	irc_mask: entity work.reg generic map(
			N    => number_of_interrupts)
		port map(
			clk  =>  clk,
			rst  =>  rst,
			we   =>  mask_we,
			di   =>  mask,
			do   =>  mask_n);

	process(irc_n, irq_n, mask_n)
		variable addr_n: std_logic_vector(addr'range) := (others => '0');
	begin
		addr_n := priority(irc_n, not lowest_interrupt_first);
		addr <= addr_n;
		if select_bit(mask_n, addr_n) = '1' then
			irq <= irq_n;
		else
			irq <= '0';
		end if;
	end process;

	irq_out: entity work.reg
		generic map(
			N      => 1)
		port map(
			clk    =>  clk,
			rst    =>  rst,
			we     =>  '1',
			di(0)  =>  irq,
			do(0)  =>  irq_o);

	addr_out: entity work.reg
		generic map(
			N      => addr_length)
		port map(
			clk    =>  clk,
			rst    =>  rst,
			we     =>  '1',
			di     =>  addr,
			do     =>  addr_o);

end architecture;
