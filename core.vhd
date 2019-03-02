--------------------------------------------------------------------------------
--| @file core.vhd
--| @brief This contains the CPU, memory and interrupt handler instances
--|
--| @author     Richard James Howe.
--| @copyright  Copyright 2013,2017 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| @todo Make a test bench for the H2 core for executing small sections of
--| code, in succession, and testing the response.
--------------------------------------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use work.util.n_bits;
use work.util.common_generics;
use work.h2_pkg.all;

package core_pkg is
	type cpu_debug_interface is record
		pc:        address;
		insn:      word;
		dwe:       std_ulogic;
		dre:       std_ulogic;
		din:       word;
		dout:      word;
		daddr:     address;
	end record;

	component core is
	generic(g: common_generics; number_of_interrupts: positive := 8);
	port(
		-- synthesis translate_off
		debug:           out cpu_debug_interface;
		-- synthesis translate_on

		clk:             in   std_ulogic;
		rst:             in   std_ulogic;

		stop:            in   std_ulogic; -- Halts the CPU

		io_wr:           out  std_ulogic; -- I/O Write enable
		io_re:           out  std_ulogic; -- hardware *READS* can have side effects
		io_din:          in   word;
		io_dout:         out  word:= (others => 'X');
		io_daddr:        out  word:= (others => 'X');

		-- Interrupts
		cpu_irc:         in std_ulogic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask:    in std_ulogic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask_we: in std_ulogic);
	end component;

	component interrupt_request_handler is
	generic(
		g: common_generics;
		number_of_interrupts:   positive := 8;
		lowest_interrupt_first: boolean  := true);
	port(
		clk:     in  std_ulogic;
		rst:     in  std_ulogic;

		irq_i:   in  std_ulogic;
		irc_i:   in  std_ulogic_vector(number_of_interrupts - 1 downto 0);

		mask:    in  std_ulogic_vector(number_of_interrupts - 1 downto 0);
		mask_we: in  std_ulogic;

		irq_o:   out std_ulogic;
		addr_o:  out std_ulogic_vector(n_bits(number_of_interrupts) - 1 downto 0));
	end component;
end package;

----- CPU ----------------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use work.core_pkg.all;
use work.h2_pkg.all;
use work.util.n_bits;
use work.util.common_generics;
use work.util.file_format;
use work.util.file_hex;
use work.util.or_reduce;

entity core is
	generic(g: common_generics; number_of_interrupts: positive := 8);
	port(
		-- synthesis translate_off
		debug:           out cpu_debug_interface;
		-- synthesis translate_on

		clk:             in   std_ulogic;
		rst:             in   std_ulogic;

		stop:            in   std_ulogic; -- Halts the CPU

		io_wr:           out  std_ulogic; -- I/O Write enable
		io_re:           out  std_ulogic; -- hardware *READS* can have side effects
		io_din:          in   word;
		io_dout:         out  word := (others => 'X');
		io_daddr:        out  word := (others => 'X');

		-- Interrupts
		cpu_irc:         in std_ulogic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask:    in std_ulogic_vector(number_of_interrupts - 1 downto 0);
		cpu_irc_mask_we: in std_ulogic);
end;

architecture structural of core is
	constant interrupt_address_length: natural     := n_bits(number_of_interrupts);
	constant file_name:                string      := "h2.hex";
	constant file_type:                file_format := file_hex;
	signal pc:    address    := (others => '0'); -- Program counter
	signal insn:  word       := (others => '0'); -- Instruction issued by program counter
	signal dwe:   std_ulogic := '0'; -- Write enable
	signal dre:   std_ulogic := '0'; -- Read enable
	signal din:   word       := (others => '0');
	signal dout:  word       := (others => '0');
	signal daddr: address    := (others => '0');
	signal irc_edges:    std_ulogic_vector(cpu_irc'range) := (others => '0');
	signal irq_edges:    std_ulogic := '0';
	signal h2_irq:       std_ulogic := '0';
	signal h2_irq_addr:  std_ulogic_vector(interrupt_address_length - 1 downto 0) := (others=>'0');
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

	-- Ensure all interrupts occur are rising edge triggered
	edges: work.util.rising_edge_detectors
	generic map(g => g, N => cpu_irc'length)
	port map(
		clk   => clk,
		rst   => rst,
		di    => cpu_irc,
		do    => irc_edges);
		
	irq_edges <= or_reduce(irc_edges);

	irqh_0: work.core_pkg.interrupt_request_handler
	generic map(g => g, number_of_interrupts => number_of_interrupts)
	port map(
		clk    => clk,
		rst    => rst,

		irq_i  => irq_edges,
		irc_i  => irc_edges,

		irq_o  => h2_irq,
		addr_o => h2_irq_addr,

		mask    => cpu_irc_mask,
		mask_we => cpu_irc_mask_we);

	h2_0: work.h2_pkg.h2 -- The actual CPU instance (H2)
	generic map(asynchronous_reset => g.asynchronous_reset, delay => g.delay, interrupt_address_length => interrupt_address_length)
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
		g             => g,
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
--| @copyright  Copyright 2017, 2019 Richard James Howe.
--| @license    MIT
--| @email      howe.r.j.89@gmail.com
--|
--| This is a simple interrupt handler, interrupts are decoded in priority
--| order which can be set by a generic. If an interrupt occurs and then
--| another interrupt of occurs before it has been processed the second 
--| interrupt will be lost.
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
use work.util.common_generics;

entity interrupt_request_handler is
	generic(
		g: common_generics;
		number_of_interrupts:   positive := 8;
		lowest_interrupt_first: boolean  := true);
	port(
		clk:     in  std_ulogic;
		rst:     in  std_ulogic;

		irq_i:   in  std_ulogic;
		irc_i:   in  std_ulogic_vector(number_of_interrupts - 1 downto 0);

		mask:    in  std_ulogic_vector(number_of_interrupts - 1 downto 0);
		mask_we: in  std_ulogic;

		irq_o:   out std_ulogic;
		addr_o:  out std_ulogic_vector(n_bits(number_of_interrupts) - 1 downto 0));
end;

architecture rtl of interrupt_request_handler is
	constant addr_length: natural := n_bits(number_of_interrupts);
	signal irq_n: std_ulogic := '0';
	signal irc_n: std_ulogic_vector(irc_i'range) := (others => '0');

	signal addr:  std_ulogic_vector(addr_length - 1 downto 0) := (others => '0');
	signal irq:   std_ulogic := '0';

	signal mask_n: std_ulogic_vector(mask'range) := (others => '0');
begin
	irq_in: entity work.reg
		generic map(g => g, N  => 1)
		port map(
			clk    =>  clk,
			rst    =>  rst,
			we     =>  '1',
			di(0)  =>  irq_i,
			do(0)  =>  irq_n);

	irc_in: entity work.reg
		generic map(g => g, N  => number_of_interrupts)
		port map(
			clk  =>  clk,
			rst  =>  rst,
			we   =>  '1',
			di   =>  irc_i,
			do   =>  irc_n);

	irc_mask: entity work.reg generic map(g => g, N  => number_of_interrupts)
		port map(
			clk  =>  clk,
			rst  =>  rst,
			we   =>  mask_we,
			di   =>  mask,
			do   =>  mask_n);

	process(irc_n, irq_n, mask_n)
		variable addr_n: std_ulogic_vector(addr'range) := (others => '0');
	begin
		addr_n := priority(irc_n, not lowest_interrupt_first);
		addr_o <= addr_n after g.delay;
		if select_bit(mask_n, addr_n) = '1' then
			irq_o <= irq_n after g.delay;
		else
			irq_o <= '0' after g.delay;
		end if;
	end process;

end architecture;
