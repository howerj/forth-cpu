-- Richard James Howe.
-- This contains the CPU/main memory cache
--
-- This module does not *do* anything, it just makes things
-- a little tidier and easier to instantiate multiple instances.
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpu is
  port(
    clk:        in   std_logic;
    rst:        in   std_logic;

    cpu_wr:     out  std_logic;
    cpu_din:    in   std_logic_vector(15 downto 0):= (others => '0');
    cpu_dout:   out  std_logic_vector(15 downto 0):= (others => '0');
    cpu_daddr:  out  std_logic_vector(15 downto 0):= (others => '0')
  );
end;

architecture behav of cpu is

  -- CPU, Internal Memory interface to RAM/ROM
  ---- This should interface with some dual port RAM, it forms the
  ---- brain of the system a CPU with some RAM that should contain
  ---- the bootloader (or the entire system).
  signal  pc:                       std_logic_vector(12 downto 0):= (others => '0'); -- Program counter
  signal  insn:                     std_logic_vector(15 downto 0):= (others => '0'); -- Instruction issued by program counter
  signal  mem_dwe:                  std_logic:= '0';   -- Read/Write toggle, 0=Read, 1=Write
  signal  mem_din:                  std_logic_vector(15 downto 0):= (others => '0');
  signal  mem_dout:                 std_logic_vector(15 downto 0):= (others => '0');
  signal  mem_daddr:                std_logic_vector(12 downto 0):= (others => '0');
begin

  -- The actual CPU instance (H2)
  h2_instance: entity work.h2
  port map(
          clk   =>    clk,
          rst   =>    rst,

          -- Instruction and instruction address to CPU
          pco   =>    pc, 
          insn  =>    insn,  

          -- External interface with the 'outside world'
          io_wr   =>  cpu_wr,
          io_din  =>  cpu_din,
          io_dout =>  cpu_dout,
          io_daddr=>  cpu_daddr,

          -- Internal Internal with memory
          dwe     =>    mem_dwe,
          din     =>    mem_din,
          dout    =>    mem_dout,
          daddr   =>    mem_daddr
      );

  -- Dual port RAM for the CPU, acts as bootloader or
  -- contains the full system software
  mem_h2_instance: entity work.mem_h2
  port map(
          -- Port A, Read only, CPU instruction/address
          a_clk   =>    clk,
          a_dwe   =>    '0',
          a_addr  =>    pc,
          a_din   =>    X"0000",
          a_dout  =>    insn,
          -- Port B, Read/Write controlled by CPU instructions
          b_clk   =>    clk,
          b_dwe   =>    mem_dwe,
          b_addr  =>    mem_daddr,
          b_din   =>    mem_dout,
          b_dout  =>    mem_din
      );

end architecture;
