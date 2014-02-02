-------------------------------------------------------------------------------
--! @file util.vhd
--! @brief          A collection of utilities and simple components,
--!                 generic, reusable components are listed here, they
--!                 should also be entirely self contained and ready
--!                 to use by themselves.
--! @author         Richard James Howe
--! @copyright      Copyright 2014 Richad James Howe
--! @license        LGPL version 3
--! @email          howe.r.j.89@gmail.com
--!
--! Notes:
--! * Two subsections have been written by Javier Valcarce García as part of
--! VGA driver, which are labeled. 
--!
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is

  --- Component definitions ---------------------------------------------------
  component gptimer is
    generic(
      gptimerbits: positive := 16
    );
    port(
      clk:          in std_logic;
      rst:          in std_logic;

      --! Write enables
      ctrin_we:     in std_logic;                     -- write enable

      --! Control register
      ctrin:        in std_logic_vector(gptimerbits - 1 downto 0); 

      -- Timer interrupts
      irq:          out std_logic;                    -- Compare Interrupt
      Q:            out std_logic;                    -- Timer signal
      NQ:           out std_logic                     -- Timer signal inverted
    );
  end component;

  component memory
    generic(
        --! addr_bitlen and data_bitlen are chosen to be the values they
        --! are as the Spartan 3 has a block RAM size of 18k which this
        --! pretty much fills.
        addr_bitlen: positive := 12;              --! address bit length 
        data_bitlen: positive := 16;              --! data bit length
        filename:    string   := "memory.binary"; --! initial RAM contents
        filetype:    string   := "binary"         --! ASCII encoding used
    );
    port(
    --! Port A of dual port RAM
        a_clk:    in  std_logic;
        a_dwe:    in  std_logic;
        a_addr:   in  std_logic_vector(addr_bitlen - 1 downto 0);
        a_din:    in  std_logic_vector(data_bitlen - 1 downto 0);
        a_dout:   out std_logic_vector(data_bitlen - 1 downto 0) := (others => '0');
    --! Port B of dual port RAM
        b_clk:    in  std_logic;
        b_dwe:    in  std_logic;
        b_addr:   in  std_logic_vector(addr_bitlen - 1 downto 0);
        b_din:    in  std_logic_vector(data_bitlen - 1 downto 0);
        b_dout:   out std_logic_vector(data_bitlen - 1 downto 0) := (others => '0')
    );
  end component;

  component losr 
    generic 
    (
      N : integer := 4
    );
    port 
    (
      reset : in  std_logic;
      clk   : in  std_logic;
      load  : in  std_logic;
      ce    : in  std_logic;
      do    : out std_logic := '0';
      di    : in  std_logic_vector(N-1 downto 0)
    );
  end component;

  component ctrm
    generic 
    (
      M : integer := 8
    );
    port (
      reset : in  std_logic;              -- asynchronous reset
      clk   : in  std_logic;
      ce    : in  std_logic;              -- enable counting
      rs    : in  std_logic;              -- synchronous reset
      do    : out integer range (M-1) downto 0 := 0
    );
  end component;
  -----------------------------------------------------------------------------

end util;

-------------------------------------------------------------------------------
--! @file util.vhd
--! @brief General Purpose Timer, for timing! It is of customizable length,
--!        the minimum being 4-bits, one for the actual timing, the other
--!        three for control. (gptimer.vhd, original file name)
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
--! TODO:
--!  * Check this synthesizes correctly.
-------------------------------------------------------------------------------

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity gptimer is
  generic(
    gptimerbits: positive := 16
  );
  port(
    clk:          in std_logic;
    rst:          in std_logic;

    -- Write enables
    ctrin_we:     in std_logic;                     -- write enable

    -- Registers
    ctrin:        in std_logic_vector(gptimerbits - 1 downto 0); -- Control register

    -- Timer interrupts
    irq:          out std_logic;                    -- Compare Interrupt
    Q:            out std_logic;                    -- Timer signal
    NQ:           out std_logic                     -- Timer signal inverted
  );
end entity;

architecture rtl of gptimer is
  constant highest_bit:         positive := gptimerbits - 1;
  constant control_enable_bit:  positive := highest_bit;
  constant local_rst_bit:       positive := highest_bit - 1;
  constant irq_enable_bit:      positive := highest_bit - 2;
  constant timer_highest_bit:   positive := highest_bit - 3;

  signal ctrl_c, ctrl_n:  std_logic_vector(control_enable_bit downto 0) := (others => '0');
  signal q_c, q_n:        std_logic:= '0';

  signal ctr_localrst:    std_logic  := '0';
  signal ctr_enabled:     std_logic  := '0';
  signal ctr_irq_en:      std_logic  := '0';

  signal internalrst:     std_logic  := '0';

  signal compare:         std_logic_vector(timer_highest_bit downto 0) := (others => '0');
  signal count:           unsigned(timer_highest_bit downto 0)         := (others => '0');
begin

  -- synthesis translate_off
  assert (gptimerbits >= 4) report "gptimer needs to be *at least* 4 bits wide, 3 bits for control, one for the counter" severity failure;
  -- synthesis translate_on

  --! Output assignments not in proc
  Q  <= q_c;
  NQ <= not q_c;

  --! Internal assignments
  ctr_enabled     <= ctrl_c(control_enable_bit);
  ctr_localrst    <= ctrl_c(local_rst_bit);
  ctr_irq_en      <= ctrl_c(irq_enable_bit);
  compare         <= ctrl_c(timer_highest_bit downto 0); 

  --! Register current state assignment
  clockRegisters: process(clk, rst)
  begin
    if rst = '1' then
      q_c    <= '0';
      ctrl_c <= (others => '0');
    elsif rising_edge(clk) then
      q_c    <= q_n;
      ctrl_c <= ctrl_n;
    end if;
  end process;

  --! Counter proc
  process (clk, rst)
  begin
    if rst = '1' then
      count <= (others => '0');
    elsif rising_edge(clk) then
      if ctr_localrst = '1' or internalrst = '1' then
        count <= (others => '0');
      elsif ctr_enabled = '1' then
        count <= count + 1;
      else
        count <= count;
      end if;
    end if;
  end process;

  --! Compare proc 
  process(count, ctrin_we, ctrin, ctrl_c, compare, q_c, ctr_irq_en, ctr_enabled)
  begin
    irq         <= '0';
    q_n         <= q_c;
    ctrl_n      <= ctrl_c;
    internalrst <= '0';

    ctrl_n(local_rst_bit)  <= '0'; -- reset!

    if ctrin_we = '1' then
      ctrl_n <= ctrin;
    end if;

    if count = unsigned(compare) and ctr_enabled = '1' then
      if ctr_irq_en = '1' then
        irq <= '1';
      end if;
      internalrst <= '1';
      q_n <= not q_c;
    end if;
  end process;
end architecture;


--------------------------------------------------------------------------------- 
--! @file util.vhd
--! @brief "memory" implements a generic dual port block RAM
--! which according to Xilinx's XST guide will synthesize with
--! a file you can specify with a string as the RAMs initial
--! value, stored in ASCII encoded binary. (memory.vhd, original
--! file name)
--!
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
--------------------------------------------------------------------------------- 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity memory is
    generic(
        --! addr_bitlen and data_bitlen are chosen to be the values they
        --! are as the Spartan 3 has a block RAM size of 18k which this
        --! pretty much fills.
        addr_bitlen: positive := 12;              --! address bit length 
        data_bitlen: positive := 16;              --! data bit length
        filename:    string   := "memory.binary"; --! initial RAM contents
        filetype:    string   := "binary"         --! ASCII encoding used
    );
    port(
    --! Port A of dual port RAM
        a_clk:    in  std_logic;
        a_dwe:    in  std_logic;
        a_addr:   in  std_logic_vector(addr_bitlen - 1 downto 0);
        a_din:    in  std_logic_vector(data_bitlen - 1 downto 0);
        a_dout:   out std_logic_vector(data_bitlen - 1 downto 0) := (others => '0');
    --! Port B of dual port RAM
        b_clk:    in  std_logic;
        b_dwe:    in  std_logic;
        b_addr:   in  std_logic_vector(addr_bitlen - 1 downto 0);
        b_din:    in  std_logic_vector(data_bitlen - 1 downto 0);
        b_dout:   out std_logic_vector(data_bitlen - 1 downto 0) := (others => '0')
    );
end memory;

architecture rtl of memory is
    constant ramSz  : positive := 2 ** addr_bitlen;

    type ramArray_t is array ((ramSz - 1 ) downto 0) of std_logic_vector(data_bitlen - 1 downto 0);

    function hexCharToStdLogicVector(hc: character) return std_logic_vector is
      variable slv: std_logic_vector(3 downto 0);
    begin
      case hc is
        when '0' => slv := "0000";
        when '1' => slv := "0001";
        when '2' => slv := "0010";
        when '3' => slv := "0011";
        when '4' => slv := "0100";
        when '5' => slv := "0101";
        when '6' => slv := "0110";
        when '7' => slv := "0111";
        when '8' => slv := "1000";
        when '9' => slv := "1001";
        when 'A' => slv := "1010";
          when 'a' => slv := "1010";
        when 'B' => slv := "1011";
          when 'b' => slv := "1011";
        when 'C' => slv := "1100";
          when 'c' => slv := "1100";
        when 'D' => slv := "1101";
          when 'd' => slv := "1101";
        when 'E' => slv := "1110";
          when 'e' => slv := "1110";
        when 'F' => slv := "1111";
          when 'f' => slv := "1111";
        when others => slv := "XXXX";
      end case;
      assert (slv /= "XXXX") report " not a valid hex character" severity failure;
      return slv;
    end hexCharToStdLogicVector;

  function to_string(sv: Std_Logic_Vector) return string is
    use Std.TextIO.all;
    variable bv: bit_vector(sv'range) := to_bitvector(sv);
    variable lp: line;
  begin
    write(lp, bv);
    return lp.all;
  end;

    --! @brief This function will initialize the RAM, it reads from
    --! a file that can be specified in a generic way
    --function initRam(fileName: in string, fileType: in string) return ramArray_t is
    function initRam(fileName, fileType: in string) return ramArray_t is
        variable ramData:   ramArray_t;
        file     inFile:    text is in fileName;
        variable inputLine: line;
        variable tmpVar:    bit_vector(data_bitlen - 1 downto 0);
        variable c:         character;
        variable slv:       std_logic_vector(data_bitlen - 1 downto 0);
    begin
        assert (data_bitlen mod 4) = 0 report "(data_bitlen%4)!=0" severity failure;
        for i in 0 to ramSz - 1 loop
            if not endfile(inFile) then
                readline(inFile,inputLine);
                -- change to "bin" and "hex"
                if fileType = "binary" then
                  read(inputLine,tmpVar);
                  ramData(i):=to_stdlogicvector(tmpVar);
                elsif fileType = "hexadecimal" then
                  for j in 1 to (data_bitlen/4) loop
                    c:= inputLine((data_bitlen/4) - j + 1);
                    slv((j*4)-1 downto (j*4)-4) := hexCharToStdLogicVector(c);
                  end loop;
                  ramData(i):= slv;
                elsif fileType = "ascii" then
                  assert false report "Not implemented currently, failing" severity failure;
                else
                  assert false report "Incorrect type given" severity failure;
                end if;
            else
                ramData(i):=(others => '0'); 
            end if;
        end loop;
        return ramData;
    end function;
    shared variable ram: ramArray_t:= initRam(filename, filetype);
begin
    a_ram:process(a_clk)
    begin
        if rising_edge(a_clk) then
            if a_dwe = '1' then
                ram(to_integer(unsigned(a_addr))) := a_din;
            end if;
            a_dout <= ram(to_integer(unsigned(a_addr)));
        end if;
    end process;

    b_ram:process(b_clk)
    begin
        if rising_edge(b_clk) then
            if b_dwe = '1' then
                ram(to_integer(unsigned(b_addr))) := b_din;
            end if;
            b_dout <= ram(to_integer(unsigned(b_addr)));
        end if;
    end process;
end architecture;

-------------------------------------------------------------------------------
--! @file util.vhd
--! @brief Shift register N-bit, asynchronous reset, synchronous load,
--!        (losr.vhd, original file name)
--! and enable
--! @author         Javier Valcarce García
--! @copyright      Copyright 2007 Javier Valcarce García
--! @license        LGPL version 3
--! @email          javier.valcarce@gmail.com
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity losr is
  generic 
  (
    N : integer := 4
  );
  port 
  (
    reset : in  std_logic;
    clk   : in  std_logic;
    load  : in  std_logic;
    ce    : in  std_logic;
    do    : out std_logic := '0';
    di    : in  std_logic_vector(N-1 downto 0)
  );
end losr;

architecture arch of losr is
begin

  process(reset, clk)
    variable data : std_logic_vector(N-1 downto 0):= (others => '0');
  begin
    if reset = '1' then
      data := (others => '0');
    elsif rising_edge(clk) then
      if load = '1' then
        data := di;
      elsif ce = '1' then
        data := data(N-2 downto 0) & "0";
      end if;
    end if;

    do <= data(N-1);
  end process;
end arch;

-------------------------------------------------------------------------------
--! @file ctrm.vhd
--! @brief Counter, asynchronous *and* synchronous reset, up only.
--!        (ctrm.vhd, original filename)
--! @author         Javier Valcarce García
--! @copyright      Copyright 2007 Javier Valcarce García
--! @license        LGPL version 3
--! @email          javier.valcarce@gmail.com
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ctrm is
  generic 
  (
    M : integer := 8
  );
  port (
    reset : in  std_logic;              -- asynchronous reset
    clk   : in  std_logic;
    ce    : in  std_logic;              -- enable counting
    rs    : in  std_logic;              -- synchronous reset
    do    : out integer range (M-1) downto 0 := 0
  );
end ctrm;

architecture arch of ctrm is
  signal c : integer range (M-1) downto 0:= 0;
begin
  do <= c;
  process(reset, clk)
  begin
    if reset = '1' then
      c <= 0;
    elsif rising_edge(clk) then
      if ce = '1' then
        if rs = '1' then
          c <= 0;
        else
          c <= c + 1;
        end if;
      end if;
    end if;
  end process;
end arch;

-------------------------------------------------------------------------------
-- END OF FILE ----------------------------------------------------------------
-------------------------------------------------------------------------------
