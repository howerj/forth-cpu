-------------------------------------------------------------------------------
--! @file util.vhd
--! @brief          A collection of utilities and simple components,
--!                 generic, reusable componets are listed here, they
--!                 should also be entirely self contained and ready
--!                 to use by themselves.
--! @author         Richard James Howe
--! @copyright      Copyright 2014 Richad James Howe
--! @license        LGPL version 3
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is
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
      reset : in  std_logic;              -- asyncronous reset
      clk   : in  std_logic;
      ce    : in  std_logic;              -- enable counting
      rs    : in  std_logic;              -- syncronous reset
      do    : out integer range (M-1) downto 0 := 0
    );
  end component;

  component memory
    generic(
        --! addr_bitlen and data_bitlen are chosen to be the values they
        --! are as the Spartan 3 has a block RAM size of 18k which this
        --! pretty much fills.
        addr_bitlen: positive := 12;              --! address bit length 
        data_bitlen: positive := 16;              --! data bit length
        filename:    string   := "memory.binary"  --! initial RAM contents
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
end util;

--------------------------------------------------------------------------------- 
--! @file util.vhd
--! @brief "memory" implements a generic dual port block RAM
--! which according to Xilinx's XST guide will synthesize with
--! a file you can specify with a string as the RAMs initial
--! value, stored in ASCII encoded binary
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
        filename:    string   := "memory.binary"  --! initial RAM contents
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

    --! @brief This function will initial the RAM, it reads from
    --! a file that can be specified in a generic way
    function initRam(fileName: in string) return ramArray_t is
        variable ramData: ramArray_t;
        file     inFile: text is in fileName;
        variable inputLine:     line;
        variable tmpVar:    bit_vector(data_bitlen - 1 downto 0);
    begin
        for i in 0 to ramSz - 1 loop
            if not endfile(inFile) then
                readline(inFile,inputLine);
                read(inputLine,tmpVar);
                ramData(i):=to_stdlogicvector(tmpVar);
            else
                ramData(i):=(others => '0'); 
            end if;
        end loop;
        return ramData;
    end function;
    shared variable ram: ramArray_t:= initRam(filename);
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
--! @brief Shift register N-bit, asynchronous reset, synchronous load 
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
--! @brief Counter, asyncronous *and* synchronous reset, up only.
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
    reset : in  std_logic;              -- asyncronous reset
    clk   : in  std_logic;
    ce    : in  std_logic;              -- enable counting
    rs    : in  std_logic;              -- syncronous reset
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
