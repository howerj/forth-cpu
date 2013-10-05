--------------------------------------------------------------------------------- 
--! @file mem_h2.vhd
--! @brief This is the RAM used by the softcore CPU. 
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
--------------------------------------------------------------------------------- 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
entity mem_h2 is
    port(
        a_clk:    in  std_logic;
        a_dwe:    in  std_logic;
        a_addr:   in  std_logic_vector(12 downto 0);
        a_din:    in  std_logic_vector(15 downto 0);
        a_dout:   out std_logic_vector(15 downto 0);

        b_clk:    in  std_logic;
        b_dwe:    in  std_logic;
        b_addr:   in  std_logic_vector(12 downto 0);
        b_din:    in  std_logic_vector(15 downto 0);
        b_dout:   out std_logic_vector(15 downto 0)

    );
end mem_h2;

architecture rtl of mem_h2 is
    constant ramSz  : positive := 8192;

    type ramArray_t is array ((ramSz - 1 ) downto 0) of std_logic_vector(15 downto 0);

    function initRam(fileName: in string) return ramArray_t is
        variable ramData: ramArray_t;
        file     inFile: text is in fileName;
        variable inputLine:     line;
        variable tmpVar:    bit_vector(15 downto 0);
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

    shared variable ram: ramArray_t:= initRam("mem_h2.binary");
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
