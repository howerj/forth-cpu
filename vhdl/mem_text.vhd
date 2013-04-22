-- Richard James Howe
--  This RAM module holds the text we want to
--  display on to the monitor. The text buffer
--  holds at least 80*40 characters.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity mem_text is
    port(
        a_clk:    in  std_logic;
        a_dwe:    in  std_logic;
        a_addr:   in  std_logic_vector(11 downto 0);
        a_din:    in  std_logic_vector(7 downto 0);
        a_dout:   out std_logic_vector(7 downto 0) := (others => '0');

        b_clk:    in  std_logic;
        b_dwe:    in  std_logic;
        b_addr:   in  std_logic_vector(11 downto 0);
        b_din:    in  std_logic_vector(7 downto 0);
        b_dout:   out std_logic_vector(7 downto 0) := (others => '0')
    );
end mem_text;

architecture rtl of mem_text is
    constant ramSz  : positive := 4096;

    type ramArray_t is array ((ramSz - 1 ) downto 0) of std_logic_vector(7 downto 0);

    function initRam(fileName: in string) return ramArray_t is
        variable ramData: ramArray_t;
        file     inFile: text is in fileName;
        variable inputLine:     line;
        variable tmpVar:    bit_vector(7 downto 0);
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
    shared variable ram: ramArray_t:= initRam("mem_text.binary");
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
