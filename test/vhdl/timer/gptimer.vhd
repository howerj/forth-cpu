-- General Purpose Timer
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity gptimer is
  port(
    clk:        in std_logic;
    rst:        in std_logic;
    ctr_r:      in std_logic_vector(15 downto 0); -- Control register
    comp1_r:    in std_logic_vector(15 downto 0); -- Compare value one
    comp2_r:    in std_logic_vector(15 downto 0); -- Compare value two

    interrupt:  out std_logic;                    -- Generate interrupt
    timersig:   out std_logic                     -- Any timer signal
  );
end entity;

architecture rtl of gptimer is
begin

end architecture;
