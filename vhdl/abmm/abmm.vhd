-- Arbitrary bit manipulation module
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com

-- TODO, use generators?

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity abmm is
  port(
  s_reg:          in  std_logic_vector(15 downto 0); -- Special bit manipulations

  addr_reg_0_3:   in  std_logic_vector(15 downto 0); -- bits 0 to 3 control reg
  addr_reg_4_7:   in  std_logic_vector(15 downto 0); 
  addr_reg_8_11:  in  std_logic_vector(15 downto 0);
  addr_reg_12_15: in  std_logic_vector(15 downto 0);

  din:            in  std_logic_vector(15 downto 0); -- Word to process
  dout:           out std_logic_vector(15 downto 0)  -- Result
      );
end;

architecture rtl of abmm is
begin

  -- First reg
  b0: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(0),
  c_bit     =>  din(0),
  input     =>  din,
  addr      =>  addr_reg_0_3(3 downto 0),

  output    =>  dout(0)
          );

  b1: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(1),
  c_bit     =>  din(1),
  input     =>  din,
  addr      =>  addr_reg_0_3(7 downto 4),

  output    =>  dout(1)
          );

  b2: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(2),
  c_bit     =>  din(2),
  input     =>  din,
  addr      =>  addr_reg_0_3(11 downto 8),

  output    =>  dout(2)
          );

  b3: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(3),
  c_bit     =>  din(3),
  input     =>  din,
  addr      =>  addr_reg_0_3(15 downto 12),

  output    =>  dout(3)
          );

  -- New reg
  b4: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(4),
  c_bit     =>  din(4),
  input     =>  din,
  addr      =>  addr_reg_4_7(3 downto 0),

  output    =>  dout(4)
          );

  b5: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(5),
  c_bit     =>  din(5),
  input     =>  din,
  addr      =>  addr_reg_4_7(7 downto 4),

  output    =>  dout(5)
          );

  b6: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(6),
  c_bit     =>  din(6),
  input     =>  din,
  addr      =>  addr_reg_4_7(11 downto 8),

  output    =>  dout(6)
          );

  b7: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(7),
  c_bit     =>  din(7),
  input     =>  din,
  addr      =>  addr_reg_4_7(15 downto 12),

  output    =>  dout(7)
          );

  -- New reg
  b8: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(8),
  c_bit     =>  din(8),
  input     =>  din,
  addr      =>  addr_reg_8_11(3 downto 0),

  output    =>  dout(8)
          );

  b9: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(9),
  c_bit     =>  din(9),
  input     =>  din,
  addr      =>  addr_reg_8_11(7 downto 4),

  output    =>  dout(9)
          );

  b10: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(10),
  c_bit     =>  din(10),
  input     =>  din,
  addr      =>  addr_reg_8_11(11 downto 8),

  output    =>  dout(10)
          );

  b11: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(11),
  c_bit     =>  din(11),
  input     =>  din,
  addr      =>  addr_reg_8_11(15 downto 12),

  output    =>  dout(11)
          );

  -- New reg        
  b12: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(12),
  c_bit     =>  din(12),
  input     =>  din,
  addr      =>  addr_reg_12_15(3 downto 0),

  output    =>  dout(12)
          );

  b13: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(13),
  c_bit     =>  din(13),
  input     =>  din,
  addr      =>  addr_reg_12_15(7 downto 4),

  output    =>  dout(13)
          );

  b14: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(14),
  c_bit     =>  din(14),
  input     =>  din,
  addr      =>  addr_reg_12_15(11 downto 8),

  output    =>  dout(14)
          );

  b15: entity work.abmm_multiplexer 
  port map(
  s         =>  s_reg(15),
  c_bit     =>  din(15),
  input     =>  din,
  addr      =>  addr_reg_12_15(15 downto 12),

  output    =>  dout(15)
          );

end architecture;

