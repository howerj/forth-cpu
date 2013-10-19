-------------------------------------------------------------------------------
--! @file cordic.vhd
--! @brief CORDIC implementation, fixed point 2.14 format
--! @author         Richard James Howe.
--! @copyright      Copyright 2013 Richard James Howe.
--! @license        LGPL      
--! @email          howe.r.j.89@gmail.com
-------------------------------------------------------------------------------
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cordic is
  port(
    clk:        in  std_logic;
    rst:        in  std_logic;

    sin_in:     in  signed(15 downto 0); -- set to X"0000"
    cos_in:     in  signed(15 downto 0); -- set to X"26DD"
    ang_in:     in  signed(15 downto 0);

    sin_out:    out signed(15 downto 0);
    cos_out:    out signed(15 downto 0);
    ang_out:    out signed(15 downto 0)
  );
end entity;

architecture behav of cordic is
  type signed_array_16x16 is array (0 to 15) of signed(15 downto 0);
  type signed_array_15x16 is array (0 to 14) of signed(15 downto 0);

  --! Array initialization functions: CORDIC Table
  function init_tval return signed_array_16x16 is
    variable tval_array_var: signed_array_16x16;
  begin
    tval_array_var(0)   := signed'(X"3243");
    tval_array_var(1)   := signed'(X"1DAC");
    tval_array_var(2)   := signed'(X"0FAD");
    tval_array_var(3)   := signed'(X"07F5");
    tval_array_var(4)   := signed'(X"03FE");
    tval_array_var(5)   := signed'(X"01FF");
    tval_array_var(6)   := signed'(X"00FF");
    tval_array_var(7)   := signed'(X"007F");
    tval_array_var(8)   := signed'(X"003F");
    tval_array_var(9)   := signed'(X"001F");
    tval_array_var(10)  := signed'(X"000F");
    tval_array_var(11)  := signed'(X"0007");
    tval_array_var(12)  := signed'(X"0003");
    tval_array_var(13)  := signed'(X"0001");
    tval_array_var(14)  := signed'(X"0000");
    tval_array_var(15)  := signed'(X"0000");
    return tval_array_var;
  end function;

  function init_arrZero return signed_array_15x16 is
    variable zero_array: signed_array_15x16;
  begin
    for i in 0 to 14 loop
      zero_array(i) := signed'(X"0000");
    end loop;
    return zero_array;
  end function;

  signal tval_array: signed_array_16x16 := init_tval;
  signal x_sig:      signed_array_15x16 := init_arrZero;
  signal y_sig:      signed_array_15x16 := init_arrZero;
  signal z_sig:      signed_array_15x16 := init_arrZero;

begin
  cordic_iteration_generator: for i in 1 to 16 generate 
  begin
    cordic_i_in: if i = 1 generate
    begin
      cordic_i: entity work.cordic_iteration
      generic map(
        shiftr => 0
                 )
      port map(
        tval => tval_array(0), 
        xin  => cos_in,
        yin  => sin_in,
        zin  => ang_in,

        xout => x_sig(0),
        yout => y_sig(0),
        zout => z_sig(0)
              );
    end generate;

    cordic_i_middle: if ((i > 1) and (i < 16)) generate
    begin
      cordic_i: entity work.cordic_iteration
      generic map(
        shiftr => i-1
                 )
      port map(
        tval => tval_array(i-1),
        xin  => x_sig(i-2),
        yin  => y_sig(i-2),
        zin  => z_sig(i-2),

        xout => x_sig(i-1),
        yout => y_sig(i-1),
        zout => z_sig(i-1)
              );
    end generate;

    cordic_i_out: if i = 16 generate
    begin
      cordic_i: entity work.cordic_iteration
      generic map(
        shiftr => i-1
                 )
      port map(
        tval => tval_array(i-1),
        xin  => x_sig(i-2),
        yin  => y_sig(i-2),
        zin  => z_sig(i-2),

        xout => cos_out,
        yout => sin_out,
        zout => ang_out
              );
    end generate;

  end generate;
end architecture;
