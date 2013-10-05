-------------------------------------------------------------------------------
--! @file cordic.vhd
--! @brief CORDIC implementation.
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

    sin_in:     in  signed(15 downto 0);
    cos_in:     in  signed(15 downto 0);
    ang_in:     in  signed(15 downto 0);

    sin_out:    in  signed(15 downto 0);
    cos_out:    in  signed(15 downto 0);
    ang_out:    in  signed(15 downto 0)
  );
end entity;

architecture behav of cordic is
  type signed_array_16x16 is array (0 to 15) of signed(15 downto 0);
  signal tval_array: signed_array_16x16 <= (others => (others => '0')); -- todo

  signal x_sig:      signed_array_16x16 <= (others => (others => '0'));
  signal y_sig:      signed_array_16x16 <= (others => (others => '0'));
  signal z_sig:      signed_array_16x16 <= (others => (others => '0'));
begin
  cordic_iteration_generator: for i in 1 to 16 generate 
  begin
    cordic_i_in: if i = 1 generate
    begin
      cordic_i: entity work.cordic_iteration
      generic map(
        shiftr => i
                 );
      port map(
        tval => tval_array(i-1), -- todo
        xin  => cos_in,
        yin  => sine_in,
        zin  => ang_in,

        xout => x_sig(0),
        yout => y_sig(0),
        zout => z_sig(0)
              );



    end generate;

    cordic_i_middle: if ((i > 1) and (i < 16)) generate
    begin

    end generate;

    cordic_i_out: if i = 16 generate
    begin

    end generate;

  end generate;
end architecture;
