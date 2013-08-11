-- Arbitrary bit manipulation module, test bench
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.r.j.89@gmail.com

library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity abmm_tb is
end abmm_tb;

architecture behavior of abmm_tb is
  constant  clk_freq:          positive                        :=  1000000000;
  constant  clk_period:        time                            :=  1000 ms / clk_freq;

  signal    wait_flag:         std_logic                       :=  '0';
  signal    tb_clk:            std_logic                       :=  '0';

  signal    tb_s_reg:          std_logic_vector(15 downto 0)   :=  (others => '0'); 
  signal    tb_addr_reg_0_3:   std_logic_vector(15 downto 0)   :=  (others => '0'); 
  signal    tb_addr_reg_4_7:   std_logic_vector(15 downto 0)   :=  (others => '0'); 
  signal    tb_addr_reg_8_11:  std_logic_vector(15 downto 0)   :=  (others => '0');
  signal    tb_addr_reg_12_15: std_logic_vector(15 downto 0)   :=  (others => '0');
  signal    tb_din:            std_logic_vector(15 downto 0)   :=  (others => '0'); 
  signal    tb_dout:           std_logic_vector(15 downto 0)   :=  (others => 'X');
begin
  abmm_uut: entity work.abmm
  port map(
            s_reg => tb_s_reg,
            addr_reg_0_3 => tb_addr_reg_0_3,
            addr_reg_4_7 => tb_addr_reg_4_7,
            addr_reg_8_11 => tb_addr_reg_8_11,
            addr_reg_12_15 => tb_addr_reg_12_15,
            din => tb_din,
            dout => tb_dout
          );

	clk_process: process
	begin
      while wait_flag = '0' loop
        tb_clk	<=	'1';
        wait for clk_period/2;
        tb_clk	<=	'0';
        wait for clk_period/2;
      end loop;
      wait;
	end process;

    -- I/O settings go here.
	stimulus_process: process
	begin
        wait for clk_period * 2;

        -- Reverse bit pattern
        tb_din <= X"F0A5";

        tb_addr_reg_0_3   <= X"CDEF";
        tb_addr_reg_4_7   <= X"89AB";
        tb_addr_reg_8_11  <= X"4567";
        tb_addr_reg_12_15 <= X"0123";

        wait for clk_period;

        -- Rotate left by one
        tb_din <= X"8888";

        tb_addr_reg_0_3   <= X"210F";
        tb_addr_reg_4_7   <= X"6543";
        tb_addr_reg_8_11  <= X"A987";
        tb_addr_reg_12_15 <= X"EDCB";

        wait for clk_period;

        -- Set to zero
        tb_din <= X"F0A5";

        tb_s_reg <= (others => '1');
        tb_addr_reg_0_3   <= X"0000";
        tb_addr_reg_4_7   <= X"0000";
        tb_addr_reg_8_11  <= X"0000";
        tb_addr_reg_12_15 <= X"0000";

        wait for clk_period;

        -- Set to one
        tb_s_reg <= (others => '1');
        tb_addr_reg_0_3   <= X"1111";
        tb_addr_reg_4_7   <= X"1111";
        tb_addr_reg_8_11  <= X"1111";
        tb_addr_reg_12_15 <= X"1111";

        wait for clk_period;

        -- Invert original bit pattern
        tb_s_reg <= (others => '1');
        tb_addr_reg_0_3   <= X"2222";
        tb_addr_reg_4_7   <= X"2222";
        tb_addr_reg_8_11  <= X"2222";
        tb_addr_reg_12_15 <= X"2222";

        wait for clk_period;

        wait_flag   <=  '1';
        wait;
  end process;


end architecture;
