-- GP Timer test bench

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity gptimer_test_bench is
end entity;

architecture simulation of gptimer_test_bench is
  constant clk_freq:     positive                        :=  1000000000;
  constant clk_period:   time                            :=  1000 ms / clk_freq;

  signal   wait_flag:    std_logic                       :=  '0';
  signal   tb_clk:       std_logic                       :=  '0';
  signal   tb_rst:       std_logic                       :=  '1';

  signal   tb_timer_reset: std_logic :=  '0';

  signal   tb_ctr_r_we:    std_logic :=  '0';
  signal   tb_comp1_r_we:  std_logic :=  '0';
  signal   tb_comp2_r_we:  std_logic :=  '0';
  signal   tb_load1_r_we:  std_logic :=  '0';
  signal   tb_load2_r_we:  std_logic :=  '0';

  signal   tb_load_s_we:   std_logic :=  '0';

  signal   tb_ctr_r:       std_logic_vector(15 downto 0) := (others =>'0'); 
  signal   tb_comp1_r:     std_logic_vector(15 downto 0) := (others =>'0'); 
  signal   tb_comp2_r:     std_logic_vector(15 downto 0) := (others =>'0'); 
  signal   tb_load1_r:     std_logic_vector(15 downto 0) := (others =>'0'); 
  signal   tb_load2_r:     std_logic_vector(15 downto 0) := (others =>'0'); 

  signal   tb_load_s:      std_logic_vector(15 downto 0) := (others =>'0'); 

  signal   tb_irq_comp1:   std_logic := 'X';
  signal   tb_irq_comp2:   std_logic := 'X';
  signal   tb_q:           std_logic := 'X';                     
  signal   tb_nq:          std_logic := 'X';                     
begin

  gpt_timer_uut: entity work.gptimer
  port map(
    clk         => tb_clk,
    rst         => tb_rst,
    timer_reset => tb_timer_reset,
    ctr_r_we    => tb_ctr_r_we,
    comp1_r_we  => tb_comp1_r_we,
    comp2_r_we  => tb_comp2_r_we,

    load1_r_we  => tb_load1_r_we,
    load2_r_we  => tb_load2_r_we,

    load_s_we   => tb_load_s_we,

    ctr_r       => tb_ctr_r,
    comp1_r     => tb_comp1_r,
    comp2_r     => tb_comp2_r,
    load1_r     => tb_load1_r,
    load2_r     => tb_load2_r,

    load_s      => tb_load_s,

    irq_comp1   => tb_irq_comp1,
    irq_comp2   => tb_irq_comp2,
    q           => tb_q,
    nq          => tb_nq
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

	stimulus_process: process
	begin
    tb_rst        <= '1';
    wait for clk_period;

    tb_rst        <= '0';

    tb_ctr_r_we   <= '1';
    tb_ctr_r      <= X"B180";

    tb_comp1_r_we <= '1';
    tb_comp1_r    <= X"0004";
    tb_comp2_r_we <= '1';
    tb_comp2_r    <= X"0008";

    tb_load2_r_we <= '1';
    tb_load2_r    <= X"FFF3";

    wait for clk_period;

    tb_ctr_r_we   <= '0';
    tb_ctr_r      <= X"0000";
    tb_comp1_r_we <= '0';
    tb_comp1_r    <= X"0000";
    tb_comp2_r_we <= '0';
    tb_comp2_r    <= X"0000";
    tb_load2_r_we <= '0';
    tb_load2_r    <= X"0000";
    wait for clk_period * 26;
    tb_ctr_r_we   <= '1';
    tb_ctr_r      <= X"B0A0";
    wait for clk_period * 1;
    tb_ctr_r_we   <= '0';
    wait for clk_period * 38;

    wait_flag <= '1';
    wait;
  end process;

end architecture;
