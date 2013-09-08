-- SPI AD5641 interface.

library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity dac_test_bench is
end entity;

architecture simulation of dac_test_bench is
  constant clk_freq:     positive                        :=  1000000000;
  constant clk_period:   time                            :=  1000 ms / clk_freq;

  signal wait_flag:       std_logic                       :=  '0';
  signal tb_clk:          std_logic                       :=  '0';
  signal tb_clk25Mhz:     std_logic                       :=  '0';
  signal tb_rst:          std_logic                       :=  '1';

  signal tb_ctr_r_we:     std_logic;                     -- ctr_r write enable
  signal tb_comp1_r_we:   std_logic;                     -- comp1_r write enable
  signal tb_comp2_r_we:   std_logic;                     -- comp2_r write enable
  signal tb_direct_r_we:  std_logic;                     -- direct_r write enable
  signal tb_ram_s_we:     std_logic;                     -- ram_s write enable
  signal tb_ctr_r:        std_logic_vector(15 downto 0); -- Control register
  signal tb_comp1_r:      std_logic_vector(15 downto 0); -- Compare value one
  signal tb_comp2_r:      std_logic_vector(15 downto 0); -- Compare value two
  signal tb_direct_r:     std_logic_vector(15 downto 0); -- Load DAV value directly
  signal tb_ram_s_addr:   std_logic_vector(15 downto 0); -- DAC RAM Address
  signal tb_ram_s_data_i: std_logic_vector(15 downto 0); -- DAC RAM Data (Input)
  signal tb_ram_s_data_o: std_logic_vector(15 downto 0); -- DAC RAM Data (Output)
  signal tb_irq_comp1:    std_logic;                     -- Compare one Interrupt
  signal tb_irq_comp2:    std_logic;                     -- Compare two Interrupt

  signal tb_cs, tb_oclk, tb_odata, tb_done: std_logic;   -- SPI, output only

begin

  dac_uut: entity work.dac
  port map(
    clk       => tb_clk,
    clk25MHz  => tb_clk25MHz,
    rst       => tb_rst,

    ctr_r_we => tb_ctr_r_we,
    comp1_r_we => tb_comp1_r_we,
    comp2_r_we => tb_comp2_r_we,
    direct_r_we => tb_direct_r_we,
    ram_s_we => tb_ram_s_we,
    ctr_r => tb_ctr_r,
    comp1_r => tb_comp1_r,
    comp2_r => tb_comp2_r,
    direct_r => tb_direct_r,
    ram_s_addr => tb_ram_s_addr,
    ram_s_data_i => tb_ram_s_data_i,
    ram_s_data_o => tb_ram_s_data_o,
    irq_comp1 => tb_irq_comp1,
    irq_comp2 => tb_irq_comp2,
    cs  => tb_cs ,
    oclk => tb_oclk,
    odata => tb_odata,
    done => tb_done
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

    wait for clk_period * 16;
    wait_flag   <=  '1';
    wait;
  end process;

end architecture;
