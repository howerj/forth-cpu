-- Richard James Howe.
--  I/O control module
--
-- @author     Richard James Howe.
-- @copyright    Copyright 2013 Richard James Howe.
-- @license    LGPL    
-- @email      howe.r.j.89@gmail.com
library ieee,work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity io is
  generic(
      baud_rate:               positive := 115200;
      clock_frequency:         positive := 100000000
         );
  port(
      clk:  in std_logic;
      rst:  in std_logic;
      ---------
      -- CPU --
      ---------
      io_wr: in std_logic;                         -- Write/Read toggle, 0=Read, 1=Write
      io_din:   out std_logic_vector(15 downto 0); -- CPU intput, IO module output
      io_dout:  in  std_logic_vector(15 downto 0); -- CPU output, IO module input
      io_daddr: in  std_logic_vector(15 downto 0); -- IO address

      --------------------
      -- INPUT / OUTPUT --
      --------------------
      buttons: in std_logic_vector(4 downto 0) := (others => 'X');
      switches: in std_logic_vector(7 downto 0) := (others => 'X');

      led_an:       out std_logic_vector(3 downto 0) := (others => '0'); -- anodes   7 segment display
      led_ka:       out std_logic_vector(7 downto 0) := (others => '0'); -- kathodes 7 segment display
      led_ld:       out std_logic_vector(7 downto 0) := (others => '0'); -- leds
      -- UART
      uart_rx:       in  std_logic                    :=      'X';  -- uart rx 
      uart_tx:       out std_logic                    :=      '0';  -- uart tx
      -- VGA
      vga_red:      out std_logic_vector(2 downto 0) := (others => '0'); 
      vga_green:    out std_logic_vector(2 downto 0) := (others => '0'); 
      vga_blue:     out std_logic_vector(1 downto 0) := (others => '0'); 
      vga_hsync:    out std_logic                    :=      '0';
      vga_vsync:    out std_logic                    :=      '0'
      );
end;

architecture behav of io is
  signal  clk25MHz:                 std_logic:= '0';
  signal  clk50MHz:                 std_logic:= '0';
begin

   -- Xilinx Application Note:
   -- It seems like it buffers the clock correctly here, so no need to
   -- use a DCM.
   ---- Clock divider /2. 
   clk50MHz <= '0' when rst = '1' else
        not clk50MHz when rising_edge(clk);

   ---- Clock divider /2. Pixel clock is 25MHz
   clk25MHz <= '0' when rst = '1' else
        not clk25MHz when rising_edge(clk50MHz);
   ---- End note.


    uart_deglitch: process (clk)
    begin
        if rising_edge(clk) then
            rx_sync <= rx;
            rx_uart <= rx_sync;
            tx <= tx_uart;
        end if;
    end process;

   io_nextState: process(clk,rst)
   begin
     if rst='1' then
       -- LEDs/Switches
       an_c        <=  (others => '0');
       ka_c        <=  (others => '0');
       ld_c        <=  (others => '0');
       -- VGA
       ocrx_c      <=  (others => '0');
       ocry_c      <=  (others => '0');
       octl_c      <=  (others => '0');
       txt_addr_c  <=  (others => '0');
       txt_din_c   <=  (others => '0');
       -- UART
       uart_din_c  <=  (others => '0');
       ack_din_c   <=  '0';
       stb_dout_c  <=  '0';
     elsif rising_edge(clk) then
       -- LEDs/Switches
       an_c        <=  an_n;
       ka_c        <=  ka_n;
       ld_c        <=  ld_n;
       -- VGA
       ocrx_c      <=  ocrx_n;
       ocry_c      <=  ocry_n;
       octl_c      <=  octl_n;
       txt_addr_c  <=  txt_addr_n;
       txt_din_c   <=  txt_din_n;
       -- UART
       uart_din_c  <=  uart_din_n; 
       ack_din_c   <=  ack_din_n;
       uart_dout_c <=  uart_dout_n;
       stb_dout_c  <=  stb_dout_n;
     end if;
   end process;


  io_select: process(
    cpu_io_wr,cpu_io_dout,cpu_io_daddr,
    an_c,ka_c,ld_c,
    ocrx_c,ocry_c,octl_c,
    txt_addr_c,txt_din_c,
    switches,uart_rx,buttons,
    vga_ram_a_dout,
    uart_din_c, ack_din_c,
    uart_dout_c, 
    uart_dout, stb_dout, ack_din,
    stb_dout, stb_dout_c
  )
  begin
    -- Outputs
    an <= an_c;
    ka <= ka_c;
    ld <= ld_c;
    crx_oreg <= ocrx_c;
    cry_oreg <= ocry_c;
    ctl_oreg <= octl_c;

    vga_ram_a_addr <= txt_addr_c;
    vga_ram_a_din  <= txt_din_c;

    uart_din    <= uart_din_c;
    stb_din     <= '0';
    ack_dout    <= '0';

    -- Register defaults
    an_n <= an_c;
    ka_n <= ka_c;
    ld_n <= ld_c;

    ocrx_n <= ocrx_c;
    ocry_n <= ocry_c;
    octl_n <= octl_c;

    txt_addr_n <= txt_addr_c;
    txt_din_n  <= txt_din_c;

    uart_din_n  <=  uart_din_c; 

    if ack_din = '1' then
        ack_din_n <= '1';
    else
        ack_din_n <= ack_din_c;
    end if;

    if stb_dout = '1' then
        stb_dout_n <= '1';
        uart_dout_n <= uart_dout;
        ack_dout <= '1';
    else
        uart_dout_n <=  uart_dout_c;
        stb_dout_n <= stb_dout_c;
    end if;

    cpu_io_din <= (others => '0');
    vga_ram_a_dwe <= '0';

    if cpu_io_wr = '1' then
      -- Write output.
      case cpu_io_daddr(3 downto 0) is
        when "0000" => -- LEDs 7 Segment displays.
          an_n <= cpu_io_dout(3 downto 0);
          ka_n <= cpu_io_dout(15 downto 8);
        when "0001" => -- LEDs, next to switches.
          ld_n <= cpu_io_dout(7 downto 0);
        when "0010" => -- VGA, cursor registers.
          ocrx_n <= cpu_io_dout(6 downto 0);
          ocry_n <= cpu_io_dout(13 downto 8);
        when "0011" => -- VGA, control register.
          octl_n <= cpu_io_dout(6 downto 0);
        when "0100" => -- VGA update address register.
          txt_addr_n <= cpu_io_dout(11 downto 0);
        when "0101" => -- VGA, update register.
          txt_din_n  <= cpu_io_dout(7 downto 0);
        when "0110" => -- VGA write, could be put into the previous statement.
          vga_ram_a_dwe <= '1';
        when "0111" => -- UART write output.
          uart_din_n <= cpu_io_dout(7 downto 0);
        when "1000" => -- UART strobe input.
          stb_din <= '1';
        when "1001" => -- UART acknowledge output.
          ack_dout <= '1';
        when "1010" =>
        when "1011" =>
        when "1100" =>
        when "1101" =>
        when "1110" =>
        when "1111" =>
        when others =>
      end case;
    else
      -- Get input.
      case cpu_io_daddr(3 downto 0) is
        when "0000" => -- Switches, plus direct access to UART bit.
                cpu_io_din <= "0000000000" & uart_rx & buttons;
        when "0001" => 
                cpu_io_din <= X"00" & switches;
        when "0010" => -- VGA, Read VGA text buffer.
                cpu_io_din <= X"00" & vga_ram_a_dout;
        when "0011" => -- UART get input.
                cpu_io_din <= X"00" & uart_dout_c;
        when "0100" => -- UART acknowledged input.
                cpu_io_din <= (0 => ack_din_c, others => '0');
                ack_din_n <= '0';
        when "0101" => -- UART strobe output (write output).
                cpu_io_din <= (0 => stb_dout_c, others => '0');
                stb_dout_n <= '0';
        when "0110" => cpu_io_din <= (others => '0');
        when "0111" => cpu_io_din <= (others => '0');
        when "1000" => cpu_io_din <= (others => '0');
        when "1001" => cpu_io_din <= (others => '0');
        when "1010" => cpu_io_din <= (others => '0');
        when "1011" => cpu_io_din <= (others => '0');
        when "1100" => cpu_io_din <= (others => '0');
        when "1101" => cpu_io_din <= (others => '0');
        when "1110" => cpu_io_din <= (others => '0');
        when "1111" => cpu_io_din <= (others => '0');
        when others => cpu_io_din <= (others => '0');
      end case;
    end if;
  end process;

  u_uart: entity work.uart 
  generic map(
    BAUD_RATE => baud_rate,
    CLOCK_FREQUENCY => clock_frequency
  )
  port map(
   clock => clk,
   reset => rst,
   data_stream_in => uart_din,
   data_stream_in_stb => stb_din,
   data_stream_in_ack => ack_din,
   data_stream_out => uart_dout,
   data_stream_out_stb => stb_dout,
   data_stream_out_ack => ack_dout,
   rx => rx_uart,
   tx => tx_uart
  );

  U_VGA : entity work.vga80x40 port map (
  reset     => rst,
  clk25MHz  => clk25MHz,
  TEXT_A    => vga_ram_b_addr,
  TEXT_D    => vga_ram_b_dout,
  FONT_A    => vga_rom_addr,
  FONT_D    => vga_rom_dout,
  ocrx    => crx_oreg,
  ocry    => cry_oreg,
  octl    => ctl_oreg,
  R       => R_internal,
  G       => G_internal,
  B       => B_internal,
  hsync     => hsync,
  vsync     => vsync
  );

  U_TEXT: entity work.mem_text port map (
  a_clk  => clk25MHz,
  a_dwe  => vga_ram_a_dwe,
  a_addr => vga_ram_a_addr,
  a_din  => vga_ram_a_din,
  a_dout => vga_ram_a_dout,

  b_clk  => clk25MHz,
  b_dwe  => vga_ram_b_dwe,
  b_addr => vga_ram_b_addr,
  b_din  => vga_ram_b_din,
  b_dout => vga_ram_b_dout
  );
  U_FONT: entity work.mem_font port map (
  a_clk => clk25MHz,
  a_addr => vga_rom_addr,
  a_dout => vga_rom_dout
  );
   
  vga_ram_b_dwe  <= '0';
  vga_ram_b_din  <= (others => '0');

-------------------------------------------------------------------------------


end architecture;
