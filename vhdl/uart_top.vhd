
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity UART_TOP is
    port 
    (  
        -- General
        CLOCK_Y3                :   in      std_logic;
        USER_RESET              :   in      std_logic;    
        USB_RS232_RXD           :   in      std_logic;
        USB_RS232_TXD           :   out     std_logic
    );
end UART_TOP;

architecture RTL of UART_TOP is

    ----------------------------------------------------------------------------
    -- Component declarations
    ----------------------------------------------------------------------------
    
    component LOOPBACK is
        port 
        (  
            -- General
            CLOCK                   :   in      std_logic;
            RESET                   :   in      std_logic;    
            RX                      :   in      std_logic;
            TX                      :   out     std_logic
        );
    end component LOOPBACK;
    
    ----------------------------------------------------------------------------
    -- Signals
    ----------------------------------------------------------------------------

    signal tx, rx, rx_sync, reset, reset_sync : std_logic;
    
begin

    ----------------------------------------------------------------------------
    -- Loopback instantiation
    ----------------------------------------------------------------------------

    LOOPBACK_inst1 : LOOPBACK
    port map    (  
            -- General
            CLOCK       => CLOCK_Y3,
            RESET       => reset, 
            RX          => rx,
            TX          => tx
    );
    
    ----------------------------------------------------------------------------
    -- Deglitch inputs
    ----------------------------------------------------------------------------
    
    DEGLITCH : process (CLOCK_Y3)
    begin
        if rising_edge(CLOCK_Y3) then
            rx_sync         <= USB_RS232_RXD;
            rx              <= rx_sync;
            reset_sync      <= USER_RESET;
            reset           <= reset_sync;
            USB_RS232_TXD   <= tx;
        end if;
    end process;
end RTL;
