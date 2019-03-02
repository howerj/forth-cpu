-- BRIEF:     UART TX/RX module
-- LICENSE:   MIT
-- COPYRIGHT: Richard James Howe (2019)
--
-- The UART (Universal Asynchronous Receiver/Transmitter) is one of the simplest
-- serial communication methods available. It is often used for debugging, for
-- issuing commands to embedded devices and sometimes even for uploading firmware
-- to devices. The data format and speed are configurable but there is no method
-- for automatically configuring a UART, both sides must have agreed on the
-- settings before hand. Configurable options include; baud, use of an
-- even of odd parity bit, number of data bits and number of stop bits.
--
-- The clock is not transmitted as part of the signal (which is why baud
-- must be agreed upon before hand), a single packet starts with a 'start bit',
-- where the line goes low. The receiver must synchronize to this start bit (it
-- resets the clock that generates pulse at the sample rate and baud when it
-- encounters a start bit).
--
-- A transmission with 8 data bits, 1 parity bit and 1 stop bit looks like
-- this:
-- ____   ______________________________   ________________
--     \_/|0|1|2|3|4|5|6|7|P|S|         \_/
--     Start   Data     Parity Stop    |--- More data --->
--
-- Start bits are always low, stop bits high. The most common format is
-- 8 data bits, no parity bit, and 1 stop bit at either 9600 or 115200 baud.
--
-- For the receiver a clock that is a multiple of the baud is used so the
-- bits can be sampled with a higher frequency than the bit rate.
--
-- See: 
-- * <https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter>
--
-- TODO: We could make this very configurable; sending N-bits, variable baud, 
-- even/odd parity, etc. We could even allow the transmission of a break 
-- (all zeros). The module could be made to be generic, but the logic removed
-- by setting the write enable for that setting to '1' and the value to a
-- a constant value. This would be the best of both worlds. Another configuration
-- option would be whether or not we both the rest of the system if there is
-- a frame or parity error on the UART RX side.
-- TODO: Convert this so records are used for the state, this should make
-- things a little more compact.
-- TODO: Fold this into 'util.vhd'.
-- TODO: The generics need cleaning up and renaming (N, D, tx_init and
-- rx_init should be set by baud rate and not clock value).
-- TODO: Add an optional FIFO which to be instantiated, and make the
-- top module have a FIFO interface regardless.
-- NOTE: A much more simple UART TX module could also be potentially more 
-- powerful. Instead of having states for start and stop bits, instead the
-- user of the system could prepare a register with the start bit, data, parity,
-- and stop bits. This would mean you could make a 5 bit UART with 1 parity and
-- 3 stop bits - if you wanted, just by loading the register with the right value.
-- The only thing the UART would need to do is; output each bit in sequence at
-- the correct baud, and inform the producer when it is busy.
-- NOTE: We could replace this entire package with an entirely software driven
-- solution. The only hardware we would need two timers driven at the sample
-- rate (one for RX, one for TX) and a deglitched RX signal. An interrupt
-- would be generated on the timers expiry.
library ieee, work;
use ieee.std_logic_1164.all;

package uart_pkg is
	component uart_tx is
		generic (
			N:                  positive; 
			stops:              positive; 
			use_parity:         boolean; 
			asynchronous_reset: boolean := true; 
			even_parity:        boolean := true
		);
		port (
			clk:   in std_ulogic;
			rst:   in std_ulogic;
			baud:  in std_ulogic;
			tx:   out std_ulogic;
			ok:   out std_ulogic;
			we:    in std_ulogic;
			di:    in std_ulogic_vector(N - 1 downto 0));
	end component;

	component uart_rx is
		generic (
			N:                  positive; 
			D:                  positive; 
			stops:              positive; 
			use_parity:         boolean; 
			asynchronous_reset: boolean := true; 
			even_parity:        boolean := true
		);
		port (
			clk:   in std_ulogic;
			rst:   in std_ulogic;
			cr:   out std_ulogic;
			baud, sample: in std_ulogic;
			failed: out std_ulogic_vector(1 downto 0);
			rx:    in std_ulogic;
			we:   out std_ulogic;
			do:   out std_ulogic_vector(N - 1 downto 0));
	end component;
	
	component uart_baud is -- Generates a pulse at the sample rate and baud
		generic (clock_frequency: positive; asynchronous_reset: boolean; init: integer; N: positive := 16; D: positive := 3);
		port (
			clk:      in std_ulogic; 
			rst:      in std_ulogic; 
			we:       in std_ulogic;
			cnt:      in std_ulogic_vector(N - 1 downto 0);
			cr:       in std_ulogic := '0';
			sample:  out std_ulogic; 
			baud:    out std_ulogic);
	end component;

	component uart_top is
		generic (clock_frequency: positive; asynchronous_reset: boolean; tx_init, rx_init: integer; N: positive := 8);
		port (
			clk:    in std_ulogic;
			rst:    in std_ulogic;

			tx:    out std_ulogic;
			tx_ok: out std_ulogic;
			tx_we:  in std_ulogic;
			tx_di:  in std_ulogic_vector(N - 1 downto 0);
		
			rx:     in std_ulogic;
			rx_ok: out std_ulogic;
			rx_nd: out std_ulogic;
			rx_re:  in std_ulogic;
			rx_do: out std_ulogic_vector(N - 1 downto 0);
		
			clock_reg:       in std_ulogic_vector(15 downto 0);
			clock_reg_tx_we: in std_ulogic;
			clock_reg_rx_we: in std_ulogic
		);
	end component;

	component uart_tb is
		generic(clock_frequency: positive := 100_000_000; delay: time := 0 ns);
	end component;
end package;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.uart_pkg.all;

entity uart_top is
	generic (clock_frequency: positive; asynchronous_reset: boolean; tx_init, rx_init: integer; N: positive := 8);
	port (
		clk:    in std_ulogic;
		rst:    in std_ulogic;

		tx:    out std_ulogic; -- physical UART TX signal
		tx_ok: out std_ulogic; -- not busy
		tx_we:  in std_ulogic; -- write data
		tx_di:  in std_ulogic_vector(N - 1 downto 0);
	
		rx:     in std_ulogic; -- physical UART RX signal
		rx_ok: out std_ulogic; -- data has no errors (parity or frame)
		rx_re:  in std_ulogic; -- read data
		rx_nd: out std_ulogic; -- new data available
		rx_do: out std_ulogic_vector(N - 1 downto 0);

		clock_reg:       in std_ulogic_vector(15 downto 0);
		clock_reg_tx_we: in std_ulogic;
		clock_reg_rx_we: in std_ulogic
	);
end entity;

architecture structural of uart_top is
	signal tx_sample, tx_baud: std_ulogic;
	signal rx_sample, rx_baud, rx_cr, rx_we: std_ulogic;
	signal rx_fail: std_ulogic_vector(1 downto 0);

	signal do, do_c, do_n: std_ulogic_vector(rx_do'range);
	signal nd_c, nd_n: std_ulogic; -- new data
begin 
	rx_ok <= not (rx_fail(0) or rx_fail(1));
	rx_do <= do;
	rx_nd <= nd_c;

	process (clk, rst) 
	begin
		if rst = '1' and asynchronous_reset then
			do_c <= (others => '0');
			nd_c <= '0';
		elsif rising_edge(clk) then
			if rst = '1' and not asynchronous_reset then
				do_c <= (others => '0');
				nd_c <= '0';
			else
				do_c <= do_n;
				nd_c <= nd_n;
			end if;
		end if;
	end process;

	process (do_c, do, nd_c, rx_we, rx_re) 
	begin
		do_n <= do_c;
		nd_n <= nd_c;
		if rx_we = '1' then
			do_n <= do;
			nd_n <= '1';
		elsif rx_re = '1' then
			nd_n <= '0';
		end if;
	end process;

	baud_tx: work.uart_pkg.uart_baud
		generic map(clock_frequency => clock_frequency, asynchronous_reset => true, init => tx_init, N => 16, D => 3)
		port map(
			clk    => clk,
			rst    => rst,
			we     => clock_reg_tx_we,
			cnt    => clock_reg,  -- 0x32/50 is 152000 @ 100MHz clk
			cr     => '0',
			sample => tx_sample,
			baud   => tx_baud);

	baud_rx: work.uart_pkg.uart_baud
		generic map(clock_frequency => clock_frequency, asynchronous_reset => true, init => rx_init, N => 16, D => 3)
		port map(
			clk    => clk,
			rst    => rst,
			we     => clock_reg_rx_we,
			cnt    => clock_reg,
			cr     => rx_cr,
			sample => rx_sample,
			baud   => rx_baud);

	tx_0: work.uart_pkg.uart_tx
		generic map(N => N, stops => 3, use_parity => false)
		port map(
			clk     => clk,
			rst     => rst, 

			baud    => tx_baud,
			ok      => tx_ok,
			we      => tx_we,
			di      => tx_di,
			tx      => tx);

	rx_0: work.uart_pkg.uart_rx
		generic map(N => N, D => 3, stops => 1, use_parity => false)
		port map(
			clk     => clk,
			rst     => rst, 

			baud    => rx_baud,
			sample  => rx_sample,
			cr      => rx_cr,
			failed  => rx_fail,
			we      => rx_we,
			do      => do,
			rx      => rx);
end architecture;

-- This module generates a sample pulse and a baud pulse. The sample rate
-- can be controlled by setting 'cnt'. This sample rate is then divided by 
-- 2^(D+1) to get the baud.
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.uart_pkg.all;

entity uart_baud is
	generic (clock_frequency: positive; asynchronous_reset: boolean; init: integer; N: positive := 16; D: positive := 3);
	port (
		clk:     in std_ulogic; 
		rst:     in std_ulogic; 
		
		we:      in std_ulogic;
		cnt:     in std_ulogic_vector(N - 1 downto 0);
		cr:      in std_ulogic := '0';

		sample: out std_ulogic;  -- sample pulse
		baud:   out std_ulogic); -- baud (sample rate / 2^(D+1))
end entity;

architecture behaviour of uart_baud is
	constant cmp_init: std_ulogic_vector := std_ulogic_vector(to_unsigned(init, cnt'length));
	signal cmp_c, cmp_n: std_ulogic_vector(cnt'range)  :=  cmp_init;
	signal cnt_c, cnt_n: std_ulogic_vector(cnt'range)  := (others => '0');
	signal div_c, div_n: std_ulogic_vector(D downto 0) := (others => '0');
	signal pul_c, pul_n: std_ulogic := '0';
	signal pulse: std_ulogic := '0';
begin
	pulse  <= (not cr) and pul_n and (pul_c xor pul_n); -- rising edge detector
	baud   <= (not cr) and div_n(div_n'high) and (div_c(div_c'high) xor div_n(div_n'high));
	sample <= pulse;

	process (clk, rst)
	begin
		if rst = '1' and asynchronous_reset then
			cmp_c <= cmp_init;
			cnt_c <= (others => '0');
			div_c <= (others => '0');
			pul_c <= '0';
		elsif rising_edge(clk) then
			if rst = '1' and not asynchronous_reset then
				cmp_c <= cmp_init;
				cnt_c <= (others => '0');
				div_c <= (others => '0');
				pul_c <= '0';
			else
				cmp_c <= cmp_n;
				cnt_c <= cnt_n;
				div_c <= div_n;
				pul_c <= pul_n;
			end if;
		end if;
	end process;

	process (pulse, div_c, cr, we)
	begin
		div_n <= div_c;
		if cr = '1' or we = '1' then
			div_n <= (others => '0');
			div_n(div_n'high) <= '1';
		elsif pulse = '1' then
			div_n <= std_ulogic_vector(unsigned(div_c) + 1);
		end if;
	end process;

	process (cmp_c, cnt_c, we, cnt, cr)
	begin
		cmp_n <= cmp_c;
		cnt_n <= cnt_c;

		if we = '1' then
			cmp_n <= cnt;
			cnt_n <= (others => '0');
			pul_n <= '0';
		elsif cr = '1' then
			cnt_n <= (others => '0');
			pul_n <= '0';
		elsif cnt_c = cmp_c then
			cnt_n <= (others => '0');
			pul_n <= '1';
		else
			cnt_n <= std_ulogic_vector(unsigned(cnt_c) + 1);
			pul_n <= '0';
		end if;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.uart_pkg.all;

entity uart_rx is
	generic (
		N:                  positive; 
		D:                  positive; 
		stops:              positive; 
		use_parity:         boolean; 
		asynchronous_reset: boolean := true; 
		even_parity:        boolean := true
	);
	port (
		clk:   in std_ulogic;
		rst:   in std_ulogic;
		cr:   out std_ulogic;        -- reset sample/baud clock when start bit detected
		baud, sample: in std_ulogic; -- pulses at baud and sample rate
		failed: out std_ulogic_vector(1 downto 0);
		rx:    in std_ulogic;        -- physical RX signal
		we:   out std_ulogic;        -- write 'do' to output register
		do:   out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture behaviour of uart_rx is
	type state is (reset, idle, start, data, parity, stop, done);
	signal state_c, state_n: state;
	signal do_c, do_n: std_ulogic_vector(do'range);
	signal rx_c, rx_n: std_ulogic_vector(do'range);
	signal sr_c, sr_n: std_ulogic_vector(D - 1 downto 0);
	signal rx_sync: std_ulogic;
	signal parity_c, parity_n: std_ulogic;
	signal count_c, count_n: integer range 0 to N - 1;
	signal majority: std_ulogic;
	signal fail_c, fail_n: std_ulogic_vector(1 downto 0);

	function parity(slv: std_ulogic_vector; even: boolean) return std_ulogic is
		variable z: std_ulogic := '0';
	begin
		if not even then
			z := '1';
		end if;
		for i in slv'range loop
			z := z xor slv(i);
		end loop;
		return z;
	end;
begin
	do     <= do_c;
	failed <= fail_c;

	-- majority <= sr_c(0);
	-- majority <= sr_c(0) and sr_c(1); -- even wins is 'sr_c(0) or sr_c(1)'
	-- NB. Majority forms in later half of cycle, as first bit is shifted off end, if
	-- majority function has one fewer inputs than shift register
	majority <= (sr_c(0) and sr_c(1)) or (sr_c(1) and sr_c(2)) or (sr_c(0) and sr_c(2));

	process (clk, rst)
		procedure reset is
		begin
			do_c     <= (others => '0');
			rx_c     <= (others => '0');
			sr_c     <= (others => '0');
			fail_c   <= (others => '0');
			state_c  <= reset;
			parity_c <= '0';
			count_c  <= 0;
			rx_sync  <= '0';
		end procedure;
	begin
		if rst = '1' and asynchronous_reset then
			reset;
		elsif rising_edge(clk) then
			if rst = '1' and not asynchronous_reset then
				reset;
			else
				do_c     <= do_n;
				rx_c     <= rx_n;
				sr_c     <= sr_n;
				state_c  <= state_n;
				parity_c <= parity_n;
				count_c  <= count_n;
				fail_c   <= fail_n;
				rx_sync  <= rx;
			end if;
		end if;
	end process;

	process (do_c, rx_c, sr_c, state_c, rx_sync, baud, sample, parity_c, count_c, fail_c, majority)
	begin
		fail_n  <= fail_c;
		do_n    <= do_c;
		rx_n    <= rx_c;
		sr_n    <= sr_c;
		state_n <= state_c;
		we      <= '0';
		cr      <= '0';
		count_n <= count_c;

		if sample = '1' then
			sr_n <= sr_c(sr_c'high - 1 downto sr_c'low) & rx_sync;
		end if;

		case state_c is
		when reset  =>
			do_n     <= (others => '0');
			rx_n     <= (others => '0');
			sr_n     <= (others => '0');
			fail_n   <= (others => '0');
			parity_n <= '0';
			state_n  <= idle;
		when idle   =>
			count_n <= 0;
			if rx_sync = '0' then
				state_n <= start;
				cr      <= '1';
				sr_n    <= (others => '0');
				fail_n  <= (others => '0');
			end if;
		when start  =>
			if baud = '1' then
				if majority /= '0' then
					state_n   <= done; -- frame error
					fail_n(0) <= '1';
				else
					state_n <= data;
				end if;
				sr_n <= (others => '0');
			end if;
		when data   =>
			rx_n(count_c) <= majority;
			if baud = '1' then
				if count_c = (N - 1) then
					count_n <= 0;
					if use_parity then
						state_n <= parity;
					else
						state_n <= stop;
					end if;
				else
					count_n <= count_c + 1;
				end if;
				sr_n <= (others => '0');
			end if;
		when parity =>
			parity_n <= majority;
			assert use_parity severity failure;
			if baud = '1' then
				if use_parity and (parity_c /= parity(rx_c, even_parity)) then
					fail_n(1) <= '1'; -- parity error, still process stop bits
				end if;
				state_n <= stop;
				sr_n <= (others => '0');
			end if;
		when stop   =>
			if baud = '1' then
				if majority /= '1' then
					state_n <= done; -- frame error
					fail_n(0) <= '1';
				elsif count_c = stops then
					count_n <= 0;
					state_n <= done;
				else
					count_n <= count_c + 1;
				end if;
			end if;
		when done   =>
			-- The consuming module needs to store fail_c as well.
			do_n <= rx_c;
			we <= '1';
			state_n <= idle;
		end case;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use work.uart_pkg.all;

entity uart_tx is
	generic (
		N: positive; 
		stops: positive; 
		use_parity: boolean; 
		asynchronous_reset: boolean := true; 
		even_parity: boolean := true
	);
	port (
		clk:   in std_ulogic;
		rst:   in std_ulogic;
		baud:  in std_ulogic; -- Pulse at baud
		tx:   out std_ulogic;
		ok:   out std_ulogic;
		we:    in std_ulogic; -- di write enable
		di:    in std_ulogic_vector(N - 1 downto 0));
end entity;

architecture behaviour of uart_tx is
	type state is (reset, idle, sync, start, data, parity, stop);
	signal state_c, state_n: state;
	signal di_c, di_n: std_ulogic_vector(di'range);
	signal busy: std_ulogic;
	signal parity_c, parity_n: std_ulogic;
	signal count_c, count_n: integer range 0 to N - 1;
	function parity(slv: std_ulogic_vector; even: boolean) return std_ulogic is
		variable z: std_ulogic := '0';
	begin
		if not even then
			z := '1';
		end if;
		for i in slv'range loop
			z := z xor slv(i);
		end loop;
		return z;
	end;
begin
	busy <= '0' when state_c = idle else '1';
	ok <= not busy;

	process (clk, rst)
		procedure reset is
		begin
			di_c     <= (others => '0');
			state_c  <= reset;
			parity_c <= '0';
			count_c  <= 0;
		end procedure;
	begin
		if rst = '1' and asynchronous_reset then
			reset;
		elsif rising_edge(clk) then
			if rst = '1' and not asynchronous_reset then
				reset;
			else
				di_c     <= di_n;
				state_c  <= state_n;
				parity_c <= parity_n;
				count_c  <= count_n;
			end if;
		end if;
	end process;

	process (di_c, di, state_c, we, baud, parity_c, count_c)
	begin
		count_n <= count_c;
		state_n <= state_c;
		di_n    <= di_c;
		tx <= '1';

		if use_parity then
			parity_n <= parity(di_c, even_parity);
		else
			parity_n <= '0';
		end if;

		case state_c is
		when reset  =>
			state_n  <= idle;
			count_n  <= 0;
			parity_n <= '0';
			di_n     <= (others => '0');
		when idle   =>
			count_n  <= 0;
			if we = '1' then
				di_n    <= di;
				state_n <= sync;
			end if;
		when sync   => -- wait until synced with baud clock
			if baud = '1' then
				state_n <= start;
			end if;
		when start  =>
			tx <= '0';
			if baud = '1' then
				state_n <= data;
			end if;
		when data   =>
			tx <= di_c(count_c);
			if baud = '1' then
				if count_c = (N - 1) then
					count_n <= 0;
					if use_parity then
						state_n <= parity;
					else
						state_n <= stop;
					end if;
				else
					count_n <= count_c + 1;
				end if;
			end if;
		when parity =>
			assert use_parity severity failure;
			tx <= parity_c;
			if baud = '1' then
				state_n <= stop;
			end if;
		when stop   =>
			tx <= '1';
			if baud = '1' then
				if count_c = stops then
					count_n <= 0;
					state_n <= idle;
				else
					count_n <= count_c + 1;
				end if;
			end if;
		end case;
	end process;
end architecture;

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.uart_pkg.all;

entity uart_tb is
	generic(clock_frequency: positive := 1_000_000; delay: time := 0 ns);
end entity;

architecture testing of uart_tb is
	constant clock_period:  time       := 1000 ms / clock_frequency;
	constant N:             positive   := 8;
	constant rx_init:       integer    := 50;
	constant tx_init:       integer    := 54;
	signal rst, clk:        std_ulogic := '1';
	signal stop:            boolean    := false;

	signal tx, rx:   std_ulogic;
	signal tx_ok, rx_ok:   std_ulogic;
	signal tx_we, rx_re:   std_ulogic := '0';
	signal rx_nd: std_ulogic;
	signal di, do:   std_ulogic_vector(N - 1 downto 0);
begin
	-- duration: process begin wait for 20000 us; stop <= true; wait; end process;
	clk_process: process
	begin
		rst <= '1';
		wait for clock_period * 5;
		rst <= '0';
		while not stop loop
			clk <= '1';
			wait for clock_period / 2;
			clk <= '0';
			wait for clock_period / 2;
		end loop;
		wait;
	end process;

	-- TODO: Assert what we receive is what we sent, also, introduce
	-- framing and parity errors and make sure we catch them.
	stimulus: process 
		procedure write(data: std_ulogic_vector(di'range)) is
		begin
			wait for clock_period;
			while tx_ok = '0' loop
				wait for clock_period;
			end loop;
			di <= data;
			tx_we <= '1';
			wait for clock_period;
			tx_we <= '0';
		end procedure;
	begin
		di <= x"00";
		wait until rst = '0';
		wait for clock_period;

		write(x"AA");
		write(x"BB");
		write(x"B2");
		write(x"00");
		write(x"10");
		wait for clock_period;
		while tx_ok = '0' loop
			wait for clock_period;
		end loop;

		stop <= true;
		wait;
	end process;

	ack: process
	begin
		while not stop loop
			if rx_nd = '1' then
				rx_re <= '1';
			else
				rx_re <= '0';
			end if;
			wait for clock_period;
		end loop;
		wait;
	end process;

	rx <= tx; -- loop back test

	uut: work.uart_pkg.uart_top
		generic map(clock_frequency => clock_frequency, asynchronous_reset => true, tx_init => tx_init, rx_init => rx_init, N => N)
		port map(
			clk           =>  clk,
			rst           =>  rst,
			tx            =>  tx,
			tx_ok         =>  tx_ok,
			tx_we         =>  tx_we,
			tx_di         =>  di,
			rx            =>  rx,
			rx_ok         =>  rx_ok,
			rx_nd         =>  rx_nd,
			rx_re         =>  rx_re,
			rx_do         =>  do,
			clock_reg     =>  (others => '0'),
			clock_reg_tx_we =>  '0',
			clock_reg_rx_we =>  '0'
		);
end architecture;


