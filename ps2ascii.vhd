--------------------------------------------------------------------------------
--
--   FileName:         ps2ascii.vhd
--   Dependencies:     ps2kbd.vhd, debounce.vhd
--   Design Software:  Quartus II 32-bit Version 12.1 Build 177 SJ Full Version
--
--   HDL CODE is PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 11/29/2013 Scott Larson
--     Initial Public Release
--    
-- See https://eewiki.net/pages/viewpage.action?pageId=28279002
-- @note This file has been renamed and updated from the original.
-- @todo Reformat this file.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity ps2ascii is
	generic(
		clk_freq                  : integer := 50000000; --system clock frequency in hz
		ps2_debounce_counter_size : integer := 8);       --set such that 2^size/clk_freq = 5us (size = 8 for 50mhz)
	port(
		clk        : in  std_logic;                     --system clock input
		ps2_clk    : in  std_logic;                     --clock signal from PS2 keyboard
		ps2_data   : in  std_logic;                     --data signal from PS2 keyboard
		ascii_new  : out std_logic := '0';              --output flag indicating new ascii value
		ascii_code : out std_logic_vector(6 downto 0)); --ASCII value
end ps2ascii;

architecture behavior OF ps2ascii is
	type machine is(ready, new_code, translate, output);              --needed states
	signal state             : machine;                               --state machine
	signal ps2_code_new      : std_logic;                             --new PS2 code flag from ps2kbd component
	signal ps2_code          : std_logic_vector(7 downto 0);          --PS2 code input form ps2kbd component
	signal prev_ps2_code_new : std_logic := '1';                      --value of ps2_code_new flag on previous clock
	signal break             : std_logic := '0';                      --'1' for break code, '0' for make code
	signal e0_code           : std_logic := '0';                      --'1' for multi-code commands, '0' for single code commands
	signal caps_lock         : std_logic := '0';                      --'1' if caps lock is active, '0' if caps lock is inactive
	signal control_r         : std_logic := '0';                      --'1' if right control key is held down, else '0'
	signal control_l         : std_logic := '0';                      --'1' if left control key is held down, else '0'
	signal shift_r           : std_logic := '0';                      --'1' if right shift is held down, else '0'
	signal shift_l           : std_logic := '0';                      --'1' if left shift is held down, else '0'
	signal ascii             : std_logic_vector(7 downto 0) := x"FF"; --internal value of ASCII translation

	--declare PS2 keyboard interface component
	component ps2kbd is
	generic(
		clk_freq              : integer;  --system clock frequency in Hz
		debounce_counter_size : integer); --set such that 2^size/clk_freq = 5us (size = 8 for 50MHz)
	port(
		clk          : IN  std_logic;                     --system clock
		ps2_clk      : IN  std_logic;                     --clock signal from PS2 keyboard
		ps2_data     : IN  std_logic;                     --data signal from PS2 keyboard
		ps2_code_new : OUT std_logic;                     --flag that new PS/2 code is available on ps2_code bus
		ps2_code     : OUT std_logic_vector(7 downto 0)); --code received from PS/2
	end component;

begin

	--instantiate PS2 keyboard interface logic
	ps2kbd_0:  ps2kbd
	generic map(clk_freq => clk_freq, debounce_counter_size => ps2_debounce_counter_size)
	port map(clk => clk, ps2_clk => ps2_clk, ps2_data => ps2_data, ps2_code_new => ps2_code_new, ps2_code => ps2_code);

	process(clk)
	begin
	if(clk'event and clk = '1') then
	  prev_ps2_code_new <= ps2_code_new; --keep track of previous ps2_code_new values to determine low-to-high transitions
	  case state is
	  
	    --ready state: wait for a new PS2 code to be received
	    when ready =>
	      if(prev_ps2_code_new = '0' AND ps2_code_new = '1') then --new PS2 code received
	        ascii_new <= '0';                                       --reset new ASCII code indicator
	        state <= new_code;                                      --proceed to new_code state
	      else                                                    --no new PS2 code received yet
	        state <= ready;                                         --remain in ready state
	      end if;
	      
	    --new_code state: determine what to do with the new PS2 code  
	    when new_code =>
	      if(ps2_code = x"F0") then    --code indicates that next command is break
	        break <= '1';                --set break flag
	        state <= ready;              --return to ready state to await next PS2 code
	      elsif(ps2_code = x"E0") then --code indicates multi-key command
	        e0_code <= '1';              --set multi-code command flag
	        state <= ready;              --return to ready state to await next PS2 code
	      else                         --code is the last PS2 code in the make/break code
	        ascii(7) <= '1';             --set internal ascii value to unsupported code (for verification)
	        state <= translate;          --proceed to translate state
	      end if;

	    --translate state: translate PS2 code to ASCII value
	    when translate =>
	        break <= '0';    --reset break flag
	        e0_code <= '0';  --reset multi-code command flag
	        
	        --handle codes for control, shift, and caps lock
	        case ps2_code is
	          when x"58" =>                   --caps lock code
	            if(break = '0') then            --if make command
	              caps_lock <= NOT caps_lock;     --toggle caps lock
	            end if;
	          when x"14" =>                   --code for the control keys
	            if(e0_code = '1') then          --code for right control
	              control_r <= NOT break;         --update right control flag
	            else                            --code for left control
	              control_l <= NOT break;         --update left control flag
	            end if;
	          when x"12" =>                   --left shift code
	            shift_l <= NOT break;           --update left shift flag
	          when x"59" =>                   --right shift code
	            shift_r <= NOT break;           --update right shift flag
	          when others => null;
	        end case;
	    
	        --translate control codes (these do not depend on shift or caps lock)
	        if(control_l = '1' OR control_r = '1') then
	          case ps2_code is
	            when x"1E" => ascii <= x"00"; --^@  NUL
	            when x"1C" => ascii <= x"01"; --^A  SOH
	            when x"32" => ascii <= x"02"; --^B  STX
	            when x"21" => ascii <= x"03"; --^C  ETX
	            when x"23" => ascii <= x"04"; --^D  EOT
	            when x"24" => ascii <= x"05"; --^E  ENQ
	            when x"2B" => ascii <= x"06"; --^F  ACK
	            when x"34" => ascii <= x"07"; --^G  BEL
	            when x"33" => ascii <= x"08"; --^H  BS
	            when x"43" => ascii <= x"09"; --^I  HT
	            when x"3B" => ascii <= x"0A"; --^J  LF
	            when x"42" => ascii <= x"0B"; --^K  VT
	            when x"4B" => ascii <= x"0C"; --^L  FF
	            when x"3A" => ascii <= x"0D"; --^M  CR
	            when x"31" => ascii <= x"0E"; --^N  SO
	            when x"44" => ascii <= x"0F"; --^O  SI
	            when x"4D" => ascii <= x"10"; --^P  DLE
	            when x"15" => ascii <= x"11"; --^Q  DC1
	            when x"2D" => ascii <= x"12"; --^R  DC2
	            when x"1B" => ascii <= x"13"; --^S  DC3
	            when x"2C" => ascii <= x"14"; --^T  DC4
	            when x"3C" => ascii <= x"15"; --^U  NAK
	            when x"2A" => ascii <= x"16"; --^V  SYN
	            when x"1D" => ascii <= x"17"; --^W  ETB
	            when x"22" => ascii <= x"18"; --^X  CAN
	            when x"35" => ascii <= x"19"; --^Y  EM
	            when x"1A" => ascii <= x"1A"; --^Z  SUB
	            when x"54" => ascii <= x"1B"; --^[  ESC
	            when x"5D" => ascii <= x"1C"; --^\  FS
	            when x"5B" => ascii <= x"1D"; --^]  GS
	            when x"36" => ascii <= x"1E"; --^^  RS
	            when x"4E" => ascii <= x"1F"; --^_  US
	            when x"4A" => ascii <= x"7F"; --^?  DEL
	            when others => null;
	          end case;
	        else --if control keys are not pressed  
	        
	          --translate characters that do not depend on shift, or caps lock
	          case ps2_code is
	            when x"29" => ascii <= x"20"; --space
	            when x"66" => ascii <= x"08"; --backspace (BS control code)
	            when x"0D" => ascii <= x"09"; --tab (HT control code)
	            when x"5A" => ascii <= x"0D"; --enter (CR control code)
	            when x"76" => ascii <= x"1B"; --escape (ESC control code)
	            when x"71" => 
	              if(e0_code = '1') then      --ps2 code for delete is a multi-key code
	                ascii <= x"7F";             --delete
	              end if;
	            when others => null;
	          end case;
	          
	          --translate letters (these depend on both shift and caps lock)
	          if((shift_r = '0' AND shift_l = '0' AND caps_lock = '0') OR
	            ((shift_r = '1' OR shift_l = '1') AND caps_lock = '1')) then  --letter is lowercase
	            case ps2_code is              
	              when x"1C" => ascii <= x"61"; --a
	              when x"32" => ascii <= x"62"; --b
	              when x"21" => ascii <= x"63"; --c
	              when x"23" => ascii <= x"64"; --d
	              when x"24" => ascii <= x"65"; --e
	              when x"2B" => ascii <= x"66"; --f
	              when x"34" => ascii <= x"67"; --g
	              when x"33" => ascii <= x"68"; --h
	              when x"43" => ascii <= x"69"; --i
	              when x"3B" => ascii <= x"6A"; --j
	              when x"42" => ascii <= x"6B"; --k
	              when x"4B" => ascii <= x"6C"; --l
	              when x"3A" => ascii <= x"6D"; --m
	              when x"31" => ascii <= x"6E"; --n
	              when x"44" => ascii <= x"6F"; --o
	              when x"4D" => ascii <= x"70"; --p
	              when x"15" => ascii <= x"71"; --q
	              when x"2D" => ascii <= x"72"; --r
	              when x"1B" => ascii <= x"73"; --s
	              when x"2C" => ascii <= x"74"; --t
	              when x"3C" => ascii <= x"75"; --u
	              when x"2A" => ascii <= x"76"; --v
	              when x"1D" => ascii <= x"77"; --w
	              when x"22" => ascii <= x"78"; --x
	              when x"35" => ascii <= x"79"; --y
	              when x"1A" => ascii <= x"7A"; --z
	              when others => null;
	            end case;
	          else                                     --letter is uppercase
	            case ps2_code is            
	              when x"1C" => ascii <= x"41"; --A
	              when x"32" => ascii <= x"42"; --B
	              when x"21" => ascii <= x"43"; --C
	              when x"23" => ascii <= x"44"; --D
	              when x"24" => ascii <= x"45"; --E
	              when x"2B" => ascii <= x"46"; --F
	              when x"34" => ascii <= x"47"; --G
	              when x"33" => ascii <= x"48"; --H
	              when x"43" => ascii <= x"49"; --I
	              when x"3B" => ascii <= x"4A"; --J
	              when x"42" => ascii <= x"4B"; --K
	              when x"4B" => ascii <= x"4C"; --L
	              when x"3A" => ascii <= x"4D"; --M
	              when x"31" => ascii <= x"4E"; --N
	              when x"44" => ascii <= x"4F"; --O
	              when x"4D" => ascii <= x"50"; --P
	              when x"15" => ascii <= x"51"; --Q
	              when x"2D" => ascii <= x"52"; --R
	              when x"1B" => ascii <= x"53"; --S
	              when x"2C" => ascii <= x"54"; --T
	              when x"3C" => ascii <= x"55"; --U
	              when x"2A" => ascii <= x"56"; --V
	              when x"1D" => ascii <= x"57"; --W
	              when x"22" => ascii <= x"58"; --X
	              when x"35" => ascii <= x"59"; --Y
	              when x"1A" => ascii <= x"5A"; --Z
	              when others => null;
	            end case;
	          end if;
	          
	          --translate numbers and symbols (these depend on shift but not caps lock)
	          if(shift_l = '1' OR shift_r = '1') then  --key's secondary character is desired
	            case ps2_code is              
	              when x"16" => ascii <= x"21"; --!
	              when x"52" => ascii <= x"22"; --"
	              when x"26" => ascii <= x"23"; --#
	              when x"25" => ascii <= x"24"; --$
	              when x"2E" => ascii <= x"25"; --%
	              when x"3D" => ascii <= x"26"; --&              
	              when x"46" => ascii <= x"28"; --(
	              when x"45" => ascii <= x"29"; --)
	              when x"3E" => ascii <= x"2A"; --*
	              when x"55" => ascii <= x"2B"; --+
	              when x"4C" => ascii <= x"3A"; --:
	              when x"41" => ascii <= x"3C"; --<
	              when x"49" => ascii <= x"3E"; -->
	              when x"4A" => ascii <= x"3F"; --?
	              when x"1E" => ascii <= x"40"; --@
	              when x"36" => ascii <= x"5E"; --^
	              when x"4E" => ascii <= x"5F"; --_
	              when x"54" => ascii <= x"7B"; --{
	              when x"5D" => ascii <= x"7C"; --|
	              when x"5B" => ascii <= x"7D"; --}
	              when x"0E" => ascii <= x"7E"; --~
	              when others => null;
	            end case;
	          else --key's primary character is desired
	            case ps2_code is  
	              when x"45" => ascii <= x"30"; --0
	              when x"16" => ascii <= x"31"; --1
	              when x"1E" => ascii <= x"32"; --2
	              when x"26" => ascii <= x"33"; --3
	              when x"25" => ascii <= x"34"; --4
	              when x"2E" => ascii <= x"35"; --5
	              when x"36" => ascii <= x"36"; --6
	              when x"3D" => ascii <= x"37"; --7
	              when x"3E" => ascii <= x"38"; --8
	              when x"46" => ascii <= x"39"; --9
	              when x"52" => ascii <= x"27"; --'
	              when x"41" => ascii <= x"2C"; --,
	              when x"4E" => ascii <= x"2D"; ---
	              when x"49" => ascii <= x"2E"; --.
	              when x"4A" => ascii <= x"2F"; --/
	              when x"4C" => ascii <= x"3B"; --;
	              when x"55" => ascii <= x"3D"; --=
	              when x"54" => ascii <= x"5B"; --[
	              when x"5D" => ascii <= x"5C"; --\
	              when x"5B" => ascii <= x"5D"; --]
	              when x"0E" => ascii <= x"60"; --`
	              when others => null;
	            end case;
	          end if;
	          
	        end if;
	      
	      if(break = '0') then  --the code is a make
	        state <= output;      --proceed to output state
	      else                  --code is a break
	        state <= ready;       --return to ready state to await next PS2 code
	      end if;
	    
	    --output state: verify the code is valid and output the ASCII value
	    when output =>
	      if(ascii(7) = '0') then            --the PS2 code has an ASCII output
	        ascii_new <= '1';                  --set flag indicating new ASCII output
	        ascii_code <= ascii(6 downto 0);   --output the ASCII value
	      end if;
	      state <= ready;                    --return to ready state to await next PS2 code

	  end case;
	end if;
	end process;

end behavior;



