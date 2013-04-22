-- Richard James Howe
-- J1 processor clone and extension. Moved bit 12 to bit 4 to
--  allow for more ALU instructions, added more ALU instructions.
-- @author         Richard James Howe.
-- @copyright      Copyright 2013 Richard James Howe.
-- @license        LGPL      
-- @email          howe.rj.89@googlemail.com
library ieee,work,std;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;

entity h2 is
    port(
        clk:        in  std_logic;
        rst:        in  std_logic;
        -- IO interface
        io_wr:      out std_logic;
        io_din:     in  std_logic_vector(15 downto 0);
        io_dout:    out std_logic_vector(15 downto 0);
        io_daddr:   out std_logic_vector(15 downto 0);
        -- RAM interface
        pco:        out std_logic_vector(12 downto 0);
        insn:       in  std_logic_vector(15 downto 0);

        dwe:        out std_logic; -- data write enable, read enable not need.
        din:        in  std_logic_vector(15 downto 0);
        dout:       out std_logic_vector(15 downto 0);
        daddr:      out std_logic_vector(12 downto 0)
    );
end;

architecture rtl of h2 is

    -- Program counter.
    signal pc_c      :   std_logic_vector(12 downto 0) := (others => '0');     
    signal pc_n      :   std_logic_vector(12 downto 0) := (others => '0');

    type   stk  is array (31 downto 0) of std_logic_vector(15 downto 0);
    -- Variable stack (RAM Template)
    signal vstkp_c   :   std_logic_vector(4 downto 0) := (others => '0');
    signal vstkp_n   :   std_logic_vector(4 downto 0) := (others => '0');
    signal vstk_ram    :   stk := (others => (others => '0'));
    -- Return stack (RAM Template)
    signal rstkp_c   :   std_logic_vector(4 downto 0) := (others => '0');
    signal rstkp_n   :   std_logic_vector(4 downto 0) := (others => '0');
    signal rstk_ram    :   stk := (others => (others => '0'));
    -- Stack deltas
    signal dd        :   std_logic_vector(4 downto 0) := (others => '0');
    signal rd        :   std_logic_vector(4 downto 0) := (others => '0');

    -- is_x signals, booleans, does the instruction have a certain property.
    signal is_alu    :   std_logic                    := '0';
    signal is_lit    :   std_logic                    := '0';
    signal is_jmp    :   std_logic                    := '0';
    signal is_cjmp   :   std_logic                    := '0';
    signal is_call   :   std_logic                    := '0';

    -- Top of stack, and next on stack.
    signal tos_c     :   std_logic_vector(15 downto 0):= (others => '0');
    signal tos_n     :   std_logic_vector(15 downto 0):= (others => '0');
    signal nos       :   std_logic_vector(15 downto 0):= (others => '0');
    -- Top of return stack.
    signal rtos_c    :   std_logic_vector(15 downto 0):= (others => '0');

    -- Multiplier register
--    signal mul_ans_c:  std_logic_vector(31 downto 0)   := (others => '0');
--    signal mul_ans_n:  std_logic_vector(31 downto 0)   := (others => '0');

    -- aluop is what is fed into the alu.
    signal aluop: std_logic_vector(4 downto 0)      := (others => '0');
    -- pc_plus_1, forces fewer adders.
    signal pc_plus_one: std_logic_vector(12 downto 0):= (others => '0');
    -- Stack signals
    signal dstkW:   std_logic                       := '0';
    signal rstkD:   std_logic_vector(15 downto 0)   := (others => '0');
    signal rstkW:   std_logic                       := '0';
begin
    -- is_x
    is_alu  <=  '1' when insn(15 downto 13) = "011" else '0';
    is_lit  <=  '1' when insn(15) = '1' else '0';
    is_jmp  <=  '1' when insn(15 downto 13) = "000" else '0';
    is_cjmp <=  '1' when insn(15 downto 13) = "001" else '0';
    is_call <=  '1' when insn(15 downto 13) = "010" else '0';

    -- Stack assignments
    nos       <=  vstk_ram(to_integer(unsigned(vstkp_c)));
    rtos_c    <=  rstk_ram(to_integer(unsigned(rstkp_c)));

    -- I/O assignments
    pco       <=  pc_n;
    dout      <=  nos;
    daddr     <=  tos_c(12 downto 0);
    dwe       <=  insn(5) when is_alu = '1' else '0';

    -- io_wr are handled in the ALU, 
    --  this makes things slower but we have
    --  run out of instruction bits to use.
    io_dout   <=  nos;
    io_daddr  <=  tos_c;

    -- misc
    pc_plus_one    <=  std_logic_vector(unsigned(pc_c) + 1);

    -- Signed addition!
    dd  <=  insn(1) & insn(1) & insn(1) & insn(1) & insn(0);
    rd  <=  insn(3) & insn(3) & insn(3) & insn(3) & insn(2);

    dstkW   <= '1' when is_lit = '1' or (is_alu = '1' and insn(7) = '1') else '0';

    stackWrite: process(
        clk
    )
    begin        
        if rising_edge(clk) then
            if dstkW = '1' then
                    vstk_ram(to_integer(unsigned(vstkp_n))) <=     tos_c;
            end if;

            if rstkW = '1' then
                    rstk_ram(to_integer(unsigned(rstkp_n))) <=     rstkD;
            end if;
        end if;
    end process;

    alu_sel: process(
        insn
    )
    begin
        case insn(14 downto 13) is
            when "00" => aluop <= "00000"; -- ubranch
            when "01" => aluop <= "00000"; -- call
            when "10" => aluop <= "00001"; -- 0branch
            when "11" => aluop <= insn(12 downto 8); -- alu operation.
            when others => aluop <= "XXXXX";
        end case;
    end process;

    -- multiplier, not in original core
--    alu_mul: process(mul_ans_c, aluop, tos_c, nos, is_alu)
--    begin
--        mul_ans_n   <=  mul_ans_c;
--        if is_alu = '1' and aluop = "10010" then
--            mul_ans_n   <= std_logic_vector(unsigned(tos_c)*unsigned(nos));
--        end if;
--    end process;

    -- ALU
    alu: process(
        is_lit, 
        tos_c, nos, rtos_c, 
        din, insn, aluop, 
        io_din,
        vstkp_c, rstkp_c 
        --,
        -- mul_ans_c
    )
    begin
        io_wr       <=  '0';
        tos_n       <=  tos_c;
        if is_lit = '1' then
            tos_n   <=  "0" & insn(14 downto 0);
        else 
            case aluop is -- ALU operation, 12 downto 8
                -- Original J1 instructions -- 
                when "00000" =>  tos_n   <=  tos_c;
                when "00001" =>  tos_n   <=  nos;
                when "00010" =>  
                    tos_n   <=  std_logic_vector(unsigned(tos_c)+unsigned(nos));
                when "00011" =>  tos_n   <=  tos_c and nos;
                when "00100" =>  tos_n   <=  tos_c or nos;
                when "00101" =>  tos_n   <=  tos_c xor nos;
                when "00110" =>  tos_n   <=  not tos_c;
                when "00111" =>  
                                if 
                                    nos = tos_c
                                then
                                    tos_n   <=  (0 => '1', others => '0');
                                else
                                    tos_n   <=  (others => '0');    
                                end if;
                when "01000" =>
                                if 
                                    signed(nos) < signed(tos_c)
                                then
                                    tos_n   <=  (0=>'1', others => '0');
                                else
                                    tos_n   <=  (others => '0');
                                end if;
                when "01001" =>  
                    tos_n   <=  std_logic_vector(unsigned(nos) srl to_integer(unsigned(tos_c(3 downto 0))));
                when "01010" =>  tos_n   <=  std_logic_vector(unsigned(tos_c)-1);
                when "01011" =>  tos_n   <=  rtos_c;
                when "01100" =>  tos_n   <=  din;
                when "01101" =>  
                    tos_n   <=  std_logic_vector(unsigned(nos) sll to_integer(unsigned(tos_c(3 downto 0))));
                when "01110" =>  tos_n   <=  vstkp_c & "000000" & rstkp_c; -- depth of stacks
                when "01111" =>
                                if 
                                    nos < tos_c
                                then
                                    tos_n <= (0=>'1', others => '0');
                                else
                                    tos_n <= (others => '0');
                                end if;
                -- Additional instructions ---
                  when "10000" => tos_n   <=  std_logic_vector(unsigned(tos_c)-unsigned(nos));
                  when "10001" => tos_n   <=  tos_c xnor nos;
--                  when "10010" => tos_n   <=  mul_ans_c(15 downto 0);
--                  when "10011" => tos_n   <=  mul_ans_c(31 downto 16);
                  when "10100" => tos_n   <=  io_din;
                  when "10101" => io_wr   <=  '1';
                  when "10110" => 
                    tos_n   <=  std_logic_vector(unsigned(nos) ror to_integer(unsigned(tos_c(3 downto 0))));
                  when "10111" =>
                    tos_n   <=  std_logic_vector(unsigned(nos) rol to_integer(unsigned(tos_c(3 downto 0))));
                  when "11000" => tos_n   <=  (others => '0');
                  when "11001" =>
                  when "11010" =>
                  when "11011" =>
                  when "11100" =>
                  when "11101" =>
                  when "11110" =>
                  when "11111" =>
                when others => tos_n    <=  (others => 'X');
            end case;
        end if;
    end process;

    -- Reset and state-machine clock.
    nextState: process(clk,rst)
    begin
        if rst='1' then
            vstkp_c     <=  (others => '0');
            rstkp_c     <=  (others => '0');
--            mul_ans_c   <=  (others => '0');
            pc_c        <=  (others => '0');
            tos_c       <=  (others => '0');
        elsif rising_edge(clk) then
            vstkp_c     <=  vstkp_n;
            rstkp_c     <=  rstkp_n;
--            mul_ans_c   <=  mul_ans_n;
            pc_c        <=  pc_n;
            tos_c       <=  tos_n;
        end if;
    end process;

    mainProcess: process(
            pc_c, 
            insn,
            vstkp_c, vstk_ram, dd,
            rstkp_c, rstk_ram, rd,
            tos_c,
            is_jmp, is_cjmp, is_call, is_lit, is_alu,
            pc_plus_one
        )
    begin
        vstkp_n   <=  vstkp_c;
        rstkp_n   <=  rstkp_c;

        -- main control

        if is_lit = '1' then
            vstkp_n <=  std_logic_vector(unsigned(vstkp_c)+1);
            rstkW   <=  '0';
            rstkD   <=  "000" & pc_plus_one; 
        elsif is_alu = '1' then 
            rstkW   <=  insn(6);
            rstkD   <=  tos_c;
            -- Signed addition.
            vstkp_n <=  std_logic_vector(unsigned(vstkp_c) + unsigned(dd));
            rstkp_n <=  std_logic_vector(unsigned(rstkp_c) + unsigned(rd));
        else
            if is_cjmp = '1' then
                vstkp_n <=  std_logic_vector(unsigned(vstkp_c) - 1);
            end if;

            if is_call = '1' then
                rstkp_n <=  std_logic_vector(unsigned(rstkp_c) + 1);
                rstkW   <=  '1';
                rstkD   <=  "000" & pc_plus_one; 
            else
                rstkW   <=  '0';
                rstkD   <=  "000" & pc_plus_one; 
            end if;
        end if;
    end process;

    pcUpdate: process(
        pc_c,insn,rtos_c,pc_plus_one,tos_c, 
        is_jmp,is_cjmp,is_call, is_alu
    )
    begin
        pc_n    <=  pc_c;
        if is_jmp = '1' or (is_cjmp = '1' and tos_c = X"0000") or is_call = '1' then
            pc_n    <=  insn(12 downto 0);
        elsif is_alu = '1' and insn(4) = '1' then
            pc_n    <=  rtos_c(12 downto 0);
        else
            pc_n    <=  pc_plus_one;
        end if;
    end process;

end architecture;
