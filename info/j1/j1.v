module j1(
   input sys_clk_i, input sys_rst_i, input [15:0] io_din,
   output io_rd, output io_wr, output [15:0] io_addr, output [15:0] io_dout);

  wire [15:0] insn;
  wire [15:0] immediate = { 1'b0, insn[14:0] };

  wire [15:0] ramrd;

  reg [4:0] dsp;  // Data stack pointer
  reg [4:0] _dsp;
  reg [15:0] st0; // Return stack pointer
  reg [15:0] _st0;
  wire _dstkW;     // D stack write

  reg [12:0] pc;
  reg [12:0] _pc;
  reg [4:0] rsp;
  reg [4:0] _rsp;
  reg _rstkW;     // R stack write
  reg [15:0] _rstkD;
  wire _ramWE;     // RAM write enable

  wire [15:0] pc_plus_1;
  assign pc_plus_1 = pc + 1;

  // The D and R stacks
  reg [15:0] dstack[0:31];
  reg [15:0] rstack[0:31];
  always @(posedge sys_clk_i)
  begin
    if (_dstkW)
      dstack[_dsp] = st0;
    if (_rstkW)
      rstack[_rsp] = _rstkD;
  end
  wire [15:0] st1 = dstack[dsp];
  wire [15:0] rst0 = rstack[rsp];

  // st0sel is the ALU operation.  For branch and call the operation
  // is T, for 0branch it is N.  For ALU ops it is loaded from the instruction
  // field.
  reg [3:0] st0sel;
  always @*
  begin
    case (insn[14:13])
      2'b00: st0sel = 0;          // ubranch
      2'b10: st0sel = 0;          // call
      2'b01: st0sel = 1;          // 0branch
      2'b11: st0sel = insn[11:8]; // ALU
      default: st0sel = 4'bxxxx;
    endcase
  end

`define RAMS 3

  genvar i;

`define w (16 >> `RAMS)
`define w1 (`w - 1)

  generate 
    for (i = 0; i < (1 << `RAMS); i=i+1) begin : ram
      // RAMB16_S18_S18
      RAMB16_S2_S2
      ram(
        .DIA(0),
        // .DIPA(0),
        .DOA(insn[`w*i+`w1:`w*i]),
        .WEA(0),
        .ENA(1),
        .CLKA(sys_clk_i),
        .ADDRA({_pc}),

        .DIB(st1[`w*i+`w1:`w*i]),
        // .DIPB(2'b0),
        .WEB(_ramWE & (_st0[15:14] == 0)),
        .ENB(|_st0[15:14] == 0),
        .CLKB(sys_clk_i),
        .ADDRB(_st0[15:1]),
        .DOB(ramrd[`w*i+`w1:`w*i]));
    end
  endgenerate

  // Compute the new value of T.
  always @*
  begin
    if (insn[15])
      _st0 = immediate;
    else
      case (st0sel)
        4'b0000: _st0 = st0;
        4'b0001: _st0 = st1;
        4'b0010: _st0 = st0 + st1;
        4'b0011: _st0 = st0 & st1;
        4'b0100: _st0 = st0 | st1;
        4'b0101: _st0 = st0 ^ st1;
        4'b0110: _st0 = ~st0;
        4'b0111: _st0 = {16{(st1 == st0)}};
        4'b1000: _st0 = {16{($signed(st1) < $signed(st0))}};
        4'b1001: _st0 = st1 >> st0[3:0];
        4'b1010: _st0 = st0 - 1;
        4'b1011: _st0 = rst0;
        4'b1100: _st0 = |st0[15:14] ? io_din : ramrd;
        4'b1101: _st0 = st1 << st0[3:0];
        4'b1110: _st0 = {rsp, 3'b000, dsp};
        4'b1111: _st0 = {16{(st1 < st0)}};
        default: _st0 = 16'hxxxx;
      endcase
  end

  wire is_alu = (insn[15:13] == 3'b011);
  wire is_lit = (insn[15]);

  assign io_rd = (is_alu & (insn[11:8] == 4'hc));
  assign io_wr = _ramWE;
  assign io_addr = st0;
  assign io_dout = st1;

  assign _ramWE = is_alu & insn[5];
  assign _dstkW = is_lit | (is_alu & insn[7]);

  wire [1:0] dd = insn[1:0];  // D stack delta
  wire [1:0] rd = insn[3:2];  // R stack delta

  always @*
  begin
    if (is_lit) begin                       // literal
      _dsp = dsp + 1;
      _rsp = rsp;
      _rstkW = 0;
      _rstkD = _pc;
    end else if (is_alu) begin
      _dsp = dsp + {dd[1], dd[1], dd[1], dd};
      _rsp = rsp + {rd[1], rd[1], rd[1], rd};
      _rstkW = insn[6];
      _rstkD = st0;
    end else begin                          // jump/call
      // predicated jump is like DROP
      if (insn[15:13] == 3'b001) begin
        _dsp = dsp - 1;
      end else begin
        _dsp = dsp;
      end
      if (insn[15:13] == 3'b010) begin // call
        _rsp = rsp + 1;
        _rstkW = 1;
        _rstkD = {pc_plus_1[14:0], 1'b0};
      end else begin
        _rsp = rsp;
        _rstkW = 0;
        _rstkD = _pc;
      end
    end
  end

  always @*
  begin
    if (sys_rst_i)
      _pc = pc;
    else
      if ((insn[15:13] == 3'b000) |
          ((insn[15:13] == 3'b001) & (|st0 == 0)) |
          (insn[15:13] == 3'b010))
        _pc = insn[12:0];
      else if (is_alu & insn[12])
        _pc = rst0[15:1];
      else
        _pc = pc_plus_1;
  end

  always @(posedge sys_clk_i)
  begin
    if (sys_rst_i) begin
      pc <= 0;
      dsp <= 0;
      st0 <= 0;
      rsp <= 0;
    end else begin
      dsp <= _dsp;
      pc <= _pc;
      st0 <= _st0;
      rsp <= _rsp;
    end
  end

endmodule // j1
