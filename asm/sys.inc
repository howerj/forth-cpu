/* System variables; Richard James Howe; LGPL */
#define vga_init_val   eval("vga_init_val = 122")
#define clock_init_val eval("clock_init_val = 1 15 << 1 13 << or 40 or")

/* When or'ed with a value, moves it into the I/O address range.*/
#define io_range eval("io_range = 3 13 <<")

#define o_vga_ctrl_reg eval("o_vga_ctrl_reg = 3 io_range or")

/* Outputs: 0x6000 - 0x7FFF */
#define o_7seg         eval("0  io_range or")
#define o_ledS         eval("1  io_range or")
#define o_vgaCursor    eval("2  io_range or")
#define o_vgaCtrl      eval("3  io_range or")
#define o_vgaTxtAddr   eval("4  io_range or")
#define o_vgaTxtDin    eval("5  io_range or")
#define o_vgaWrite     eval("6  io_range or")
#define o_uartWrite    eval("7  io_range or")
#define o_uartStbWrite eval("8  io_range or")
#define o_uartAckDout  eval("9  io_range or")
#define o_timerCtrl    eval("10 io_range or")

/** Inputs: 0x6000 - 0x7FFF*/
#define i_buttons      eval("0 io_range or")
#define i_switches     eval("1 io_range or")
#define i_vgaTxtDout   eval("2 io_range or")
#define i_uartRead     eval("3 io_range or")
#define i_uartAckWrite eval("4 io_range or")
#define i_uartStbDout  eval("5 io_range or")
#define i_ascii_new    eval("6 io_range or")
#define i_ascii_char   eval("7 io_range or")

/* Interrupt Service Requests*/
#define isr_reset      eval("0")
#define isr_clock      eval("1")
#define isr_unused01   eval("2")
#define isr_unused02   eval("3")

/*#define eval(...)*/
/* End */
