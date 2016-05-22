/*****************************************************************************/
/* System header *************************************************************/
/*****************************************************************************/

/* When or'ed with a value, moves it into the I/O address range.*/
define("3 13 << const io_range")

/* System variables; Richard James Howe; LGPL */
#define vga_init_val   eval("122")
#define clock_init_val eval("1 15 << 1 13 << or 40 or")

#define o_vga_ctrl_reg eval("3 io_range or")

/* Outputs: 0x6000 - 0x7FFF */
#define o_NOTUSED      eval("0  io_range or")
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
#define o_8SegLED_0_1  eval("11 io_range or")
#define o_8SegLED_2_3  eval("12 io_range or")

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
#define isr_uart_ack   eval("2")
#define isr_uart_stb   eval("3")
#define isr_unused03   eval("4")
#define isr_unused04   eval("5")
#define isr_unused05   eval("6")
#define isr_unused06   eval("7")

/*#define eval(...)*/

/*****************************************************************************/
/* End                                                                       */
/*****************************************************************************/
