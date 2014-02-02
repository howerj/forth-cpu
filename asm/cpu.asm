###############################################################################
# Richard James Howe                                                          #
# H2 CPU program                                                              #
###############################################################################

#### System variables #########################################################

$vga_init_val   "122"
$clock_init_val " 1 15 << 1 13 << or 40 or "

# When or'ed with a value, moves it into the
# I/O address range.
$io_range "3 13 <<"

$o_vga_ctrl_reg "3 $io_range or"

# Outputs
# 0x6000 - 0x7FFF
$o_7seg         "0  $io_range or"
$o_ledS         "1  $io_range or"
$o_vgaCursor    "2  $io_range or"
$o_vgaCtrl      "3  $io_range or"
$o_vgaTxtAddr   "4  $io_range or"
$o_vgaTxtDin    "5  $io_range or"
$o_vgaWrite     "6  $io_range or"
$o_uartWrite    "7  $io_range or"
$o_uartStbWrite "8  $io_range or"
$o_uartAckDout  "9  $io_range or"
$o_timerCtrl    "10 $io_range or"

# Inputs
# 0x6000 - 0x7FFF
$i_buttons      "0 $io_range or"
$i_switches     "1 $io_range or"
$i_vgaTxtDout   "2 $io_range or"
$i_uartRead     "3 $io_range or"
$i_uartAckWrite "4 $io_range or"
$i_uartStbDout  "5 $io_range or"

# Interrupt Service Requests
$isr_reset      "0 "
$isr_clock      "1 "
$isr_unused01   "2 "
$isr_unused02   "3 "
###############################################################################

#### System Macros ############################################################

# setup routines go here
%macro setup
  # Setup Clock
  clock_init_val
  o_timerCtrl 
  store
  toggle_interrupts

  # Setup VGA
  vga_init_val 
  o_vgaCtrl 
  store
%endmacro

%macro write_LED
  o_ledS
  store
%endmacro

%macro read_SWITCHES
  i_switches 
  load
%endmacro

###############################################################################

#### Variable Addr. ###########################################################

$vga_cursor "8000 "

###############################################################################


#### program entry point ######################################################
setup
begin:

  # wait for UART input
  uartwait:
  i_uartStbDout 
  load
  jumpc  uartwait

  # duplicate UART input, write back to UART also
  i_uartRead load dup
  o_uartWrite store
  o_vgaTxtDin store

  # increment vga cursor
  vga_cursor load
  1 add dup dup
  vga_cursor store
  o_vgaCursor store
  o_vgaTxtAddr store
  0 o_vgaWrite store

jump begin

###############################################################################

#### interrupt service routines ###############################################
isr isr_clock
  read_SWITCHES
  write_LED
  exit
isr isr_unused01
  exit
isr isr_unused02
  exit
###############################################################################

###############################################################################
### EOF #######################################################################
###############################################################################

