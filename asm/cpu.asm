###############################################################################
# Richard James Howe                                                          #
# H2 CPU program                                                              #
###############################################################################

#### System includes ##########################################################

# include system varables
%include sys.inc

#### System Macros ############################################################

# setup routines go here
%macro setup
  # Setup Clock
#  clock_init_val
#  o_timerCtrl 
#  store
  # toggle_interrupts

  # Setup VGA
  vga_init_val 
  o_vgaCtrl 
  store
%endmacro

# write to the LEDs
%macro write_LED
  o_ledS
  store
%endmacro

# read from the switches
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
  jumpc uartwait
  

  # duplicate UART input, write back to UART also
  i_uartRead 
  load 
#  i_ps2Read
#  or
#  dup
  1
  o_uartWrite
  store
  1
  o_uartStbWrite
  store
  1
  o_vgaTxtDin 
  store

  # increment vga cursor
#  vga_cursor 
#  load
#  1 
#  add 
#  dup 
#  dup
#  vga_cursor 
#  store
#  o_vgaCursor 
#  store
#  o_vgaTxtAddr 
#  store
#  1 
#  o_vgaWrite 
#  store

jump begin

###############################################################################

#### interrupt service routines ###############################################
isr isr_clock
  # read_SWITCHES
  # write_LED
  exit
isr isr_unused01
  exit
isr isr_unused02
  exit
###############################################################################

###############################################################################
### EOF #######################################################################
###############################################################################

