###############################################################################
# Richard James Howe                                                          #
# H2 CPU program                                                              #
###############################################################################

#### System includes ##########################################################

# include system varables
%include sys.inc

#### Variable Addr. ###########################################################

$vga_cursor "1024 "

###############################################################################

#### System Macros ############################################################



# setup routines go here
%macro setup
  # Setup Clock
#  clock_init_val
#  o_timerCtrl 
#  store
#  toggle_interrupts

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
  0
  load
%endmacro

%macro memload
  0
  load
#  swap
#  drop
%endmacro

###############################################################################



#### program entry point ######################################################
setup
begin:

 uartwait:
  i_uartStbDout
  load
  jumpc uartwait
  

  i_uartRead
  load
  dup # new

  o_vgaTxtDin
  store

  o_uartWrite #new
  store

  #new
  1
  o_uartStbWrite
  store
  #new

  call loadcursor
  o_vgaTxtAddr
  store

  # increment cursor variable
  call loadcursor
  1 add
  vga_cursor store

  # load in cursor, output to actual VGA cursor
  call loadcursor
  o_vgaCursor store

 
  0
  o_vgaWrite
  store

jump begin

###############################################################################

loadcursor:
  vga_cursor
  memload
  exit

#### interrupt service routines ###############################################
isr isr_clock
#  read_SWITCHES
#  write_LED
  exit
isr isr_unused01
  exit
isr isr_unused02
  exit
###############################################################################

###############################################################################
### EOF #######################################################################
###############################################################################

